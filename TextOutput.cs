using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Runtime.InteropServices;
using System.Threading;
using System.Windows.Forms;

namespace uclliu
{
    public enum TextOutputMode
    {
        UnicodeSendInput,
        WindowMessageChar,
        PasteShiftInsert,
        PasteCtrlV,
        PasteBig5,
        TsfBridge
    }

    public sealed class TextOutputContext
    {
        public TextOutputContext(string processName, string windowTitle, bool isWindows11)
        {
            ProcessName = processName ?? "";
            WindowTitle = windowTitle ?? "";
            IsWindows11 = isWindows11;
        }

        public string ProcessName { get; private set; }
        public string WindowTitle { get; private set; }
        public bool IsWindows11 { get; private set; }
    }

    public static class TextOutputCompatibilityDefaults
    {
        public static readonly string[] PasteShiftInsertApps = new string[] { "putty", "pietty", "pcman", "xyplorer", "kinza.exe", "iedit.exe", "rimworldwin64.exe", "windowsterminal.exe", "wt.exe", "mintty.exe" };
        public static readonly string[] PasteCtrlVApps = new string[] { "oxygennotincluded.exe", "iedit_.exe" };
        public static readonly string[] PasteBig5Apps = new string[] { "zip32w", "daqkingcon.exe", "EWinner.exe" };
        public static readonly string[] WindowMessageCharApps = new string[] { };
        public static readonly string[] NoUclApps = new string[] { "mstsc.exe", "cyberpunk2077.exe", "vncviewer.exe" };
    }

    public static class TextOutputRouter
    {
        public static TextOutputMode Select(string defaultOutputType, string processName, IEnumerable<string> shiftInsertApps, IEnumerable<string> ctrlVApps, IEnumerable<string> big5Apps)
        {
            return Select(defaultOutputType, new TextOutputContext(processName, "", false), shiftInsertApps, ctrlVApps, big5Apps);
        }

        public static TextOutputMode Select(string defaultOutputType, TextOutputContext context, IEnumerable<string> shiftInsertApps, IEnumerable<string> ctrlVApps, IEnumerable<string> big5Apps)
        {
            string outputType = (defaultOutputType ?? "DEFAULT").Trim().ToUpperInvariant();
            if (context == null)
            {
                context = new TextOutputContext("", "", false);
            }

            if (outputType == "BIG5")
            {
                return TextOutputMode.PasteBig5;
            }

            if (outputType == "PASTE")
            {
                return TextOutputMode.PasteShiftInsert;
            }

            if (outputType == "TSF")
            {
                return TextOutputMode.TsfBridge;
            }

            if (IsPttWindow(context.WindowTitle))
            {
                return TextOutputMode.PasteShiftInsert;
            }

            if (context.IsWindows11 && MatchesProcess(context.ProcessName, new string[] { "notepad" }))
            {
                return TextOutputMode.PasteCtrlV;
            }

            if (MatchesProcess(context.ProcessName, shiftInsertApps))
            {
                return TextOutputMode.PasteShiftInsert;
            }

            if (MatchesProcess(context.ProcessName, ctrlVApps))
            {
                return TextOutputMode.PasteCtrlV;
            }

            if (MatchesProcess(context.ProcessName, big5Apps))
            {
                return TextOutputMode.PasteBig5;
            }

            if (MatchesProcess(context.ProcessName, TextOutputCompatibilityDefaults.WindowMessageCharApps))
            {
                return TextOutputMode.WindowMessageChar;
            }

            return TextOutputMode.UnicodeSendInput;
        }

        public static bool MatchesProcess(string processName, IEnumerable<string> appPatterns)
        {
            if (string.IsNullOrWhiteSpace(processName) || appPatterns == null)
            {
                return false;
            }

            string normalizedProcess = NormalizeProcessName(processName);
            foreach (string appPattern in appPatterns)
            {
                if (string.IsNullOrWhiteSpace(appPattern))
                {
                    continue;
                }

                string normalizedPattern = NormalizeProcessName(appPattern);
                if (normalizedProcess == normalizedPattern || normalizedProcess.IndexOf(normalizedPattern, StringComparison.Ordinal) >= 0)
                {
                    return true;
                }
            }

            return false;
        }

        private static string NormalizeProcessName(string value)
        {
            value = (value ?? "").Trim().ToLowerInvariant();
            try
            {
                value = Path.GetFileName(value);
                value = Path.GetFileNameWithoutExtension(value);
            }
            catch
            {
            }
            return value;
        }

        private static bool IsPttWindow(string windowTitle)
        {
            if (string.IsNullOrWhiteSpace(windowTitle))
            {
                return false;
            }

            string title = windowTitle.Trim().ToLowerInvariant();
            return title.IndexOf("批踢踢實業坊", StringComparison.Ordinal) >= 0
                || title.IndexOf("term.ptt.cc", StringComparison.Ordinal) >= 0
                || title.IndexOf("ws.ptt.cc", StringComparison.Ordinal) >= 0
                || title.IndexOf("bbs", StringComparison.Ordinal) >= 0;
        }
    }

    internal sealed class StaActionDispatcher : IDisposable
    {
        private readonly object sync = new object();
        private readonly ManualResetEvent readySignal = new ManualResetEvent(false);
        private readonly Action<Exception> errorHandler;
        private readonly Thread worker;
        private Control invoker;
        private Exception startupError;
        private bool stopping;

        public StaActionDispatcher(Action<Exception> errorHandler)
        {
            this.errorHandler = errorHandler;
            worker = new Thread(run);
            worker.IsBackground = true;
            worker.Name = "UCLLIU text output";
            worker.SetApartmentState(ApartmentState.STA);
            worker.Start();

            if (!readySignal.WaitOne(2000))
            {
                throw new TimeoutException("STA text output worker did not start");
            }
            if (startupError != null)
            {
                throw new InvalidOperationException("STA text output worker failed to start", startupError);
            }
        }

        public void Post(Action action)
        {
            if (action == null)
            {
                throw new ArgumentNullException("action");
            }

            lock (sync)
            {
                if (stopping)
                {
                    return;
                }

                invoker.BeginInvoke((MethodInvoker)delegate
                {
                    try
                    {
                        action();
                    }
                    catch (Exception ex)
                    {
                        report_error(ex);
                    }
                });
            }
        }

        private void run()
        {
            Control control = null;
            try
            {
                control = new Control();
                IntPtr handle = control.Handle;
                lock (sync)
                {
                    invoker = control;
                }
                readySignal.Set();

                // SendKeys.SendWait 需要呼叫執行緒具備 Windows 訊息迴圈；
                // 用隱藏 Control 承接工作，避免剪貼簿輸出堵住主 UI 與鍵盤 hook。
                Application.Run();
            }
            catch (Exception ex)
            {
                startupError = ex;
                readySignal.Set();
                report_error(ex);
            }
            finally
            {
                if (control != null)
                {
                    control.Dispose();
                }
            }
        }

        private void report_error(Exception ex)
        {
            if (errorHandler != null)
            {
                try
                {
                    errorHandler(ex);
                }
                catch
                {
                    // 背景輸出錯誤不能讓工作執行緒停止。
                }
            }
        }

        public void Dispose()
        {
            Control control;
            lock (sync)
            {
                if (stopping)
                {
                    return;
                }
                stopping = true;
                control = invoker;
            }

            if (Thread.CurrentThread == worker)
            {
                Application.ExitThread();
                return;
            }

            if (control != null && !control.IsDisposed)
            {
                try
                {
                    control.BeginInvoke((MethodInvoker)delegate { Application.ExitThread(); });
                }
                catch (InvalidOperationException)
                {
                    // 訊息迴圈已經結束。
                }
            }

            worker.Join(500);
            readySignal.Close();
        }
    }

    public sealed class DeferredTextOutputDispatcher
    {
        private readonly Action<Action> post;

        public DeferredTextOutputDispatcher(Action<Action> post)
        {
            if (post == null)
            {
                throw new ArgumentNullException("post");
            }

            this.post = post;
        }

        public void Queue(string text, Action<string> sendOutput)
        {
            if (sendOutput == null)
            {
                throw new ArgumentNullException("sendOutput");
            }

            post(delegate
            {
                sendOutput(text);
            });
        }

        public void Queue(string text, Func<string, string> prepareOutput, Action<string> sendOutput)
        {
            if (prepareOutput == null)
            {
                throw new ArgumentNullException("prepareOutput");
            }
            if (sendOutput == null)
            {
                throw new ArgumentNullException("sendOutput");
            }

            string preparedText = prepareOutput(text);
            Queue(preparedText, sendOutput);
        }
    }

    public static class WindowsVersionDetector
    {
        public static bool IsWindows11OrLater()
        {
            Version version = GetWindowsVersion();
            return version.Major > 10 || (version.Major == 10 && version.Build >= 22000);
        }

        private static Version GetWindowsVersion()
        {
            OSVERSIONINFOEX versionInfo = new OSVERSIONINFOEX();
            versionInfo.dwOSVersionInfoSize = Marshal.SizeOf(typeof(OSVERSIONINFOEX));
            try
            {
                if (RtlGetVersion(ref versionInfo) == 0)
                {
                    return new Version((int)versionInfo.dwMajorVersion, (int)versionInfo.dwMinorVersion, (int)versionInfo.dwBuildNumber);
                }
            }
            catch
            {
            }

            return Environment.OSVersion.Version;
        }

        [DllImport("ntdll.dll")]
        private static extern int RtlGetVersion(ref OSVERSIONINFOEX versionInfo);

        [StructLayout(LayoutKind.Sequential, CharSet = CharSet.Unicode)]
        private struct OSVERSIONINFOEX
        {
            public int dwOSVersionInfoSize;
            public uint dwMajorVersion;
            public uint dwMinorVersion;
            public uint dwBuildNumber;
            public uint dwPlatformId;
            [MarshalAs(UnmanagedType.ByValTStr, SizeConst = 128)]
            public string szCSDVersion;
            public ushort wServicePackMajor;
            public ushort wServicePackMinor;
            public ushort wSuiteMask;
            public byte wProductType;
            public byte wReserved;
        }
    }

    public sealed class UnicodeSendInputOutput
    {
        public const int InputKeyboard = 1;
        public const uint KeyEventKeyUp = 0x0002;
        public const uint KeyEventUnicode = 0x0004;
        public static readonly IntPtr UclExtraInfo = new IntPtr(0x55434C49);

        private readonly IUnicodeInputSender inputSender;

        public UnicodeSendInputOutput()
            : this(new Win32UnicodeInputSender())
        {
        }

        internal UnicodeSendInputOutput(IUnicodeInputSender inputSender)
        {
            if (inputSender == null)
            {
                throw new ArgumentNullException("inputSender");
            }

            this.inputSender = inputSender;
        }

        public bool TrySendText(string text, out string error)
        {
            error = null;
            if (text == null)
            {
                throw new ArgumentNullException("text");
            }
            if (text.Length == 0)
            {
                return true;
            }

            for (int i = 0; i < text.Length; i++)
            {
                INPUT[] inputs = BuildInputsForText(text[i].ToString());
                uint sent = inputSender.Send(inputs);
                if (sent == inputs.Length)
                {
                    continue;
                }

                int lastError = inputSender.GetLastError();
                error = "SendInput inserted " + sent + "/" + inputs.Length + " events at char " + i;
                if (lastError != 0)
                {
                    error += ": " + new Win32Exception(lastError).Message;
                }
                return false;
            }

            return true;
        }

        public static INPUT[] BuildInputsForText(string text)
        {
            if (text == null)
            {
                throw new ArgumentNullException("text");
            }

            INPUT[] inputs = new INPUT[text.Length * 2];
            int outputIndex = 0;
            for (int i = 0; i < text.Length; i++)
            {
                inputs[outputIndex++] = CreateKeyboardInput(text[i], false);
                inputs[outputIndex++] = CreateKeyboardInput(text[i], true);
            }
            return inputs;
        }

        private static INPUT CreateKeyboardInput(char textChar, bool keyUp)
        {
            INPUT input = new INPUT();
            input.type = InputKeyboard;
            input.u.ki.wVk = 0;
            input.u.ki.wScan = textChar;
            input.u.ki.dwFlags = KeyEventUnicode | (keyUp ? KeyEventKeyUp : 0);
            input.u.ki.time = 0;
            input.u.ki.dwExtraInfo = UclExtraInfo;
            return input;
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct INPUT
        {
            public int type;
            public InputUnion u;
        }

        [StructLayout(LayoutKind.Explicit)]
        public struct InputUnion
        {
            [FieldOffset(0)]
            public MOUSEINPUT mi;

            [FieldOffset(0)]
            public KEYBDINPUT ki;

            [FieldOffset(0)]
            public HARDWAREINPUT hi;
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct KEYBDINPUT
        {
            public ushort wVk;
            public ushort wScan;
            public uint dwFlags;
            public uint time;
            public IntPtr dwExtraInfo;
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct MOUSEINPUT
        {
            public int dx;
            public int dy;
            public int mouseData;
            public uint dwFlags;
            public uint time;
            public IntPtr dwExtraInfo;
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct HARDWAREINPUT
        {
            public uint uMsg;
            public ushort wParamL;
            public ushort wParamH;
        }
    }

    internal interface IUnicodeInputSender
    {
        uint Send(UnicodeSendInputOutput.INPUT[] inputs);
        int GetLastError();
    }

    internal sealed class Win32UnicodeInputSender : IUnicodeInputSender
    {
        [DllImport("user32.dll", SetLastError = true)]
        private static extern uint SendInput(uint nInputs, UnicodeSendInputOutput.INPUT[] pInputs, int cbSize);

        public uint Send(UnicodeSendInputOutput.INPUT[] inputs)
        {
            return SendInput((uint)inputs.Length, inputs, Marshal.SizeOf(typeof(UnicodeSendInputOutput.INPUT)));
        }

        public int GetLastError()
        {
            return Marshal.GetLastWin32Error();
        }
    }

    public sealed class WindowMessageCharOutput
    {
        private readonly IFocusedTextWindowGateway gateway;

        public WindowMessageCharOutput()
            : this(new Win32FocusedTextWindowGateway())
        {
        }

        internal WindowMessageCharOutput(IFocusedTextWindowGateway gateway)
        {
            if (gateway == null)
            {
                throw new ArgumentNullException("gateway");
            }

            this.gateway = gateway;
        }

        public bool TrySendText(string text, out string error)
        {
            if (text == null)
            {
                throw new ArgumentNullException("text");
            }

            error = null;
            if (text.Length == 0)
            {
                return true;
            }

            IntPtr focusedWindow = gateway.GetFocusedWindow();
            if (focusedWindow == IntPtr.Zero)
            {
                error = "focused window unavailable";
                return false;
            }

            for (int i = 0; i < text.Length; i++)
            {
                if (!gateway.PostChar(focusedWindow, text[i]))
                {
                    error = "PostMessage WM_CHAR failed at char " + i;
                    return false;
                }
            }

            return true;
        }
    }

    internal enum ClipboardTextKind
    {
        Unicode,
        Ansi
    }

    internal interface IFocusedTextWindowGateway
    {
        IntPtr GetFocusedWindow();
        bool PostChar(IntPtr windowHandle, char textChar);
    }

    internal interface IClipboardGateway
    {
        object GetDataObject();
        bool ContainsText();
        string GetText();
        void SetText(string text, ClipboardTextKind textKind);
        void Clear();
        void SetDataObject(object dataObject);
    }

    internal interface IKeySender
    {
        void SendWait(string keys);
    }

    internal sealed class SendKeysKeySender : IKeySender
    {
        public void SendWait(string keys)
        {
            SendKeys.SendWait(keys);
        }
    }

    internal interface ISelectedTextTransformCommand
    {
        bool TryRun(Func<string, string> transform, Action<string> sendOutput, out string error);
    }

    internal sealed class WinFormsClipboardGateway : IClipboardGateway
    {
        public object GetDataObject()
        {
            return Clipboard.GetDataObject();
        }

        public bool ContainsText()
        {
            return Clipboard.ContainsText(TextDataFormat.UnicodeText);
        }

        public string GetText()
        {
            return Clipboard.GetText(TextDataFormat.UnicodeText);
        }

        public void SetText(string text, ClipboardTextKind textKind)
        {
            TextDataFormat format = textKind == ClipboardTextKind.Ansi ? TextDataFormat.Text : TextDataFormat.UnicodeText;
            Clipboard.SetText(text ?? string.Empty, format);
        }

        public void Clear()
        {
            Clipboard.Clear();
        }

        public void SetDataObject(object dataObject)
        {
            Clipboard.SetDataObject(dataObject, true);
        }
    }

    internal sealed class Win32FocusedTextWindowGateway : IFocusedTextWindowGateway
    {
        private const uint WmChar = 0x0102;

        public IntPtr GetFocusedWindow()
        {
            IntPtr foregroundWindow = GetForegroundWindow();
            if (foregroundWindow == IntPtr.Zero)
            {
                return IntPtr.Zero;
            }

            uint processId;
            uint threadId = GetWindowThreadProcessId(foregroundWindow, out processId);
            GuiThreadInfo info = new GuiThreadInfo();
            info.cbSize = Marshal.SizeOf(typeof(GuiThreadInfo));
            if (threadId != 0 && GetGUIThreadInfo(threadId, ref info) && info.hwndFocus != IntPtr.Zero)
            {
                return info.hwndFocus;
            }

            return foregroundWindow;
        }

        public bool PostChar(IntPtr windowHandle, char textChar)
        {
            return PostMessage(windowHandle, WmChar, new IntPtr((int)textChar), IntPtr.Zero);
        }

        [DllImport("user32.dll")]
        private static extern IntPtr GetForegroundWindow();

        [DllImport("user32.dll")]
        private static extern uint GetWindowThreadProcessId(IntPtr hWnd, out uint lpdwProcessId);

        [DllImport("user32.dll", SetLastError = true)]
        private static extern bool GetGUIThreadInfo(uint idThread, ref GuiThreadInfo lpgui);

        [DllImport("user32.dll", SetLastError = true, CharSet = CharSet.Unicode)]
        private static extern bool PostMessage(IntPtr hWnd, uint msg, IntPtr wParam, IntPtr lParam);

        [StructLayout(LayoutKind.Sequential)]
        private struct GuiThreadInfo
        {
            public int cbSize;
            public int flags;
            public IntPtr hwndActive;
            public IntPtr hwndFocus;
            public IntPtr hwndCapture;
            public IntPtr hwndMenuOwner;
            public IntPtr hwndMoveSize;
            public IntPtr hwndCaret;
            public Rect rcCaret;
        }

        [StructLayout(LayoutKind.Sequential)]
        private struct Rect
        {
            public int Left;
            public int Top;
            public int Right;
            public int Bottom;
        }
    }

    internal sealed class SelectedTextTransformDispatcher
    {
        private readonly Action<Action> post;
        private readonly Action<bool> setSending;
        private readonly Action<string> log;

        public SelectedTextTransformDispatcher(Action<Action> post, Action<bool> setSending, Action<string> log)
        {
            if (post == null)
            {
                throw new ArgumentNullException("post");
            }
            if (setSending == null)
            {
                throw new ArgumentNullException("setSending");
            }
            if (log == null)
            {
                throw new ArgumentNullException("log");
            }

            this.post = post;
            this.setSending = setSending;
            this.log = log;
        }

        public void Queue(ISelectedTextTransformCommand command, string commandName, Func<string, string> transform, Action<string> sendOutput)
        {
            if (command == null)
            {
                throw new ArgumentNullException("command");
            }
            if (transform == null)
            {
                throw new ArgumentNullException("transform");
            }
            if (sendOutput == null)
            {
                throw new ArgumentNullException("sendOutput");
            }

            post(delegate
            {
                string error;
                setSending(true);
                try
                {
                    if (!command.TryRun(transform, sendOutput, out error))
                    {
                        log("可能會當 " + commandName + ": " + error);
                    }
                }
                finally
                {
                    setSending(false);
                }
            });
        }
    }

    public sealed class SelectedTextTransformCommand : ISelectedTextTransformCommand
    {
        private readonly IClipboardGateway clipboard;
        private readonly IKeySender keySender;
        private readonly Action<int> sleep;

        public int RetryCount = 8;
        public int RetryDelayMs = 25;

        public SelectedTextTransformCommand()
            : this(new WinFormsClipboardGateway(), new SendKeysKeySender(), Thread.Sleep)
        {
        }

        internal SelectedTextTransformCommand(IClipboardGateway clipboard, IKeySender keySender, Action<int> sleep)
        {
            if (clipboard == null)
            {
                throw new ArgumentNullException("clipboard");
            }
            if (keySender == null)
            {
                throw new ArgumentNullException("keySender");
            }
            if (sleep == null)
            {
                throw new ArgumentNullException("sleep");
            }

            this.clipboard = clipboard;
            this.keySender = keySender;
            this.sleep = sleep;
        }

        public bool TryRun(Func<string, string> transform, Action<string> sendOutput, out string error)
        {
            if (transform == null)
            {
                throw new ArgumentNullException("transform");
            }
            if (sendOutput == null)
            {
                throw new ArgumentNullException("sendOutput");
            }

            ClipboardBackup backup;
            if (!TryCaptureBackup(out backup, out error))
            {
                return false;
            }

            bool success = false;
            string operationError = null;
            try
            {
                string selectedText;
                if (!TryReadSelectedText(out selectedText, out operationError))
                {
                    success = false;
                }
                else
                {
                    string output = transform(selectedText) ?? "";
                    sendOutput(output);
                    success = true;
                }
            }
            catch (Exception ex)
            {
                operationError = "selected text transform failed: " + ex.Message;
                success = false;
            }
            finally
            {
                string restoreError;
                if (!TryRestoreBackup(backup, out restoreError) && operationError == null)
                {
                    operationError = restoreError;
                    success = false;
                }
            }

            error = operationError;
            return success && error == null;
        }

        private bool TryReadSelectedText(out string selectedText, out string error)
        {
            selectedText = null;
            string actionError;
            if (!TryClipboardAction(delegate { clipboard.Clear(); }, "clear clipboard failed", out actionError))
            {
                error = actionError;
                return false;
            }

            try
            {
                keySender.SendWait("^{c}");
            }
            catch (Exception ex)
            {
                error = "send Ctrl+C failed: " + ex.Message;
                return false;
            }

            int attempts = Math.Max(1, RetryCount);
            for (int i = 0; i < attempts; i++)
            {
                string copiedText = null;
                string readError;
                bool ok = TryClipboardAction(
                    delegate
                    {
                        if (clipboard.ContainsText())
                        {
                            copiedText = clipboard.GetText();
                        }
                    },
                    "read copied text failed",
                    out readError);

                if (!ok)
                {
                    error = readError;
                    return false;
                }

                if (copiedText != null)
                {
                    selectedText = copiedText;
                    error = null;
                    return true;
                }

                if (i + 1 < attempts && RetryDelayMs > 0)
                {
                    sleep(RetryDelayMs);
                }
            }

            error = "copy selected text failed: clipboard has no unicode text";
            return false;
        }

        private bool TryCaptureBackup(out ClipboardBackup backup, out string error)
        {
            if (clipboard == null)
            {
                backup = new ClipboardBackup();
                error = null;
                return true;
            }

            ClipboardBackup captured = new ClipboardBackup();
            bool ok = TryClipboardAction(
                delegate
                {
                    captured.DataObject = clipboard.GetDataObject();
                    captured.HasText = clipboard.ContainsText();
                    captured.Text = captured.HasText ? clipboard.GetText() : null;
                },
                "capture clipboard failed",
                out error);

            backup = captured;
            return ok;
        }

        private bool TryRestoreBackup(ClipboardBackup backup, out string error)
        {
            if (clipboard == null)
            {
                error = null;
                return true;
            }

            return TryClipboardAction(
                delegate
                {
                    if (backup.DataObject != null)
                    {
                        clipboard.SetDataObject(backup.DataObject);
                    }
                    else if (backup.HasText)
                    {
                        clipboard.SetText(backup.Text, ClipboardTextKind.Unicode);
                    }
                    else
                    {
                        clipboard.Clear();
                    }
                },
                "restore clipboard failed",
                out error);
        }

        private bool TryClipboardAction(Action action, string errorPrefix, out string error)
        {
            return ClipboardActionRunner.Try(action, errorPrefix, RetryCount, RetryDelayMs, sleep, out error);
        }

        private struct ClipboardBackup
        {
            public object DataObject;
            public bool HasText;
            public string Text;
        }
    }

    internal static class ClipboardActionRunner
    {
        public static bool Try(Action action, string errorPrefix, int retryCount, int retryDelayMs, Action<int> sleep, out string error)
        {
            Exception lastException = null;
            int attempts = Math.Max(1, retryCount);
            for (int i = 0; i < attempts; i++)
            {
                try
                {
                    action();
                    error = null;
                    return true;
                }
                catch (Exception ex)
                {
                    lastException = ex;
                    if (i + 1 < attempts && retryDelayMs > 0)
                    {
                        sleep(retryDelayMs);
                    }
                }
            }

            error = errorPrefix;
            if (lastException != null)
            {
                error += ": " + lastException.Message;
            }
            return false;
        }
    }

    internal sealed class ClipboardPasteStageSample
    {
        public ClipboardPasteStageSample(string stage, int elapsedMilliseconds, bool succeeded)
        {
            Stage = stage;
            ElapsedMilliseconds = elapsedMilliseconds;
            Succeeded = succeeded;
        }

        public string Stage { get; private set; }
        public int ElapsedMilliseconds { get; private set; }
        public bool Succeeded { get; private set; }
    }

    public sealed class ClipboardPasteOutput
    {
        private readonly IClipboardGateway clipboard;
        private readonly IKeySender keySender;
        private readonly Action<int> sleep;

        public int RetryCount = 5;
        public int RetryDelayMs = 15;
        public int RestoreDelayMs = 45;

        public ClipboardPasteOutput()
            : this(new WinFormsClipboardGateway(), new SendKeysKeySender(), Thread.Sleep)
        {
        }

        internal ClipboardPasteOutput(IClipboardGateway clipboard, IKeySender keySender, Action<int> sleep)
        {
            if (clipboard == null)
            {
                throw new ArgumentNullException("clipboard");
            }
            if (keySender == null)
            {
                throw new ArgumentNullException("keySender");
            }
            if (sleep == null)
            {
                throw new ArgumentNullException("sleep");
            }

            this.clipboard = clipboard;
            this.keySender = keySender;
            this.sleep = sleep;
        }

        public bool TryPasteText(string text, string sendKeys, out string error)
        {
            return TryPasteText(text, ClipboardTextKind.Unicode, sendKeys, null, out error);
        }

        internal bool TryPasteText(string text, string sendKeys, Action<ClipboardPasteStageSample> stageObserver, out string error)
        {
            return TryPasteText(text, ClipboardTextKind.Unicode, sendKeys, stageObserver, out error);
        }

        public bool TryPasteAnsiText(string text, string sendKeys, out string error)
        {
            return TryPasteText(text, ClipboardTextKind.Ansi, sendKeys, null, out error);
        }

        internal bool TryPasteAnsiText(string text, string sendKeys, Action<ClipboardPasteStageSample> stageObserver, out string error)
        {
            return TryPasteText(text, ClipboardTextKind.Ansi, sendKeys, stageObserver, out error);
        }

        private bool TryPasteText(string text, ClipboardTextKind textKind, string sendKeys, Action<ClipboardPasteStageSample> stageObserver, out string error)
        {
            error = null;

            ClipboardBackup backup;
            long stageStarted = start_stage(stageObserver);
            bool captured = TryCaptureBackup(out backup, out error);
            report_stage(stageObserver, "capture", stageStarted, captured);
            if (!captured)
            {
                return false;
            }

            bool success = false;
            bool shouldRestore = false;
            string operationError = null;

            try
            {
                stageStarted = start_stage(stageObserver);
                bool textSet = TryClipboardAction(delegate { clipboard.SetText(text, textKind); }, "set clipboard failed", out operationError);
                report_stage(stageObserver, "set", stageStarted, textSet);
                if (!textSet)
                {
                    success = false;
                }
                else
                {
                    shouldRestore = true;
                    bool sendReported = false;
                    try
                    {
                        stageStarted = start_stage(stageObserver);
                        keySender.SendWait(sendKeys);
                        report_stage(stageObserver, "send", stageStarted, true);
                        sendReported = true;
                        if (RestoreDelayMs > 0)
                        {
                            stageStarted = start_stage(stageObserver);
                            sleep(RestoreDelayMs);
                            report_stage(stageObserver, "wait", stageStarted, true);
                        }
                        success = true;
                    }
                    catch (Exception ex)
                    {
                        if (!sendReported)
                        {
                            report_stage(stageObserver, "send", stageStarted, false);
                        }
                        else if (RestoreDelayMs > 0)
                        {
                            report_stage(stageObserver, "wait", stageStarted, false);
                        }
                        operationError = "send keys failed: " + ex.Message;
                    }
                }
            }
            finally
            {
                if (shouldRestore)
                {
                    string restoreError;
                    stageStarted = start_stage(stageObserver);
                    bool restored = TryRestoreBackup(backup, out restoreError);
                    report_stage(stageObserver, "restore", stageStarted, restored);
                    if (!restored && operationError == null)
                    {
                        operationError = restoreError;
                        success = false;
                    }
                }
            }

            error = operationError;
            return success && error == null;
        }

        private static long start_stage(Action<ClipboardPasteStageSample> stageObserver)
        {
            return stageObserver == null ? 0 : Stopwatch.GetTimestamp();
        }

        private static void report_stage(Action<ClipboardPasteStageSample> stageObserver, string stage, long startedTicks, bool succeeded)
        {
            if (stageObserver == null)
            {
                return;
            }

            int elapsedMilliseconds = (int)(((Stopwatch.GetTimestamp() - startedTicks) * 1000.0) / Stopwatch.Frequency);
            try
            {
                stageObserver(new ClipboardPasteStageSample(stage, elapsedMilliseconds, succeeded));
            }
            catch
            {
                // 診斷回呼不能改變剪貼簿送字的成功或失敗結果。
            }
        }

        private bool TryCaptureBackup(out ClipboardBackup backup, out string error)
        {
            ClipboardBackup captured = new ClipboardBackup();
            bool ok = TryClipboardAction(
                delegate
                {
                    captured.HasText = clipboard.ContainsText();
                    if (captured.HasText)
                    {
                        // 文字用值複製保存；原始 IDataObject 可能仍依附舊剪貼簿擁有者，
                        // 覆蓋剪貼簿後再重用會讓 PTT 的背景送字卡在備份或還原。
                        captured.Text = clipboard.GetText();
                    }
                    else
                    {
                        captured.DataObject = clipboard.GetDataObject();
                    }
                },
                "capture clipboard failed",
                out error);

            backup = captured;
            return ok;
        }

        private bool TryRestoreBackup(ClipboardBackup backup, out string error)
        {
            return TryClipboardAction(
                delegate
                {
                    if (backup.HasText)
                    {
                        clipboard.SetText(backup.Text, ClipboardTextKind.Unicode);
                    }
                    else if (backup.DataObject != null)
                    {
                        clipboard.SetDataObject(backup.DataObject);
                    }
                    else
                    {
                        clipboard.Clear();
                    }
                },
                "restore clipboard failed",
                out error);
        }

        private bool TryClipboardAction(Action action, string errorPrefix, out string error)
        {
            Exception lastException = null;
            int attempts = Math.Max(1, RetryCount);
            for (int i = 0; i < attempts; i++)
            {
                try
                {
                    action();
                    error = null;
                    return true;
                }
                catch (Exception ex)
                {
                    lastException = ex;
                    if (i + 1 < attempts && RetryDelayMs > 0)
                    {
                        sleep(RetryDelayMs);
                    }
                }
            }

            error = errorPrefix;
            if (lastException != null)
            {
                error += ": " + lastException.Message;
            }
            return false;
        }

        private struct ClipboardBackup
        {
            public object DataObject;
            public bool HasText;
            public string Text;
        }

    }
}
