using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Runtime.InteropServices;
using System.Threading;
using System.Windows.Forms;

namespace uclliu
{
    public enum TextOutputMode
    {
        UnicodeSendInput,
        PasteShiftInsert,
        PasteCtrlV,
        PasteBig5
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
        public static readonly string[] PasteCtrlVApps = new string[] { "oxygennotincluded.exe", "iedit_.exe", "notepad++.exe" };
        public static readonly string[] PasteBig5Apps = new string[] { "zip32w", "daqkingcon.exe", "EWinner.exe" };
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

            if (IsPttWindow(context.WindowTitle))
            {
                return TextOutputMode.PasteCtrlV;
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
                || title.IndexOf("ws.ptt.cc", StringComparison.Ordinal) >= 0;
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

        [DllImport("user32.dll", SetLastError = true)]
        private static extern uint SendInput(uint nInputs, INPUT[] pInputs, int cbSize);

        public bool TrySendText(string text, out string error)
        {
            error = null;
            INPUT[] inputs = BuildInputsForText(text);
            if (inputs.Length == 0)
            {
                return true;
            }

            uint sent = SendInput((uint)inputs.Length, inputs, Marshal.SizeOf(typeof(INPUT)));
            if (sent == inputs.Length)
            {
                return true;
            }

            int lastError = Marshal.GetLastWin32Error();
            error = "SendInput inserted " + sent + "/" + inputs.Length + " events";
            if (lastError != 0)
            {
                error += ": " + new Win32Exception(lastError).Message;
            }
            return false;
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
            input.u.ki.dwExtraInfo = IntPtr.Zero;
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

    internal enum ClipboardTextKind
    {
        Unicode,
        Ansi
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

    internal sealed class SendKeysKeySender : IKeySender
    {
        public void SendWait(string keys)
        {
            SendKeys.SendWait(keys);
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
            return TryPasteText(text, ClipboardTextKind.Unicode, sendKeys, out error);
        }

        public bool TryPasteAnsiText(string text, string sendKeys, out string error)
        {
            return TryPasteText(text, ClipboardTextKind.Ansi, sendKeys, out error);
        }

        private bool TryPasteText(string text, ClipboardTextKind textKind, string sendKeys, out string error)
        {
            error = null;

            ClipboardBackup backup;
            if (!TryCaptureBackup(out backup, out error))
            {
                return false;
            }

            bool success = false;
            bool shouldRestore = false;
            string operationError = null;

            try
            {
                if (!TryClipboardAction(delegate { clipboard.SetText(text, textKind); }, "set clipboard failed", out operationError))
                {
                    success = false;
                }
                else
                {
                    shouldRestore = true;
                    try
                    {
                        keySender.SendWait(sendKeys);
                        if (RestoreDelayMs > 0)
                        {
                            sleep(RestoreDelayMs);
                        }
                        success = true;
                    }
                    catch (Exception ex)
                    {
                        operationError = "send keys failed: " + ex.Message;
                    }
                }
            }
            finally
            {
                if (shouldRestore)
                {
                    string restoreError;
                    if (!TryRestoreBackup(backup, out restoreError) && operationError == null)
                    {
                        operationError = restoreError;
                        success = false;
                    }
                }
            }

            error = operationError;
            return success && error == null;
        }

        private bool TryCaptureBackup(out ClipboardBackup backup, out string error)
        {
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
