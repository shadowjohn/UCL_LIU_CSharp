using System;
using System.Collections.Generic;
using System.ComponentModel;
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

    public static class TextOutputRouter
    {
        public static TextOutputMode Select(string defaultOutputType, string processName, IEnumerable<string> shiftInsertApps, IEnumerable<string> ctrlVApps, IEnumerable<string> big5Apps)
        {
            string outputType = (defaultOutputType ?? "DEFAULT").Trim().ToUpperInvariant();

            if (outputType == "BIG5")
            {
                return TextOutputMode.PasteBig5;
            }

            if (outputType == "PASTE")
            {
                return TextOutputMode.PasteShiftInsert;
            }

            if (MatchesApp(processName, shiftInsertApps))
            {
                return TextOutputMode.PasteShiftInsert;
            }

            if (MatchesApp(processName, ctrlVApps))
            {
                return TextOutputMode.PasteCtrlV;
            }

            if (MatchesApp(processName, big5Apps))
            {
                return TextOutputMode.PasteBig5;
            }

            return TextOutputMode.UnicodeSendInput;
        }

        private static bool MatchesApp(string processName, IEnumerable<string> appPatterns)
        {
            if (string.IsNullOrWhiteSpace(processName) || appPatterns == null)
            {
                return false;
            }

            string normalizedProcess = processName.Trim().ToLowerInvariant();
            foreach (string appPattern in appPatterns)
            {
                if (string.IsNullOrWhiteSpace(appPattern))
                {
                    continue;
                }

                string normalizedPattern = appPattern.Trim().ToLowerInvariant();
                if (normalizedProcess == normalizedPattern || normalizedProcess.IndexOf(normalizedPattern, StringComparison.Ordinal) >= 0)
                {
                    return true;
                }
            }

            return false;
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

        private sealed class WinFormsClipboardGateway : IClipboardGateway
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

        private sealed class SendKeysKeySender : IKeySender
        {
            public void SendWait(string keys)
            {
                SendKeys.SendWait(keys);
            }
        }
    }
}
