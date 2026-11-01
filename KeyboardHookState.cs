using System;
using System.Collections.Generic;

namespace uclliu
{
    public static class KeyboardHookMessage
    {
        public const int WmKeyDown = 0x0100;
        public const int WmKeyUp = 0x0101;
        public const int WmSysKeyDown = 0x0104;
        public const int WmSysKeyUp = 0x0105;
        public const int LowLevelInjectedFlag = 0x10;

        public static bool IsKeyDown(int message)
        {
            return message == WmKeyDown || message == WmSysKeyDown;
        }

        public static bool IsKeyUp(int message)
        {
            return message == WmKeyUp || message == WmSysKeyUp;
        }

        public static bool IsInjected(int flags)
        {
            return (flags & LowLevelInjectedFlag) == LowLevelInjectedFlag;
        }

        public static bool IsInjectedByUcl(int flags, IntPtr extraInfo)
        {
            return IsInjected(flags) && extraInfo == UnicodeSendInputOutput.UclExtraInfo;
        }
    }

    public struct CtrlSpaceKeyDecision
    {
        public CtrlSpaceKeyDecision(bool shouldConsume, bool shouldToggleInputMode)
        {
            ShouldConsume = shouldConsume;
            ShouldToggleInputMode = shouldToggleInputMode;
        }

        public bool ShouldConsume { get; private set; }
        public bool ShouldToggleInputMode { get; private set; }
    }

    public sealed class CtrlSpaceShortcutState
    {
        private bool leftControlDown;
        private bool rightControlDown;
        private bool spaceDown;
        private bool consumeSpaceUntilRelease;

        public bool IsControlDown { get { return leftControlDown || rightControlDown; } }

        public CtrlSpaceKeyDecision HandleKey(int virtualKey, bool keyDown, bool keyUp, bool enabled)
        {
            if (virtualKey == 162)
            {
                if (keyDown) leftControlDown = true;
                else if (keyUp) leftControlDown = false;
            }
            else if (virtualKey == 163)
            {
                if (keyDown) rightControlDown = true;
                else if (keyUp) rightControlDown = false;
            }

            if (virtualKey != 32)
            {
                return new CtrlSpaceKeyDecision(false, false);
            }
            if (keyUp)
            {
                bool consume = consumeSpaceUntilRelease;
                spaceDown = false;
                consumeSpaceUntilRelease = false;
                return new CtrlSpaceKeyDecision(consume, false);
            }
            if (keyDown)
            {
                bool firstDown = !spaceDown;
                spaceDown = true;
                if (consumeSpaceUntilRelease)
                {
                    return new CtrlSpaceKeyDecision(true, false);
                }
                if (firstDown && enabled && IsControlDown)
                {
                    // 接管完整的 Space 按壓，不依賴 Ctrl／模式在放開時仍保持原狀。
                    consumeSpaceUntilRelease = true;
                    return new CtrlSpaceKeyDecision(true, true);
                }
            }
            return new CtrlSpaceKeyDecision(false, false);
        }
    }

    public sealed class KeyboardConsumedKeys
    {
        private readonly HashSet<int> keysDown = new HashSet<int>();

        public void MarkKeyDown(int virtualKey)
        {
            keysDown.Add(virtualKey);
        }

        public bool TryConsumeKeyUp(int virtualKey, bool keyUp)
        {
            return keyUp && keysDown.Remove(virtualKey);
        }
    }

    public struct ShiftKeyReleaseDecision
    {
        public ShiftKeyReleaseDecision(bool shouldClearShiftState, bool shouldToggleInputMode)
        {
            ShouldClearShiftState = shouldClearShiftState;
            ShouldToggleInputMode = shouldToggleInputMode;
        }

        public bool ShouldClearShiftState { get; private set; }
        public bool ShouldToggleInputMode { get; private set; }
    }

    public static class KeyboardHookStateRules
    {
        public const int MaxStandaloneShiftToggleMilliseconds = 350;

        public static ShiftKeyReleaseDecision EvaluateShiftRelease(bool ctrlSpaceEnabled, bool shiftWasUsedWithOtherKey)
        {
            return EvaluateShiftRelease(ctrlSpaceEnabled, shiftWasUsedWithOtherKey, 0);
        }

        public static ShiftKeyReleaseDecision EvaluateShiftRelease(bool ctrlSpaceEnabled, bool shiftWasUsedWithOtherKey, int shiftHeldMilliseconds)
        {
            bool shouldToggle = !ctrlSpaceEnabled
                && !shiftWasUsedWithOtherKey
                && shiftHeldMilliseconds >= 0
                && shiftHeldMilliseconds <= MaxStandaloneShiftToggleMilliseconds;
            return new ShiftKeyReleaseDecision(true, shouldToggle);
        }
    }

    public static class KeyboardCandidateSelection
    {
        public static bool TryGetCandidateIndex(int virtualKeyCode, out int index)
        {
            if (virtualKeyCode >= 48 && virtualKeyCode <= 57)
            {
                index = virtualKeyCode - 48;
                return true;
            }

            if (virtualKeyCode >= 96 && virtualKeyCode <= 105)
            {
                index = virtualKeyCode - 96;
                return true;
            }

            index = -1;
            return false;
        }
    }

    public static class PhoneCandidateKeyRules
    {
        public static bool ShouldCommitFirstCandidateOnSpace(bool phoneMode, int candidateCount)
        {
            return phoneMode && candidateCount > 0;
        }

        public static bool ShouldPageOnShiftSpace(bool phoneMode, bool hasMorePage, bool halfFullShortcutEnabled)
        {
            return halfFullShortcutEnabled && phoneMode && hasMorePage;
        }
    }

    public struct PhoneInputComposeResult
    {
        public PhoneInputComposeResult(bool accepted, string text, bool showOnly)
        {
            Accepted = accepted;
            Text = text;
            ShowOnly = showOnly;
        }

        public bool Accepted { get; private set; }
        public string Text { get; private set; }
        public bool ShowOnly { get; private set; }
    }

    public static class PhoneInputComposer
    {
        // Aligned with Python issue 166: same-level Bopomofo symbols replace instead of append.
        private static readonly char[] Initials = "ㄅㄆㄇㄈㄉㄊㄋㄌㄍㄎㄏㄐㄑㄒㄓㄔㄕㄖㄗㄘㄙ".ToCharArray();
        private static readonly char[] Medials = "ㄧㄨㄩ".ToCharArray();
        private static readonly char[] Finals = "ㄚㄛㄜㄝㄞㄟㄠㄡㄢㄣㄤㄥㄦ".ToCharArray();
        private static readonly char[] Tones = " ˊˇˋ˙".ToCharArray();

        public static PhoneInputComposeResult Apply(string currentText, string phone)
        {
            string current = currentText ?? "";
            if (String.IsNullOrEmpty(phone) || current.Length >= 4)
            {
                return Reject(current);
            }

            char symbol = phone[0];
            if (current.IndexOf(symbol) >= 0 || EndsWithTone(current))
            {
                return Reject(current);
            }

            if (Contains(Initials, symbol))
            {
                if (current.Length > 0 && Contains(Initials, current[0]))
                {
                    return Accept(symbol + current.Substring(1), true);
                }
                return Accept(symbol + current, true);
            }

            if (Contains(Medials, symbol))
            {
                if (ContainsAny(current, Medials))
                {
                    return Accept(ReplaceAny(current, Medials, symbol), true);
                }

                int finalIndex = IndexOfAny(current, Finals);
                if (finalIndex >= 0)
                {
                    return Accept(current.Insert(finalIndex, symbol.ToString()), true);
                }

                return Accept(current + symbol, true);
            }

            if (Contains(Finals, symbol))
            {
                if (ContainsAny(current, Finals))
                {
                    return Accept(ReplaceAny(current, Finals, symbol), true);
                }

                return Accept(current + symbol, true);
            }

            if (Contains(Tones, symbol))
            {
                return Accept(current + symbol, false);
            }

            return Accept(current + symbol, true);
        }

        private static PhoneInputComposeResult Accept(string text, bool showOnly)
        {
            return new PhoneInputComposeResult(true, text, showOnly);
        }

        private static PhoneInputComposeResult Reject(string text)
        {
            return new PhoneInputComposeResult(false, text, true);
        }

        private static bool EndsWithTone(string value)
        {
            return value.Length > 0 && Contains(Tones, value[value.Length - 1]);
        }

        private static bool ContainsAny(string value, char[] targets)
        {
            for (int i = 0; i < value.Length; i++)
            {
                if (Contains(targets, value[i]))
                {
                    return true;
                }
            }
            return false;
        }

        private static int IndexOfAny(string value, char[] targets)
        {
            for (int i = 0; i < value.Length; i++)
            {
                if (Contains(targets, value[i]))
                {
                    return i;
                }
            }
            return -1;
        }

        private static string ReplaceAny(string value, char[] targets, char replacement)
        {
            char[] chars = value.ToCharArray();
            for (int i = 0; i < chars.Length; i++)
            {
                if (Contains(targets, chars[i]))
                {
                    chars[i] = replacement;
                }
            }
            return new string(chars);
        }

        private static bool Contains(char[] values, char value)
        {
            for (int i = 0; i < values.Length; i++)
            {
                if (values[i] == value)
                {
                    return true;
                }
            }
            return false;
        }
    }

    public struct HalfFullShortcutDecision
    {
        public HalfFullShortcutDecision(bool shouldToggleHalfFull, bool shouldKeepShiftDown)
        {
            ShouldToggleHalfFull = shouldToggleHalfFull;
            ShouldKeepShiftDown = shouldKeepShiftDown;
        }

        public bool ShouldToggleHalfFull { get; private set; }
        public bool ShouldKeepShiftDown { get; private set; }
    }

    public static class HalfFullShortcutRules
    {
        public static bool ShouldToggleOnShiftSpace(bool halfFullShortcutEnabled, bool shiftDown)
        {
            return EvaluateShiftSpace(halfFullShortcutEnabled, shiftDown).ShouldToggleHalfFull;
        }

        public static HalfFullShortcutDecision EvaluateShiftSpace(bool halfFullShortcutEnabled, bool shiftDown)
        {
            bool shouldToggle = halfFullShortcutEnabled && shiftDown;
            return new HalfFullShortcutDecision(shouldToggle, shouldToggle);
        }
    }

    public static class SmartCandidateKeyRules
    {
        public static bool ShouldEndContext(int virtualKey, bool keyDown)
        {
            return keyDown && virtualKey == 13;
        }

        public static int SelectionNumber(int virtualKey, bool shiftDown)
        {
            return shiftDown && virtualKey >= 49 && virtualKey <= 53 ? virtualKey - 48 : 0;
        }

        public static bool ShouldPageOnShiftSpace(bool visible, bool hasNextPage)
        {
            return visible && hasNextPage;
        }
    }
}
