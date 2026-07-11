using System;

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
