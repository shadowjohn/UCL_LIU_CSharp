namespace uclliu
{
    public static class KeyboardHookMessage
    {
        public const int WmKeyDown = 0x0100;
        public const int WmKeyUp = 0x0101;
        public const int WmSysKeyDown = 0x0104;
        public const int WmSysKeyUp = 0x0105;

        public static bool IsKeyDown(int message)
        {
            return message == WmKeyDown || message == WmSysKeyDown;
        }

        public static bool IsKeyUp(int message)
        {
            return message == WmKeyUp || message == WmSysKeyUp;
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
        public static ShiftKeyReleaseDecision EvaluateShiftRelease(bool ctrlSpaceEnabled, bool shiftWasUsedWithOtherKey)
        {
            return new ShiftKeyReleaseDecision(true, !ctrlSpaceEnabled && !shiftWasUsedWithOtherKey);
        }
    }
}
