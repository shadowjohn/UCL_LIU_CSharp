using System;
using uclliu;

internal static class CtrlSpaceShortcutTests
{
    public static void TestConsumesWholePressAndTogglesOnce()
    {
        CtrlSpaceShortcutState state = new CtrlSpaceShortcutState();
        AssertDecision(state.HandleKey(162, true, false, true), false, false);
        AssertDecision(state.HandleKey(32, true, false, true), true, true);
        AssertDecision(state.HandleKey(32, true, false, true), true, false);
        AssertDecision(state.HandleKey(32, true, false, true), true, false);
        AssertDecision(state.HandleKey(32, false, true, true), true, false);
        AssertDecision(state.HandleKey(32, true, false, true), true, true);
        AssertDecision(state.HandleKey(32, false, true, true), true, false);
        AssertDecision(state.HandleKey(162, false, true, true), false, false);
        AssertDecision(state.HandleKey(32, true, false, true), false, false);
        AssertDecision(state.HandleKey(32, false, true, true), false, false);
    }

    public static void TestControlReleasedBeforeSpace()
    {
        CtrlSpaceShortcutState state = new CtrlSpaceShortcutState();
        state.HandleKey(163, true, false, true);
        AssertDecision(state.HandleKey(32, true, false, true), true, true);
        AssertDecision(state.HandleKey(163, false, true, true), false, false);
        AssertDecision(state.HandleKey(32, true, false, true), true, false);
        AssertDecision(state.HandleKey(32, false, true, true), true, false);
        AssertDecision(state.HandleKey(32, true, false, true), false, false);
        AssertDecision(state.HandleKey(32, false, true, true), false, false);
    }

    public static void TestDisabledAndOtherShortcutsRemainNative()
    {
        CtrlSpaceShortcutState state = new CtrlSpaceShortcutState();
        AssertDecision(state.HandleKey(162, true, false, false), false, false);
        AssertDecision(state.HandleKey(65, true, false, true), false, false);
        AssertDecision(state.HandleKey(65, false, true, true), false, false);
        AssertDecision(state.HandleKey(32, true, false, false), false, false);
        AssertDecision(state.HandleKey(32, true, false, true), false, false);
        AssertDecision(state.HandleKey(32, false, true, true), false, false);
        AssertDecision(state.HandleKey(32, true, false, true), true, true);
        AssertDecision(state.HandleKey(32, false, true, true), true, false);
    }

    public static void TestControlPressedDuringOrdinarySpace()
    {
        CtrlSpaceShortcutState state = new CtrlSpaceShortcutState();
        AssertDecision(state.HandleKey(32, true, false, true), false, false);
        state.HandleKey(162, true, false, true);
        AssertDecision(state.HandleKey(32, true, false, true), false, false);
        AssertDecision(state.HandleKey(32, false, true, true), false, false);
        AssertDecision(state.HandleKey(32, true, false, true), true, true);
    }

    public static void TestBothControlKeysAreTracked()
    {
        CtrlSpaceShortcutState state = new CtrlSpaceShortcutState();
        state.HandleKey(162, true, false, true);
        state.HandleKey(163, true, false, true);
        state.HandleKey(162, false, true, true);
        if (!state.IsControlDown) throw new Exception("right Ctrl must remain pressed after left Ctrl is released");
        AssertDecision(state.HandleKey(32, true, false, true), true, true);
        AssertDecision(state.HandleKey(32, false, true, true), true, false);
        state.HandleKey(163, false, true, true);
        if (state.IsControlDown) throw new Exception("both Ctrl keys have been released");
    }

    public static void TestCapturedSpaceSurvivesModeChanges()
    {
        CtrlSpaceShortcutState state = new CtrlSpaceShortcutState();
        state.HandleKey(162, true, false, true);
        AssertDecision(state.HandleKey(32, true, false, true), true, true);
        state.HandleKey(162, false, true, false);
        AssertDecision(state.HandleKey(32, true, false, false), true, false);
        AssertDecision(state.HandleKey(32, false, true, false), true, false);
        AssertDecision(state.HandleKey(32, true, false, false), false, false);
    }

    public static void TestConsumedKeyReleaseIsPaired()
    {
        KeyboardConsumedKeys keys = new KeyboardConsumedKeys();
        if (keys.TryConsumeKeyUp(65, true)) throw new Exception("unhandled keyup must remain native");
        keys.MarkKeyDown(65);
        keys.MarkKeyDown(65);
        keys.MarkKeyDown(32);
        if (keys.TryConsumeKeyUp(65, false)) throw new Exception("keydown must not clear pending keyup");
        if (keys.TryConsumeKeyUp(66, true)) throw new Exception("unrelated keyup must remain native");
        if (!keys.TryConsumeKeyUp(65, true)) throw new Exception("converted letter keyup must be consumed");
        if (keys.TryConsumeKeyUp(65, true)) throw new Exception("only one matching release may be consumed");
        if (!keys.TryConsumeKeyUp(32, true)) throw new Exception("converted space keyup must be consumed");
    }

    private static void AssertDecision(CtrlSpaceKeyDecision actual, bool consume, bool toggle)
    {
        if (actual.ShouldConsume != consume || actual.ShouldToggleInputMode != toggle)
        {
            throw new Exception("Expected consume=" + consume + ", toggle=" + toggle
                + "; got consume=" + actual.ShouldConsume + ", toggle=" + actual.ShouldToggleInputMode);
        }
    }
}
