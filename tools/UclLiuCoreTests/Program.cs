using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using uclliu;

internal static class Program
{
    private static int Main()
    {
        int failed = 0;
        failed += Run("cin parser groups multiple words under one root", TestCinParserGroupsWords);
        failed += Run("unitab converter writes expected cin entry", TestUnitabConverterWritesCinEntry);
        failed += Run("ensure json converts cin in working directory", TestEnsureJsonConvertsCin);
        failed += Run("app info exposes version and author message", TestAppInfoExposesVersionAndAuthorMessage);
        failed += Run("alt tab window style hides tool window from switcher", TestAltTabWindowStyleHidesToolWindowFromSwitcher);
        failed += Run("foreground process snapshot normalizes process name", TestForegroundProcessSnapshotNormalizesProcessName);
        failed += Run("tray menu text marks current output mode", TestTrayMenuTextMarksCurrentOutputMode);
        failed += Run("tray menu text marks boolean settings", TestTrayMenuTextMarksBooleanSettings);
        failed += Run("tray menu opens on left and right click", TestTrayMenuOpensOnLeftAndRightClick);
        failed += Run("tsf bridge assets prefer architecture subfolder", TestTsfBridgeAssetsPreferArchitectureSubfolder);
        failed += Run("tsf bridge command text is stable", TestTsfBridgeCommandText);
        failed += Run("short root setting toggles and persists SP", TestShortRootSettingTogglesAndPersistsSp);
        failed += Run("short root setting reads normalized config", TestShortRootSettingReadsNormalizedConfig);
        failed += Run("short mode width is bounded and proportional", TestShortModeWidth);
        failed += Run("custom root validation matches UCL rules", TestCustomRootValidation);
        failed += Run("simple ini reads default section values", TestSimpleIniReadsDefaultSectionValues);
        failed += Run("simple ini writes readable sections", TestSimpleIniWritesReadableSections);
        failed += Run("liu json parser decodes chardefs without System.Json", TestLiuJsonParserDecodesChardefs);
        failed += Run("liu reverse lookup builds numbered candidate hash", TestLiuReverseLookupBuildsNumberedCandidateHash);
        failed += Run("custom dictionary lowercases and merges values", TestCustomDictionaryMerge);
        failed += Run("custom dictionary save writes deterministic json", TestCustomDictionarySave);
        failed += Run("unicode sendinput builds literal down/up events", TestUnicodeSendInputBuildsLiteralEvents);
        failed += Run("unicode sendinput preserves surrogate pairs", TestUnicodeSendInputPreservesSurrogatePairs);
        failed += Run("unicode sendinput marks injected events", TestUnicodeSendInputMarksInjectedEvents);
        failed += Run("unicode sendinput sends each character as separate action", TestUnicodeSendInputSendsEachCharacterAsSeparateAction);
        failed += Run("keyboard hook only treats marked injected keys as UCL output", TestKeyboardHookOnlyTreatsMarkedInjectedKeysAsUclOutput);
        failed += Run("keyboard hook performance policy raises priority and cache duration", TestKeyboardHookPerformancePolicy);
        failed += Run("keyboard hook latency monitor logs slow callbacks with throttle", TestKeyboardHookLatencyMonitor);
        failed += Run("clipboard paste restores original text after send failure", TestClipboardPasteRestoresOriginalTextAfterSendFailure);
        failed += Run("clipboard paste reports set clipboard failure before send", TestClipboardPasteReportsSetClipboardFailureBeforeSend);
        failed += Run("selected text transform copies selection and restores clipboard", TestSelectedTextTransformCopiesSelectionAndRestoresClipboard);
        failed += Run("selected text transform does not use stale clipboard", TestSelectedTextTransformDoesNotUseStaleClipboard);
        failed += Run("selected text transform dispatcher posts work outside hook", TestSelectedTextTransformDispatcherPostsWorkOutsideHook);
        failed += Run("deferred text output dispatcher posts send outside hook", TestDeferredTextOutputDispatcherPostsSendOutsideHook);
        failed += Run("deferred text output dispatcher prepares before posting", TestDeferredTextOutputDispatcherPreparesBeforePosting);
        failed += Run("deferred text output dispatcher preserves label update order", TestDeferredTextOutputDispatcherPreservesLabelUpdateOrder);
        failed += Run("output router prefers unicode sendinput unless app needs paste", TestOutputRouterPrefersUnicodeSendInputUnlessAppNeedsPaste);
        failed += Run("output router supports manual tsf mode", TestOutputRouterSupportsManualTsfMode);
        failed += Run("output router matches app names with optional exe suffix", TestOutputRouterMatchesAppNamesWithOptionalExeSuffix);
        failed += Run("default compatibility keeps Notepad++ on unicode sendinput", TestDefaultCompatibilityKeepsNotepadPlusPlusOnUnicodeSendInput);
        failed += Run("tsf bridge protocol escapes json and parses ok", TestTsfBridgeProtocolEscapesJsonAndParsesOk);
        failed += Run("tsf bridge output tries pid pipe before global pipe", TestTsfBridgeOutputTriesPidPipeBeforeGlobalPipe);
        failed += Run("tsf bridge output does not try global pipe after commit failure", TestTsfBridgeOutputDoesNotTryGlobalPipeAfterCommitFailure);
        failed += Run("window message char output posts each character to focused control", TestWindowMessageCharOutputPostsToFocusedControl);
        failed += Run("output router forces paste for PTT browser titles", TestOutputRouterForcesPasteForPttBrowserTitles);
        failed += Run("output router forces paste for Windows 11 Notepad", TestOutputRouterForcesPasteForWindows11Notepad);
        failed += Run("typing sound volume clamps to supported range", TestTypingSoundVolumeClamp);
        failed += Run("typing sound suppresses repeated keydown until keyup", TestTypingSoundKeyState);
        failed += Run("typing sound catalog maps special wav names", TestTypingSoundCatalogSpecialNames);
        failed += Run("typing sound volume scaler adjusts pcm16 samples", TestTypingSoundVolumeScalerPcm16);
        failed += Run("typing sound reuses loaded handle on hot playback", TestTypingSoundReusesLoadedHandleOnHotPlayback);
        failed += Run("typing sound parses pcm16 wav for independent waveout playback", TestTypingSoundParsesPcm16WavForIndependentWaveOutPlayback);
        failed += Run("keyboard hook treats system key messages as key transitions", TestKeyboardHookSystemMessages);
        failed += Run("shift release clears state even when ctrl space is enabled", TestShiftReleaseClearsStateWithCtrlSpace);
        failed += Run("shift release toggles input only for standalone shift mode", TestShiftReleaseToggleRules);
        failed += Run("shift release ignores stale standalone shift", TestShiftReleaseIgnoresStaleStandaloneShift);
        failed += Run("pinyi v001 same sound skips phonetic code and bopomofo tokens", TestPinyiV001SkipsPhoneCodeAndBopomofo);
        failed += Run("pinyi v001 same sound sorts by closest token index", TestPinyiV001SortsByClosestTokenIndex);
        failed += Run("pinyi legacy same sound keeps whole matching lines", TestPinyiLegacyKeepsWholeMatchingLines);
        failed += Run("phone table converts zhuyin query to candidates", TestPhoneTableConvertsZhuyinQueryToCandidates);
        failed += Run("phone table maps words back to zhuyin labels", TestPhoneTableMapsWordsBackToZhuyinLabels);

        if (failed > 0)
        {
            Console.Error.WriteLine("FAILED: " + failed);
            return 1;
        }

        Console.WriteLine("PASS: all core tests");
        return 0;
    }

    private static int Run(string name, Action test)
    {
        try
        {
            test();
            Console.WriteLine("PASS " + name);
            return 0;
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine("FAIL " + name + ": " + ex.Message);
            return 1;
        }
    }

    private static void TestCinParserGroupsWords()
    {
        string cin = "%gen_inp\r\n%chardef begin\r\nucl 肥 米\r\nabc 一\r\n%chardef end\r\n";
        string json = LiuTableConverter.ConvertCinTextToJson(cin);

        AssertContains(json, "\"ucl\"");
        AssertContains(json, "\"肥\"");
        AssertContains(json, "\"米\"");
        AssertContains(json, "\"abc\"");
        AssertContains(json, "\"一\"");
    }

    private static void TestUnitabConverterWritesCinEntry()
    {
        byte[] unitab = BuildUnitab("uc", 12, 0, 0x80A5);
        string cin = LiuTableConverter.ConvertUnitabBytesToCin(unitab);

        AssertContains(cin, "%chardef begin");
        AssertContains(cin, "ucl 肥");
        AssertContains(cin, "%chardef end");
    }

    private static void TestEnsureJsonConvertsCin()
    {
        string dir = Path.Combine(Path.GetTempPath(), "uclliu-core-tests-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(dir);
        try
        {
            File.WriteAllText(Path.Combine(dir, "liu.cin"), "%chardef begin\nucl 肥\n%chardef end\n", new UTF8Encoding(false));

            bool created = LiuTableConverter.EnsureLiuJson(dir, null);

            AssertTrue(created, "EnsureLiuJson should create liu.json from liu.cin");
            string json = File.ReadAllText(Path.Combine(dir, "liu.json"), Encoding.UTF8);
            AssertContains(json, "\"ucl\"");
            AssertContains(json, "\"肥\"");
        }
        finally
        {
            Directory.Delete(dir, true);
        }
    }

    private static void TestAppInfoExposesVersionAndAuthorMessage()
    {
        string expected = "UCLLIU 肥米輸入法 C# 版\n\n"
            + "作者：羽山秋人 (https://3wa.tw)\n"
            + "版本：0.12\n\n"
            + "熱鍵提示：\n\n"
            + "「,,,VERSION」目前版本\n"
            + "「'ucl」同音字查詢\n"
            + "「';zo6」注音查詢\n"
            + "「,,,UNLOCK」回到正常模式\n"
            + "「,,,LOCK」進入遊戲模式\n"
            + "「,,,C」簡體模式\n"
            + "「,,,T」繁體模式\n"
            + "「,,,S」UI變窄\n"
            + "「,,,L」UI變寬\n"
            + "「,,,+」UI變大\n"
            + "「,,,-」UI變小\n"
            + "「,,,X」框字的字根轉回文字\n"
            + "「,,,Z」框字的文字變成字根\n"
            + "「,,,BOX」開啟自定詞庫\n";

        AssertEqual("0.12", UclLiuAppInfo.Version);
        AssertEqual("UCLLIU 肥米輸入法 C# 版", UclLiuAppInfo.AboutTitle);
        AssertEqual("Fastest Chinese Input Method", UclLiuAppInfo.FileDescription);
        AssertEqual("UCLLIU Input Method", UclLiuAppInfo.ProductName);
        AssertEqual("Magic 3WA The legend of trainer (https://3wa.tw)", UclLiuAppInfo.CompanyName);
        AssertEqual("Copyright (c) MIT 3WA Studio (https://3wa.tw)", UclLiuAppInfo.Copyright);
        AssertEqual("Author: FeatherMountain (https://3wa.tw)", UclLiuAppInfo.Comments);
        AssertEqual(expected, UclLiuAppInfo.BuildAboutText());
    }

    private static void TestAltTabWindowStyleHidesToolWindowFromSwitcher()
    {
        int original = AltTabWindowStyle.WsExAppWindow;

        int adjusted = AltTabWindowStyle.HideFromSwitcher(original);

        AssertEqual(0, adjusted & AltTabWindowStyle.WsExAppWindow);
        AssertEqual(AltTabWindowStyle.WsExToolWindow, adjusted & AltTabWindowStyle.WsExToolWindow);
    }

    private static void TestForegroundProcessSnapshotNormalizesProcessName()
    {
        Dictionary<string, string> snapshot = ForegroundProcessSnapshot.Create("測試 - Notepad++", "Notepad++", 42);

        AssertEqual("測試 - Notepad++", snapshot["PROCESS_TITLE"]);
        AssertEqual("notepad++", snapshot["PROCESS_NAME"]);
        AssertEqual("42", snapshot["PROCESS_PID"]);
    }

    private static void TestTrayMenuTextMarksCurrentOutputMode()
    {
        AssertEqual("【●】正常出字模式（Unicode）", TrayMenuText.OutputModeDefault("DEFAULT"));
        AssertEqual("【　】正常出字模式（Unicode）", TrayMenuText.OutputModeDefault("PASTE"));
        AssertEqual("【●】BIG5模式", TrayMenuText.OutputModeBig5("BIG5"));
        AssertEqual("【●】複製貼上模式", TrayMenuText.OutputModePaste("PASTE"));
        AssertEqual("【●】TSF出字模式", TrayMenuText.OutputModeTsf("TSF"));
        AssertEqual("【　】TSF出字模式", TrayMenuText.OutputModeTsf("DEFAULT"));
    }

    private static void TestTrayMenuTextMarksBooleanSettings()
    {
        AssertEqual("5.【●】使用 CTRL+SPACE 切換輸入法", TrayMenuText.ToggleItem("5.", true, "使用 CTRL+SPACE 切換輸入法"));
        AssertEqual("5.【　】使用 CTRL+SPACE 切換輸入法", TrayMenuText.ToggleItem("5.", false, "使用 CTRL+SPACE 切換輸入法"));
    }

    private static void TestTrayMenuOpensOnLeftAndRightClick()
    {
        AssertTrue(TrayMenuClickPolicy.ShouldOpenMenu(System.Windows.Forms.MouseButtons.Left), "left click should open tray menu");
        AssertTrue(TrayMenuClickPolicy.ShouldOpenMenu(System.Windows.Forms.MouseButtons.Right), "right click should open tray menu");
        AssertTrue(!TrayMenuClickPolicy.ShouldOpenMenu(System.Windows.Forms.MouseButtons.Middle), "middle click should not open tray menu");
    }

    private static void TestTsfBridgeAssetsPreferArchitectureSubfolder()
    {
        string dir = Path.Combine(Path.GetTempPath(), "uclliu-tsf-tests-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(Path.Combine(dir, "tsf_bridge", "x64"));
        Directory.CreateDirectory(Path.Combine(dir, "tsf_bridge", "x86"));
        try
        {
            File.WriteAllText(Path.Combine(dir, "tsf_bridge", "UclTsfBridge.dll"), "root");
            File.WriteAllText(Path.Combine(dir, "tsf_bridge", "x64", "UclTsfBridge.dll"), "x64");
            File.WriteAllText(Path.Combine(dir, "tsf_bridge", "x86", "UclTsfBridge.dll"), "x86");
            File.WriteAllText(Path.Combine(dir, "tsf_bridge", "register_tsf_bridge.bat"), "");
            File.WriteAllText(Path.Combine(dir, "tsf_bridge", "unregister_tsf_bridge.bat"), "");
            File.WriteAllText(Path.Combine(dir, "tsf_bridge", "unlock_tsf_bridge.ps1"), "");

            TsfBridgeAssets assets = TsfBridgeAssetLocator.Locate(dir, true);

            AssertTrue(assets.HasBridgeDll, "bridge dll should be found");
            AssertTrue(assets.BridgeDllPath.EndsWith(Path.Combine("tsf_bridge", "x64", "UclTsfBridge.dll"), StringComparison.OrdinalIgnoreCase), "x64 dll should be preferred");
            AssertTrue(assets.HasRegisterScript, "register script should be found");
            AssertTrue(assets.HasUnregisterScript, "unregister script should be found");
            AssertTrue(assets.HasUnlockScript, "unlock script should be found");
        }
        finally
        {
            Directory.Delete(dir, true);
        }
    }

    private static void TestTsfBridgeCommandText()
    {
        AssertEqual("TSF Bridge 已註冊", TrayMenuText.TsfBridgeStatus(true));
        AssertEqual("TSF Bridge 未註冊", TrayMenuText.TsfBridgeStatus(false));
    }

    private static void TestShortRootSettingTogglesAndPersistsSp()
    {
        SimpleIniData config = new SimpleIniData();
        config["DEFAULT"]["SP"] = "0";
        int saveCount = 0;

        bool enabled = ShortRootDisplaySetting.Toggle(config, false, delegate { saveCount++; });

        AssertTrue(enabled, "toggle should enable short root display");
        AssertEqual("1", config["DEFAULT"]["SP"]);
        AssertEqual(1, saveCount);

        enabled = ShortRootDisplaySetting.Toggle(config, enabled, delegate { saveCount++; });

        AssertTrue(!enabled, "toggle should disable short root display");
        AssertEqual("0", config["DEFAULT"]["SP"]);
        AssertEqual(2, saveCount);
    }

    private static void TestShortRootSettingReadsNormalizedConfig()
    {
        AssertTrue(!ShortRootDisplaySetting.IsEnabled("0"), "0 should disable short root display");
        AssertTrue(ShortRootDisplaySetting.IsEnabled("1"), "1 should enable short root display");
        AssertTrue(ShortRootDisplaySetting.IsEnabled("9"), "positive values should enable short root display");
        AssertEqual("0", ShortRootDisplaySetting.Normalize("-1"));
        AssertEqual("0", ShortRootDisplaySetting.Normalize("bad"));
        AssertEqual("1", ShortRootDisplaySetting.Normalize("9"));
    }

    private static void TestShortModeWidth()
    {
        AssertEqual(0, UiLayoutCalculator.ShortModeTextWidth("", 1.0, 28, 0, 900));
        AssertEqual(84, UiLayoutCalculator.ShortModeTextWidth("abc", 1.0, 28, 0, 900));
        AssertEqual(900, UiLayoutCalculator.ShortModeTextWidth(new string('字', 100), 1.0, 28, 0, 900));
        AssertEqual(42, UiLayoutCalculator.ShortModeTextWidth("abc", 0.5, 28, 0, 900));
    }

    private static void TestCustomRootValidation()
    {
        AssertTrue(CustomDictionaryStore.IsValidRootKey("abc"), "abc should be valid");
        AssertTrue(CustomDictionaryStore.IsValidRootKey(",.]['"), ",.][' should be valid");
        AssertTrue(!CustomDictionaryStore.IsValidRootKey("abcde1"), "digits should be invalid");
        AssertTrue(!CustomDictionaryStore.IsValidRootKey("abcdef"), "longer than 5 chars should be invalid");
        AssertEqual("ucl", CustomDictionaryStore.NormalizeRootKey("UCL"));
    }

    private static void TestSimpleIniReadsDefaultSectionValues()
    {
        string ini = "; comment\r\n[DEFAULT]\r\nX = 120\r\nSEND_KIND_1_PASTE = putty.exe,foo=bar\r\n\r\n[OTHER]\r\nX=999\r\n";

        SimpleIniData data = SimpleIniData.Parse(ini);

        AssertEqual("120", data["DEFAULT"]["X"]);
        AssertEqual("putty.exe,foo=bar", data["DEFAULT"]["SEND_KIND_1_PASTE"]);
        AssertEqual("", data["DEFAULT"]["MISSING"]);
    }

    private static void TestSimpleIniWritesReadableSections()
    {
        string dir = Path.Combine(Path.GetTempPath(), "uclliu-ini-tests-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(dir);
        try
        {
            string path = Path.Combine(dir, "UCLLIU.ini");
            SimpleIniData data = new SimpleIniData();
            data["DEFAULT"]["X"] = "120";
            data["DEFAULT"]["PLAY_SOUND_ENABLE"] = "1";

            SimpleIniFile.WriteFile(path, data);
            SimpleIniData loaded = SimpleIniFile.ReadFile(path);

            AssertEqual("120", loaded["DEFAULT"]["X"]);
            AssertEqual("1", loaded["DEFAULT"]["PLAY_SOUND_ENABLE"]);
        }
        finally
        {
            Directory.Delete(dir, true);
        }
    }

    private static void TestLiuJsonParserDecodesChardefs()
    {
        Dictionary<string, List<string>> chardefs = LiuJsonTable.ParseChardefsJson("{\"chardefs\":{\"ucl\":[\"肥\",\"米\"],\"abc\":\"一\"}}");

        AssertSequence(new string[] { "肥", "米" }, chardefs["ucl"].ToArray());
        AssertSequence(new string[] { "一" }, chardefs["abc"].ToArray());
    }

    private static void TestLiuReverseLookupBuildsNumberedCandidateHash()
    {
        Dictionary<string, List<string>> chardefs = new Dictionary<string, List<string>>(StringComparer.Ordinal);
        chardefs["gqd"] = new List<string>() { "動", "舅", "娚" };
        chardefs["A"] = new List<string>() { "舅" };

        LiuReverseLookupTable table = LiuReverseLookupTable.Build(chardefs);

        AssertEqual("gqd", table.WordToRoot["動"]);
        AssertEqual("a", table.WordToRoot["舅"]);
        AssertEqual("gqd2", table.WordToRoot["娚"]);
        AssertEqual("動", table.CodeToWord["gqd"]);
        AssertEqual("舅", table.CodeToWord["gqd1"]);
        AssertEqual("娚", table.CodeToWord["gqd2"]);
        AssertEqual("舅", table.CodeToWord["a"]);
    }

    private static void TestCustomDictionaryMerge()
    {
        Dictionary<string, List<string>> chardefs = new Dictionary<string, List<string>>(StringComparer.Ordinal);
        chardefs["ucl"] = new List<string>() { "肥" };
        chardefs["abc"] = new List<string>() { "一" };
        Dictionary<string, List<string>> custom = new Dictionary<string, List<string>>();
        custom["UCL"] = new List<string>() { "肥宅", "肥" };
        custom["new"] = new List<string>() { "新詞" };

        int merged = CustomDictionaryStore.MergeInto(chardefs, custom);

        AssertEqual(2, merged);
        AssertSequence(new string[] { "肥", "肥宅" }, chardefs["ucl"].ToArray());
        AssertSequence(new string[] { "新詞" }, chardefs["new"].ToArray());
    }

    private static void TestCustomDictionarySave()
    {
        string dir = Path.Combine(Path.GetTempPath(), "uclliu-custom-tests-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(dir);
        try
        {
            string path = Path.Combine(dir, "custom.json");
            Dictionary<string, List<string>> custom = new Dictionary<string, List<string>>();
            custom["ucl"] = new List<string>() { "肥宅", "肥米" };
            custom["box"] = new List<string>() { "盒子" };

            CustomDictionaryStore.Save(path, custom);
            Dictionary<string, List<string>> loaded = CustomDictionaryStore.Load(path, null);

            AssertSequence(new string[] { "盒子" }, loaded["box"].ToArray());
            AssertSequence(new string[] { "肥宅", "肥米" }, loaded["ucl"].ToArray());
        }
        finally
        {
            Directory.Delete(dir, true);
        }
    }

    private static void TestUnicodeSendInputBuildsLiteralEvents()
    {
        UnicodeSendInputOutput.INPUT[] inputs = UnicodeSendInputOutput.BuildInputsForText("+^%{}() ");

        AssertEqual(16, inputs.Length);
        AssertKeyboardInput(inputs[0], '+', false);
        AssertKeyboardInput(inputs[1], '+', true);
        AssertKeyboardInput(inputs[2], '^', false);
        AssertKeyboardInput(inputs[3], '^', true);
        AssertKeyboardInput(inputs[14], ' ', false);
        AssertKeyboardInput(inputs[15], ' ', true);
    }

    private static void TestUnicodeSendInputPreservesSurrogatePairs()
    {
        string text = char.ConvertFromUtf32(0x20BB7);

        UnicodeSendInputOutput.INPUT[] inputs = UnicodeSendInputOutput.BuildInputsForText(text);

        AssertEqual(4, inputs.Length);
        AssertKeyboardInput(inputs[0], text[0], false);
        AssertKeyboardInput(inputs[1], text[0], true);
        AssertKeyboardInput(inputs[2], text[1], false);
        AssertKeyboardInput(inputs[3], text[1], true);
    }

    private static void TestUnicodeSendInputMarksInjectedEvents()
    {
        UnicodeSendInputOutput.INPUT[] inputs = UnicodeSendInputOutput.BuildInputsForText("肥");

        AssertEqual(UnicodeSendInputOutput.UclExtraInfo, inputs[0].u.ki.dwExtraInfo);
        AssertEqual(UnicodeSendInputOutput.UclExtraInfo, inputs[1].u.ki.dwExtraInfo);
    }

    private static void TestUnicodeSendInputSendsEachCharacterAsSeparateAction()
    {
        FakeUnicodeInputSender sender = new FakeUnicodeInputSender();
        UnicodeSendInputOutput output = new UnicodeSendInputOutput(sender);

        string error;
        bool ok = output.TrySendText("肥A", out error);

        AssertTrue(ok, "unicode sendinput should succeed");
        AssertEqual(null, error);
        AssertEqual(2, sender.Batches.Count);
        AssertEqual("肥", sender.BatchText(0));
        AssertEqual("A", sender.BatchText(1));
    }

    private static void TestKeyboardHookOnlyTreatsMarkedInjectedKeysAsUclOutput()
    {
        AssertTrue(KeyboardHookMessage.IsInjected(KeyboardHookMessage.LowLevelInjectedFlag), "injected flag should be detected");
        AssertTrue(KeyboardHookMessage.IsInjectedByUcl(KeyboardHookMessage.LowLevelInjectedFlag, UnicodeSendInputOutput.UclExtraInfo), "marked injected event should be UCL output");
        AssertTrue(!KeyboardHookMessage.IsInjectedByUcl(KeyboardHookMessage.LowLevelInjectedFlag, IntPtr.Zero), "generic injected event should not be UCL unicode output");
        AssertTrue(!KeyboardHookMessage.IsInjectedByUcl(0, UnicodeSendInputOutput.UclExtraInfo), "physical event should not be UCL unicode output");
    }

    private static void TestKeyboardHookPerformancePolicy()
    {
        AssertEqual((int)System.Diagnostics.ProcessPriorityClass.AboveNormal, (int)KeyboardHookPerformancePolicy.ProcessPriorityClass);
        AssertEqual((int)System.Threading.ThreadPriority.AboveNormal, (int)KeyboardHookPerformancePolicy.UiThreadPriority);
        AssertTrue(KeyboardHookPerformancePolicy.ForegroundProcessCacheMilliseconds >= 500, "foreground process cache should reduce hot-path process lookup");
        AssertTrue(KeyboardHookPerformancePolicy.SlowHookThresholdMilliseconds <= 50, "slow hook threshold should catch visible typing stalls");
    }

    private static void TestKeyboardHookLatencyMonitor()
    {
        KeyboardHookLatencyMonitor monitor = new KeyboardHookLatencyMonitor(20, 1000);
        long firstTick = KeyboardHookLatencyMonitor.MillisecondsToTicks(1000);

        AssertTrue(!monitor.ShouldLogElapsedMilliseconds(19, firstTick), "fast callback should not log");
        AssertTrue(monitor.ShouldLogElapsedMilliseconds(20, firstTick), "slow callback should log");
        AssertTrue(!monitor.ShouldLogElapsedMilliseconds(80, firstTick + KeyboardHookLatencyMonitor.MillisecondsToTicks(500)), "slow callback should be throttled");
        AssertTrue(monitor.ShouldLogElapsedMilliseconds(80, firstTick + KeyboardHookLatencyMonitor.MillisecondsToTicks(1000)), "slow callback should log after throttle interval");
    }

    private static void TestClipboardPasteRestoresOriginalTextAfterSendFailure()
    {
        FakeClipboardGateway clipboard = new FakeClipboardGateway("原剪貼簿");
        FakeKeySender keySender = new FakeKeySender();
        keySender.ThrowOnSend = true;
        ClipboardPasteOutput output = new ClipboardPasteOutput(clipboard, keySender, delegate(int ms) { });

        string error;
        bool ok = output.TryPasteText("肥米", "^{v}", out error);

        AssertTrue(!ok, "paste should report send failure");
        AssertContains(error, "send keys failed");
        AssertEqual("原剪貼簿", clipboard.Text);
        AssertEqual(1, keySender.SendCount);
    }

    private static void TestClipboardPasteReportsSetClipboardFailureBeforeSend()
    {
        FakeClipboardGateway clipboard = new FakeClipboardGateway("原剪貼簿");
        clipboard.ThrowOnSetText = true;
        FakeKeySender keySender = new FakeKeySender();
        ClipboardPasteOutput output = new ClipboardPasteOutput(clipboard, keySender, delegate(int ms) { });

        string error;
        bool ok = output.TryPasteText("肥米", "^{v}", out error);

        AssertTrue(!ok, "paste should report clipboard failure");
        AssertContains(error, "set clipboard failed");
        AssertEqual("原剪貼簿", clipboard.Text);
        AssertEqual(0, keySender.SendCount);
    }

    private static void TestSelectedTextTransformCopiesSelectionAndRestoresClipboard()
    {
        FakeClipboardGateway clipboard = new FakeClipboardGateway("原剪貼簿");
        FakeKeySender keySender = new FakeKeySender();
        keySender.OnSend = delegate(string keys)
        {
            AssertEqual("^{c}", keys);
            AssertEqual(null, clipboard.Text);
            clipboard.SetText("ucl d gz", ClipboardTextKind.Unicode);
        };
        SelectedTextTransformCommand command = new SelectedTextTransformCommand(clipboard, keySender, delegate(int ms) { });
        string sent = null;

        string error;
        bool ok = command.TryRun(
            delegate(string selected) { return selected.Replace("ucl d gz", "肥的好"); },
            delegate(string output) { sent = output; },
            out error);

        AssertTrue(ok, "selected text transform should succeed");
        AssertEqual(null, error);
        AssertEqual("肥的好", sent);
        AssertEqual("原剪貼簿", clipboard.Text);
        AssertEqual(1, keySender.SendCount);
    }

    private static void TestSelectedTextTransformDoesNotUseStaleClipboard()
    {
        FakeClipboardGateway clipboard = new FakeClipboardGateway("舊資料");
        FakeKeySender keySender = new FakeKeySender();
        SelectedTextTransformCommand command = new SelectedTextTransformCommand(clipboard, keySender, delegate(int ms) { });
        bool outputCalled = false;

        string error;
        bool ok = command.TryRun(
            delegate(string selected) { return "不應該送出"; },
            delegate(string output) { outputCalled = true; },
            out error);

        AssertTrue(!ok, "selected text transform should fail when copy returns no text");
        AssertContains(error, "copy selected text failed");
        AssertTrue(!outputCalled, "stale clipboard text should not be transformed");
        AssertEqual("舊資料", clipboard.Text);
        AssertEqual(1, keySender.SendCount);
    }

    private static void TestSelectedTextTransformDispatcherPostsWorkOutsideHook()
    {
        List<Action> postedActions = new List<Action>();
        List<string> events = new List<string>();
        FakeSelectedTextTransformCommand command = new FakeSelectedTextTransformCommand("ucl");
        SelectedTextTransformDispatcher dispatcher = new SelectedTextTransformDispatcher(
            delegate(Action action)
            {
                events.Add("post");
                postedActions.Add(action);
            },
            delegate(bool isSending)
            {
                events.Add("send=" + isSending.ToString());
            },
            delegate(string message)
            {
                events.Add("log:" + message);
            });

        dispatcher.Queue(
            command,
            ",,,x",
            delegate(string selected)
            {
                events.Add("transform");
                return selected.ToUpperInvariant();
            },
            delegate(string output)
            {
                events.Add("output:" + output);
            });

        AssertEqual(0, command.RunCount);
        AssertSequence(new string[] { "post" }, events.ToArray());
        AssertEqual(1, postedActions.Count);

        postedActions[0]();

        AssertEqual(1, command.RunCount);
        AssertSequence(new string[] { "post", "send=True", "transform", "output:UCL", "send=False" }, events.ToArray());
    }

    private static void TestDeferredTextOutputDispatcherPostsSendOutsideHook()
    {
        List<Action> postedActions = new List<Action>();
        List<string> events = new List<string>();
        DeferredTextOutputDispatcher dispatcher = new DeferredTextOutputDispatcher(
            delegate(Action action)
            {
                events.Add("post");
                postedActions.Add(action);
            });

        dispatcher.Queue("肥", delegate(string output)
        {
            events.Add("send:" + output);
        });

        AssertSequence(new string[] { "post" }, events.ToArray());
        AssertEqual(1, postedActions.Count);

        postedActions[0]();

        AssertSequence(new string[] { "post", "send:肥" }, events.ToArray());
    }

    private static void TestDeferredTextOutputDispatcherPreparesBeforePosting()
    {
        List<Action> postedActions = new List<Action>();
        List<string> events = new List<string>();
        DeferredTextOutputDispatcher dispatcher = new DeferredTextOutputDispatcher(
            delegate(Action action)
            {
                events.Add("post");
                postedActions.Add(action);
            });

        dispatcher.Queue(
            "肥",
            delegate(string output)
            {
                events.Add("prepare:" + output);
                return output + "米";
            },
            delegate(string output)
            {
                events.Add("send:" + output);
            });

        AssertSequence(new string[] { "prepare:肥", "post" }, events.ToArray());
        AssertEqual(1, postedActions.Count);

        postedActions[0]();

        AssertSequence(new string[] { "prepare:肥", "post", "send:肥米" }, events.ToArray());
    }

    private static void TestDeferredTextOutputDispatcherPreservesLabelUpdateOrder()
    {
        List<Action> postedActions = new List<Action>();
        List<string> events = new List<string>();
        DeferredTextOutputDispatcher dispatcher = new DeferredTextOutputDispatcher(
            delegate(Action action)
            {
                events.Add("post");
                postedActions.Add(action);
            });

        dispatcher.Queue("米", delegate(string output)
        {
            events.Add("send:" + output);
            events.Add("sp:" + output);
            events.Add("phone:" + output);
        });

        AssertSequence(new string[] { "post" }, events.ToArray());
        postedActions[0]();

        AssertSequence(new string[] { "post", "send:米", "sp:米", "phone:米" }, events.ToArray());
    }

    private static void TestOutputRouterPrefersUnicodeSendInputUnlessAppNeedsPaste()
    {
        List<string> shiftInsertApps = new List<string>() { "putty" };
        List<string> ctrlVApps = new List<string>() { "oxygennotincluded.exe" };
        List<string> big5Apps = new List<string>() { "zip32w" };

        AssertEqual((int)TextOutputMode.UnicodeSendInput, (int)TextOutputRouter.Select("DEFAULT", "notepad.exe", shiftInsertApps, ctrlVApps, big5Apps));
        AssertEqual((int)TextOutputMode.PasteShiftInsert, (int)TextOutputRouter.Select("DEFAULT", "putty.exe", shiftInsertApps, ctrlVApps, big5Apps));
        AssertEqual((int)TextOutputMode.PasteCtrlV, (int)TextOutputRouter.Select("DEFAULT", "oxygennotincluded.exe", shiftInsertApps, ctrlVApps, big5Apps));
        AssertEqual((int)TextOutputMode.PasteBig5, (int)TextOutputRouter.Select("BIG5", "notepad.exe", shiftInsertApps, ctrlVApps, big5Apps));
        AssertEqual((int)TextOutputMode.PasteShiftInsert, (int)TextOutputRouter.Select("PASTE", "notepad.exe", shiftInsertApps, ctrlVApps, big5Apps));
    }

    private static void TestOutputRouterSupportsManualTsfMode()
    {
        List<string> empty = new List<string>();

        AssertEqual((int)TextOutputMode.TsfBridge, (int)TextOutputRouter.Select("TSF", "notepad.exe", empty, empty, empty));
    }

    private static void TestOutputRouterMatchesAppNamesWithOptionalExeSuffix()
    {
        List<string> shiftInsertApps = new List<string>() { "rimworldwin64.exe" };
        List<string> ctrlVApps = new List<string>() { "oxygennotincluded.exe" };
        List<string> big5Apps = new List<string>() { "EWinner.exe" };

        AssertEqual((int)TextOutputMode.PasteCtrlV, (int)TextOutputRouter.Select("DEFAULT", "oxygennotincluded", shiftInsertApps, ctrlVApps, big5Apps));
        AssertEqual((int)TextOutputMode.PasteShiftInsert, (int)TextOutputRouter.Select("DEFAULT", "rimworldwin64", shiftInsertApps, ctrlVApps, big5Apps));
        AssertEqual((int)TextOutputMode.PasteBig5, (int)TextOutputRouter.Select("DEFAULT", "ewinner", shiftInsertApps, ctrlVApps, big5Apps));
    }

    private static void TestDefaultCompatibilityKeepsNotepadPlusPlusOnUnicodeSendInput()
    {
        List<string> empty = new List<string>();

        AssertEqual((int)TextOutputMode.UnicodeSendInput, (int)TextOutputRouter.Select("DEFAULT", "notepad++.exe", empty, TextOutputCompatibilityDefaults.PasteCtrlVApps, empty));
        AssertEqual((int)TextOutputMode.PasteShiftInsert, (int)TextOutputRouter.Select("PASTE", "notepad++.exe", empty, TextOutputCompatibilityDefaults.PasteCtrlVApps, empty));
    }

    private static void TestWindowMessageCharOutputPostsToFocusedControl()
    {
        FakeFocusedTextWindowGateway gateway = new FakeFocusedTextWindowGateway(new IntPtr(42));
        WindowMessageCharOutput output = new WindowMessageCharOutput(gateway);

        string error;
        bool ok = output.TrySendText("肥A", out error);

        AssertTrue(ok, "window message char output should report success");
        AssertEqual(null, error);
        AssertSequence(new string[] { "42:肥", "42:A" }, gateway.PostedMessages.ToArray());
    }

    private static void TestTsfBridgeProtocolEscapesJsonAndParsesOk()
    {
        string request = TsfBridgeProtocol.BuildCommitTextRequest("肥\"\\\n米");

        AssertEqual("{\"cmd\":\"commit_text\",\"text\":\"肥\\\"\\\\\\n米\"}\n", request);
        AssertTrue(TsfBridgeProtocol.IsOkResponse("{\"ok\":true,\"queued\":true}\n"), "ok true response should be accepted");
        AssertTrue(!TsfBridgeProtocol.IsOkResponse("{\"ok\":false,\"error\":\"COMMIT_FAILED\"}\n"), "ok false response should be rejected");
        AssertEqual("COMMIT_FAILED", TsfBridgeProtocol.GetErrorCode("{\"ok\":false,\"error\":\"COMMIT_FAILED\"}\n"));
    }

    private static void TestTsfBridgeOutputTriesPidPipeBeforeGlobalPipe()
    {
        FakeTsfBridgePipeClientFactory factory = new FakeTsfBridgePipeClientFactory();
        factory.Responses["uclliu_tsf_bridge_123"] = "{\"ok\":false,\"error\":\"PIPE_ERROR\"}\n";
        factory.Responses["uclliu_tsf_bridge"] = "{\"ok\":true}\n";
        TsfBridgeOutput output = new TsfBridgeOutput(factory);

        string error;
        bool ok = output.TryCommitText("肥", 123, 80, out error);

        AssertTrue(ok, "global pipe fallback should succeed");
        AssertEqual(null, error);
        AssertSequence(new string[] { "uclliu_tsf_bridge_123", "uclliu_tsf_bridge" }, factory.CreatedPipes.ToArray());
        AssertEqual("{\"cmd\":\"commit_text\",\"text\":\"肥\"}\n", factory.LastRequest);
    }

    private static void TestTsfBridgeOutputDoesNotTryGlobalPipeAfterCommitFailure()
    {
        FakeTsfBridgePipeClientFactory factory = new FakeTsfBridgePipeClientFactory();
        factory.Responses["uclliu_tsf_bridge_123"] = "{\"ok\":false,\"error\":\"COMMIT_FAILED\"}\n";
        factory.Responses["uclliu_tsf_bridge"] = "{\"ok\":true}\n";
        TsfBridgeOutput output = new TsfBridgeOutput(factory);

        string error;
        bool ok = output.TryCommitText("肥", 123, 80, out error);

        AssertTrue(!ok, "commit failure should not fall through to global pipe");
        AssertContains(error, "COMMIT_FAILED");
        AssertSequence(new string[] { "uclliu_tsf_bridge_123" }, factory.CreatedPipes.ToArray());
    }

    private static void TestOutputRouterForcesPasteForPttBrowserTitles()
    {
        List<string> empty = new List<string>();

        AssertEqual((int)TextOutputMode.PasteCtrlV, (int)TextOutputRouter.Select("DEFAULT", new TextOutputContext("chrome", "批踢踢實業坊 - Google Chrome", false), empty, empty, empty));
        AssertEqual((int)TextOutputMode.PasteCtrlV, (int)TextOutputRouter.Select("DEFAULT", new TextOutputContext("msedge", "ws.ptt.cc - 個人 - Microsoft Edge", false), empty, empty, empty));
        AssertEqual((int)TextOutputMode.PasteCtrlV, (int)TextOutputRouter.Select("DEFAULT", new TextOutputContext("brave", "term.ptt.cc - Brave", false), empty, empty, empty));
        AssertEqual((int)TextOutputMode.UnicodeSendInput, (int)TextOutputRouter.Select("DEFAULT", new TextOutputContext("chrome", "一般網頁 - Google Chrome", false), empty, empty, empty));
    }

    private static void TestOutputRouterForcesPasteForWindows11Notepad()
    {
        List<string> empty = new List<string>();

        AssertEqual((int)TextOutputMode.PasteCtrlV, (int)TextOutputRouter.Select("DEFAULT", new TextOutputContext("notepad", "未命名 - 記事本", true), empty, empty, empty));
        AssertEqual((int)TextOutputMode.PasteCtrlV, (int)TextOutputRouter.Select("DEFAULT", new TextOutputContext("notepad.exe", "Untitled - Notepad", true), empty, empty, empty));
        AssertEqual((int)TextOutputMode.UnicodeSendInput, (int)TextOutputRouter.Select("DEFAULT", new TextOutputContext("notepad", "Untitled - Notepad", false), empty, empty, empty));
    }

    private static void TestTypingSoundVolumeClamp()
    {
        AssertEqual(0, TypingSoundVolume.Clamp(-10));
        AssertEqual(30, TypingSoundVolume.Normalize("bad", 30));
        AssertEqual(30, TypingSoundVolume.Normalize("", 30));
        AssertEqual(45, TypingSoundVolume.Normalize("45", 30));
        AssertEqual(100, TypingSoundVolume.Normalize("500", 30));
    }

    private static void TestTypingSoundKeyState()
    {
        TypingSoundKeyState state = new TypingSoundKeyState();

        AssertTrue(state.ShouldPlayKeyDown(65), "first A keydown should play");
        AssertTrue(!state.ShouldPlayKeyDown(65), "held A keydown should not play repeatedly");
        AssertTrue(state.ShouldPlayKeyDown(66), "different key should play");
        state.HandleKeyUp(66);
        AssertTrue(!state.ShouldPlayKeyDown(65), "A should still be treated as held until A keyup");
        state.HandleKeyUp(65);
        AssertTrue(state.ShouldPlayKeyDown(65), "A should play again after keyup");
    }

    private static void TestTypingSoundCatalogSpecialNames()
    {
        AssertEqual(13, TypingSoundCatalog.GetSpecialKeyCode("enter.wav").Value);
        AssertEqual(13, TypingSoundCatalog.GetSpecialKeyCode("return.WAV").Value);
        AssertEqual(46, TypingSoundCatalog.GetSpecialKeyCode("delete.wav").Value);
        AssertEqual(46, TypingSoundCatalog.GetSpecialKeyCode("del.wav").Value);
        AssertEqual(8, TypingSoundCatalog.GetSpecialKeyCode("backspace.wav").Value);
        AssertEqual(8, TypingSoundCatalog.GetSpecialKeyCode("bs.wav").Value);
        AssertEqual(32, TypingSoundCatalog.GetSpecialKeyCode("space.wav").Value);
        AssertEqual(32, TypingSoundCatalog.GetSpecialKeyCode("sp.wav").Value);
        AssertTrue(!TypingSoundCatalog.GetSpecialKeyCode("1.wav").HasValue, "numeric wav should be random typing sound");
    }

    private static void TestTypingSoundVolumeScalerPcm16()
    {
        byte[] pcm = new byte[8];
        WriteInt16(pcm, 0, 1000);
        WriteInt16(pcm, 2, -1000);
        WriteInt16(pcm, 4, short.MaxValue);
        WriteInt16(pcm, 6, short.MinValue);

        byte[] scaled = WavPcmVolumeScaler.ScalePcm16Data(pcm, 50);

        AssertEqual(500, ReadInt16(scaled, 0));
        AssertEqual(-500, ReadInt16(scaled, 2));
        AssertEqual(16384, ReadInt16(scaled, 4));
        AssertEqual(-16384, ReadInt16(scaled, 6));
    }

    private static void TestTypingSoundReusesLoadedHandleOnHotPlayback()
    {
        string dir = Path.Combine(Path.GetTempPath(), "uclliu-sound-tests-" + Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(dir);
        try
        {
            File.WriteAllBytes(Path.Combine(dir, "enter.wav"), BuildPcm16WavBytes());
            FakeTypingSoundPlaybackEngine engine = new FakeTypingSoundPlaybackEngine();
            TypingSoundPlayer player = new TypingSoundPlayer(dir, engine);

            player.PreviewForKey(13, 100);
            player.PlayForKey(13, 100);

            AssertEqual(1, engine.CreateCount);
            AssertEqual(2, engine.TotalPlayCount);
        }
        finally
        {
            Directory.Delete(dir, true);
        }
    }

    private static void TestTypingSoundParsesPcm16WavForIndependentWaveOutPlayback()
    {
        Pcm16WavData data;

        AssertTrue(Pcm16WavData.TryCreate(BuildPcm16WavBytes(), out data), "pcm16 wav should parse");
        AssertEqual(1, data.Channels);
        AssertEqual(8000, data.SamplesPerSecond);
        AssertEqual(16000, data.AverageBytesPerSecond);
        AssertEqual(2, data.BlockAlign);
        AssertEqual(16, data.BitsPerSample);
        AssertEqual(4, data.PcmData.Length);
    }

    private static void TestKeyboardHookSystemMessages()
    {
        AssertTrue(KeyboardHookMessage.IsKeyDown(0x0100), "WM_KEYDOWN should be keydown");
        AssertTrue(KeyboardHookMessage.IsKeyDown(0x0104), "WM_SYSKEYDOWN should be keydown");
        AssertTrue(KeyboardHookMessage.IsKeyUp(0x0101), "WM_KEYUP should be keyup");
        AssertTrue(KeyboardHookMessage.IsKeyUp(0x0105), "WM_SYSKEYUP should be keyup");
        AssertTrue(!KeyboardHookMessage.IsKeyDown(0x0101), "WM_KEYUP should not be keydown");
    }

    private static void TestShiftReleaseClearsStateWithCtrlSpace()
    {
        ShiftKeyReleaseDecision decision = KeyboardHookStateRules.EvaluateShiftRelease(true, false);

        AssertTrue(decision.ShouldClearShiftState, "shift keyup should always clear shift state");
        AssertTrue(!decision.ShouldToggleInputMode, "CTRL+SPACE mode should not toggle input on Shift keyup");
    }

    private static void TestShiftReleaseToggleRules()
    {
        ShiftKeyReleaseDecision standaloneShift = KeyboardHookStateRules.EvaluateShiftRelease(false, false, 120);
        ShiftKeyReleaseDecision shiftWithOtherKey = KeyboardHookStateRules.EvaluateShiftRelease(false, true, 120);

        AssertTrue(standaloneShift.ShouldClearShiftState, "standalone shift release should clear state");
        AssertTrue(standaloneShift.ShouldToggleInputMode, "standalone shift release should toggle input when CTRL+SPACE is disabled");
        AssertTrue(shiftWithOtherKey.ShouldClearShiftState, "shift release after another key should clear state");
        AssertTrue(!shiftWithOtherKey.ShouldToggleInputMode, "shift release after another key should not toggle input");
    }

    private static void TestShiftReleaseIgnoresStaleStandaloneShift()
    {
        ShiftKeyReleaseDecision staleShift = KeyboardHookStateRules.EvaluateShiftRelease(false, false, KeyboardHookStateRules.MaxStandaloneShiftToggleMilliseconds + 1);

        AssertTrue(staleShift.ShouldClearShiftState, "stale shift release should still clear state");
        AssertTrue(!staleShift.ShouldToggleInputMode, "stale standalone shift should not toggle input under high load");
    }

    private static void TestPinyiV001SkipsPhoneCodeAndBopomofo()
    {
        string[] lines = new string[]
        {
            "VERSION_0.01",
            ", - . / 0 1 u",
            "ㄝ ㄦ ㄡ ㄥ ㄢ ㄅ ㄧ",
            "u ㄧ 一 壹 衣",
            "u4 意 義 一 ㄧ",
            "0 安 鞍 ㄢ"
        };

        List<string> candidates = PinyiCandidateSelector.FindCandidates(lines, "一");

        AssertSequence(new string[] { "一", "壹", "衣", "意", "義" }, candidates.ToArray());
        AssertTrue(!candidates.Contains("u"), "phonetic code should not become candidate");
        AssertTrue(!candidates.Contains("ㄧ"), "bopomofo token should not become candidate");
    }

    private static void TestPinyiV001SortsByClosestTokenIndex()
    {
        string[] lines = new string[]
        {
            "VERSION_0.01",
            ", - . / 0 1 u",
            "ㄝ ㄦ ㄡ ㄥ ㄢ ㄅ ㄧ",
            "x 甲 乙 同",
            "y 同 丙 丁"
        };

        List<string> candidates = PinyiCandidateSelector.FindCandidates(lines, "同");

        AssertSequence(new string[] { "同", "丙", "丁", "甲", "乙" }, candidates.ToArray());
    }

    private static void TestPinyiLegacyKeepsWholeMatchingLines()
    {
        string[] lines = new string[]
        {
            "安 鞍 庵",
            "案 安 岸",
            "同 童 銅"
        };

        List<string> candidates = PinyiCandidateSelector.FindCandidates(lines, "安");

        AssertSequence(new string[] { "安", "鞍", "庵", "案", "岸" }, candidates.ToArray());
    }

    private static void TestPhoneTableConvertsZhuyinQueryToCandidates()
    {
        PhoneCodeTable table = PhoneCodeTable.Parse(new string[]
        {
            "VERSION_0.01",
            ", - . / 0 1 2 3 4 5 6 7 8 9 ; a b c d e f g h i j k l m n o p q r s t u v w x y z",
            "ㄝ ㄦ ㄡ ㄥ ㄢ ㄅ ㄉ ˇ ˋ ㄓ ˊ ˙ ㄚ ㄞ ㄤ ㄇ ㄖ ㄏ ㄎ ㄍ ㄑ ㄕ ㄘ ㄛ ㄨ ㄜ ㄠ ㄩ ㄙ ㄟ ㄣ ㄆ ㄐ ㄋ ㄔ ㄧ ㄒ ㄊ ㄌ ㄗ ㄈ",
            "zo 非 飛 菲",
            "zo6 肥 淝 腓",
            "u ㄧ 一 壹 衣"
        });

        AssertEqual("ㄈㄟˊ", table.KeyboardToPhoneCode("zo6"));
        AssertEqual("zo6", table.PhoneCodeToKeyboard("ㄈㄟˊ"));
        AssertSequence(new string[] { "肥", "淝", "腓" }, table.FindCandidatesByPhoneCode("ㄈㄟˊ").ToArray());
        AssertSequence(new string[] { "一", "壹", "衣" }, table.FindCandidatesByPhoneCode("ㄧ").ToArray());
    }

    private static void TestPhoneTableMapsWordsBackToZhuyinLabels()
    {
        PhoneCodeTable table = PhoneCodeTable.Parse(new string[]
        {
            "VERSION_0.01",
            ", - . / 0 1 2 3 4 5 6 7 8 9 ; a b c d e f g h i j k l m n o p q r s t u v w x y z",
            "ㄝ ㄦ ㄡ ㄥ ㄢ ㄅ ㄉ ˇ ˋ ㄓ ˊ ˙ ㄚ ㄞ ㄤ ㄇ ㄖ ㄏ ㄎ ㄍ ㄑ ㄕ ㄘ ㄛ ㄨ ㄜ ㄠ ㄩ ㄙ ㄟ ㄣ ㄆ ㄐ ㄋ ㄔ ㄧ ㄒ ㄊ ㄌ ㄗ ㄈ",
            "zo 非 飛 菲",
            "zo6 肥 菲 腓"
        });

        AssertSequence(new string[] { "ㄈㄟˊ" }, table.GetPhonesForWord("肥").ToArray());
        AssertSequence(new string[] { "ㄈㄟ", "ㄈㄟˊ" }, table.GetPhonesForWord("菲").ToArray());
    }

    private static byte[] BuildUnitab(string firstTwoKeys, int key3, int key4, int unicodeCodePoint)
    {
        ushort[] keyTable = new ushort[1024];
        int key1 = KeyIndex(firstTwoKeys[0]);
        int key2 = KeyIndex(firstTwoKeys[1]);
        int tableIndex = key1 * 32 + key2;

        for (int i = tableIndex + 1; i < keyTable.Length; i++)
        {
            keyTable[i] = 1;
        }

        byte[] highBytes = new byte[10];
        int high = (unicodeCodePoint >> 14) & 0x03;
        if ((high & 0x02) != 0)
        {
            highBytes[2] |= 0x80;
        }
        if ((high & 0x01) != 0)
        {
            highBytes[2] |= 0x40;
        }

        int low = unicodeCodePoint & 0x3FFF;
        int record = ((key3 & 0x1F) << 19) | ((key4 & 0x1F) << 14) | low;

        using (MemoryStream ms = new MemoryStream())
        {
            for (int i = 0; i < keyTable.Length; i++)
            {
                ms.WriteByte((byte)(keyTable[i] & 0xFF));
                ms.WriteByte((byte)((keyTable[i] >> 8) & 0xFF));
            }

            ms.Write(highBytes, 0, highBytes.Length);
            ms.WriteByte(0);
            ms.WriteByte(0);
            ms.WriteByte((byte)((record >> 16) & 0xFF));
            ms.WriteByte((byte)((record >> 8) & 0xFF));
            ms.WriteByte((byte)(record & 0xFF));
            return ms.ToArray();
        }
    }

    private static int KeyIndex(char key)
    {
        const string keys = " abcdefghijklmnopqrstuvwxyz,.'[]";
        int index = keys.IndexOf(key);
        if (index < 0)
        {
            throw new ArgumentException("Unknown key: " + key);
        }
        return index;
    }

    private static byte[] BuildPcm16WavBytes()
    {
        byte[] wav = new byte[48];
        WriteAscii(wav, 0, "RIFF");
        WriteInt32(wav, 4, 40);
        WriteAscii(wav, 8, "WAVE");
        WriteAscii(wav, 12, "fmt ");
        WriteInt32(wav, 16, 16);
        WriteInt16(wav, 20, 1);
        WriteInt16(wav, 22, 1);
        WriteInt32(wav, 24, 8000);
        WriteInt32(wav, 28, 16000);
        WriteInt16(wav, 32, 2);
        WriteInt16(wav, 34, 16);
        WriteAscii(wav, 36, "data");
        WriteInt32(wav, 40, 4);
        WriteInt16(wav, 44, 1000);
        WriteInt16(wav, 46, -1000);
        return wav;
    }

    private static void WriteAscii(byte[] buffer, int offset, string value)
    {
        byte[] data = Encoding.ASCII.GetBytes(value);
        Buffer.BlockCopy(data, 0, buffer, offset, data.Length);
    }

    private static void WriteInt32(byte[] buffer, int offset, int value)
    {
        byte[] data = BitConverter.GetBytes(value);
        buffer[offset] = data[0];
        buffer[offset + 1] = data[1];
        buffer[offset + 2] = data[2];
        buffer[offset + 3] = data[3];
    }

    private static void AssertContains(string haystack, string needle)
    {
        if (haystack.IndexOf(needle, StringComparison.Ordinal) < 0)
        {
            throw new Exception("Expected to find '" + needle + "' in: " + haystack);
        }
    }

    private static void AssertTrue(bool condition, string message)
    {
        if (!condition)
        {
            throw new Exception(message);
        }
    }

    private static void AssertEqual(int expected, int actual)
    {
        if (expected != actual)
        {
            throw new Exception("Expected " + expected + ", got " + actual);
        }
    }

    private static void AssertEqual(string expected, string actual)
    {
        if (expected != actual)
        {
            throw new Exception("Expected " + expected + ", got " + actual);
        }
    }

    private static void AssertEqual(IntPtr expected, IntPtr actual)
    {
        if (expected != actual)
        {
            throw new Exception("Expected " + expected + ", got " + actual);
        }
    }

    private static void AssertKeyboardInput(UnicodeSendInputOutput.INPUT input, char expectedChar, bool isKeyUp)
    {
        AssertEqual(UnicodeSendInputOutput.InputKeyboard, input.type);
        AssertEqual(0, input.u.ki.wVk);
        AssertEqual((int)expectedChar, input.u.ki.wScan);

        uint expectedFlags = UnicodeSendInputOutput.KeyEventUnicode;
        if (isKeyUp)
        {
            expectedFlags |= UnicodeSendInputOutput.KeyEventKeyUp;
        }
        AssertEqual((int)expectedFlags, (int)input.u.ki.dwFlags);
    }

    private static void WriteInt16(byte[] buffer, int offset, short value)
    {
        byte[] data = BitConverter.GetBytes(value);
        buffer[offset] = data[0];
        buffer[offset + 1] = data[1];
    }

    private static short ReadInt16(byte[] buffer, int offset)
    {
        return BitConverter.ToInt16(buffer, offset);
    }

    private static void AssertSequence(string[] expected, string[] actual)
    {
        if (expected.Length != actual.Length)
        {
            throw new Exception("Expected length " + expected.Length + ", got " + actual.Length);
        }
        for (int i = 0; i < expected.Length; i++)
        {
            if (expected[i] != actual[i])
            {
                throw new Exception("Expected [" + i + "] " + expected[i] + ", got " + actual[i]);
            }
        }
    }

    private sealed class FakeClipboardGateway : IClipboardGateway
    {
        public FakeClipboardGateway(string text)
        {
            Text = text;
        }

        public string Text { get; private set; }
        public bool ThrowOnSetText { get; set; }

        public object GetDataObject()
        {
            return Text;
        }

        public bool ContainsText()
        {
            return Text != null;
        }

        public string GetText()
        {
            return Text;
        }

        public void SetText(string text, ClipboardTextKind textKind)
        {
            if (ThrowOnSetText)
            {
                throw new InvalidOperationException("fake clipboard failure");
            }
            Text = text;
        }

        public void Clear()
        {
            Text = null;
        }

        public void SetDataObject(object dataObject)
        {
            Text = dataObject as string;
        }
    }

    private sealed class FakeKeySender : IKeySender
    {
        public bool ThrowOnSend { get; set; }
        public int SendCount { get; private set; }
        public Action<string> OnSend { get; set; }

        public void SendWait(string keys)
        {
            SendCount++;
            if (ThrowOnSend)
            {
                throw new InvalidOperationException("fake send failure");
            }
            if (OnSend != null)
            {
                OnSend(keys);
            }
        }
    }

    private sealed class FakeUnicodeInputSender : IUnicodeInputSender
    {
        public readonly List<UnicodeSendInputOutput.INPUT[]> Batches = new List<UnicodeSendInputOutput.INPUT[]>();
        public uint NextResult = 0;
        public int LastError = 0;

        public uint Send(UnicodeSendInputOutput.INPUT[] inputs)
        {
            Batches.Add(inputs);
            return NextResult == 0 ? (uint)inputs.Length : NextResult;
        }

        public int GetLastError()
        {
            return LastError;
        }

        public string BatchText(int index)
        {
            UnicodeSendInputOutput.INPUT[] inputs = Batches[index];
            StringBuilder text = new StringBuilder();
            for (int i = 0; i < inputs.Length; i += 2)
            {
                text.Append((char)inputs[i].u.ki.wScan);
            }
            return text.ToString();
        }
    }

    private sealed class FakeFocusedTextWindowGateway : IFocusedTextWindowGateway
    {
        private readonly IntPtr focusedWindow;
        public readonly List<string> PostedMessages = new List<string>();

        public FakeFocusedTextWindowGateway(IntPtr focusedWindow)
        {
            this.focusedWindow = focusedWindow;
        }

        public IntPtr GetFocusedWindow()
        {
            return focusedWindow;
        }

        public bool PostChar(IntPtr windowHandle, char textChar)
        {
            PostedMessages.Add(windowHandle.ToInt64().ToString() + ":" + textChar);
            return true;
        }
    }

    private sealed class FakeTsfBridgePipeClientFactory : ITsfBridgePipeClientFactory
    {
        public readonly Dictionary<string, string> Responses = new Dictionary<string, string>(StringComparer.Ordinal);
        public readonly List<string> CreatedPipes = new List<string>();
        public string LastRequest;

        public ITsfBridgePipeClient Create(string pipeName)
        {
            CreatedPipes.Add(pipeName);
            string response;
            if (!Responses.TryGetValue(pipeName, out response))
            {
                response = "{\"ok\":false,\"error\":\"PIPE_ERROR\"}\n";
            }
            return new FakeTsfBridgePipeClient(this, response);
        }

        private sealed class FakeTsfBridgePipeClient : ITsfBridgePipeClient
        {
            private readonly FakeTsfBridgePipeClientFactory owner;
            private readonly string response;

            public FakeTsfBridgePipeClient(FakeTsfBridgePipeClientFactory owner, string response)
            {
                this.owner = owner;
                this.response = response;
            }

            public void Connect(int timeoutMilliseconds)
            {
            }

            public void WriteRequest(string request)
            {
                owner.LastRequest = request;
            }

            public string ReadResponse()
            {
                return response;
            }

            public void Dispose()
            {
            }
        }
    }

    private sealed class FakeSelectedTextTransformCommand : ISelectedTextTransformCommand
    {
        private readonly string selectedText;

        public FakeSelectedTextTransformCommand(string selectedText)
        {
            this.selectedText = selectedText;
        }

        public int RunCount { get; private set; }

        public bool TryRun(Func<string, string> transform, Action<string> sendOutput, out string error)
        {
            RunCount++;
            sendOutput(transform(selectedText));
            error = null;
            return true;
        }
    }

    private sealed class FakeTypingSoundPlaybackEngine : ITypingSoundPlaybackEngine
    {
        private readonly List<FakeTypingSoundHandle> handles = new List<FakeTypingSoundHandle>();

        public int CreateCount { get; private set; }

        public int TotalPlayCount
        {
            get
            {
                int count = 0;
                for (int i = 0; i < handles.Count; i++)
                {
                    count += handles[i].PlayCount;
                }
                return count;
            }
        }

        public ITypingSoundHandle Create(byte[] wavData)
        {
            CreateCount++;
            FakeTypingSoundHandle handle = new FakeTypingSoundHandle();
            handles.Add(handle);
            return handle;
        }
    }

    private sealed class FakeTypingSoundHandle : ITypingSoundHandle
    {
        public int PlayCount { get; private set; }
        public bool IsDisposed { get; private set; }

        public void Play()
        {
            PlayCount++;
        }

        public void Dispose()
        {
            IsDisposed = true;
        }
    }
}
