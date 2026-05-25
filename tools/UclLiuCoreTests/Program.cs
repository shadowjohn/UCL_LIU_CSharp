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
        failed += Run("short mode width is bounded and proportional", TestShortModeWidth);
        failed += Run("custom root validation matches UCL rules", TestCustomRootValidation);
        failed += Run("simple ini reads default section values", TestSimpleIniReadsDefaultSectionValues);
        failed += Run("simple ini writes readable sections", TestSimpleIniWritesReadableSections);
        failed += Run("liu json parser decodes chardefs without System.Json", TestLiuJsonParserDecodesChardefs);
        failed += Run("custom dictionary lowercases and merges values", TestCustomDictionaryMerge);
        failed += Run("custom dictionary save writes deterministic json", TestCustomDictionarySave);
        failed += Run("unicode sendinput builds literal down/up events", TestUnicodeSendInputBuildsLiteralEvents);
        failed += Run("unicode sendinput preserves surrogate pairs", TestUnicodeSendInputPreservesSurrogatePairs);
        failed += Run("clipboard paste restores original text after send failure", TestClipboardPasteRestoresOriginalTextAfterSendFailure);
        failed += Run("clipboard paste reports set clipboard failure before send", TestClipboardPasteReportsSetClipboardFailureBeforeSend);
        failed += Run("output router prefers unicode sendinput unless app needs paste", TestOutputRouterPrefersUnicodeSendInputUnlessAppNeedsPaste);
        failed += Run("output router matches app names with optional exe suffix", TestOutputRouterMatchesAppNamesWithOptionalExeSuffix);
        failed += Run("output router forces paste for PTT browser titles", TestOutputRouterForcesPasteForPttBrowserTitles);
        failed += Run("output router forces paste for Windows 11 Notepad", TestOutputRouterForcesPasteForWindows11Notepad);
        failed += Run("typing sound volume clamps to supported range", TestTypingSoundVolumeClamp);
        failed += Run("typing sound suppresses repeated keydown until keyup", TestTypingSoundKeyState);
        failed += Run("typing sound catalog maps special wav names", TestTypingSoundCatalogSpecialNames);
        failed += Run("typing sound volume scaler adjusts pcm16 samples", TestTypingSoundVolumeScalerPcm16);
        failed += Run("keyboard hook treats system key messages as key transitions", TestKeyboardHookSystemMessages);
        failed += Run("shift release clears state even when ctrl space is enabled", TestShiftReleaseClearsStateWithCtrlSpace);
        failed += Run("shift release toggles input only for standalone shift mode", TestShiftReleaseToggleRules);
        failed += Run("pinyi v001 same sound skips phonetic code and bopomofo tokens", TestPinyiV001SkipsPhoneCodeAndBopomofo);
        failed += Run("pinyi v001 same sound sorts by closest token index", TestPinyiV001SortsByClosestTokenIndex);
        failed += Run("pinyi legacy same sound keeps whole matching lines", TestPinyiLegacyKeepsWholeMatchingLines);

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
            + "版本：0.11\n\n"
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

        AssertEqual("0.11", UclLiuAppInfo.Version);
        AssertEqual("UCLLIU 肥米輸入法 C# 版", UclLiuAppInfo.ProductName);
        AssertEqual("3WA Studio (https://3wa.tw)", UclLiuAppInfo.CompanyName);
        AssertEqual("Copyright (c) 2019-2026 羽山秋人 (https://3wa.tw)", UclLiuAppInfo.Copyright);
        AssertEqual(expected, UclLiuAppInfo.BuildAboutText());
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

    private static void TestOutputRouterMatchesAppNamesWithOptionalExeSuffix()
    {
        List<string> shiftInsertApps = new List<string>() { "rimworldwin64.exe" };
        List<string> ctrlVApps = new List<string>() { "oxygennotincluded.exe" };
        List<string> big5Apps = new List<string>() { "EWinner.exe" };

        AssertEqual((int)TextOutputMode.PasteCtrlV, (int)TextOutputRouter.Select("DEFAULT", "oxygennotincluded", shiftInsertApps, ctrlVApps, big5Apps));
        AssertEqual((int)TextOutputMode.PasteShiftInsert, (int)TextOutputRouter.Select("DEFAULT", "rimworldwin64", shiftInsertApps, ctrlVApps, big5Apps));
        AssertEqual((int)TextOutputMode.PasteBig5, (int)TextOutputRouter.Select("DEFAULT", "ewinner", shiftInsertApps, ctrlVApps, big5Apps));
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
        ShiftKeyReleaseDecision standaloneShift = KeyboardHookStateRules.EvaluateShiftRelease(false, false);
        ShiftKeyReleaseDecision shiftWithOtherKey = KeyboardHookStateRules.EvaluateShiftRelease(false, true);

        AssertTrue(standaloneShift.ShouldClearShiftState, "standalone shift release should clear state");
        AssertTrue(standaloneShift.ShouldToggleInputMode, "standalone shift release should toggle input when CTRL+SPACE is disabled");
        AssertTrue(shiftWithOtherKey.ShouldClearShiftState, "shift release after another key should clear state");
        AssertTrue(!shiftWithOtherKey.ShouldToggleInputMode, "shift release after another key should not toggle input");
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

        public void SendWait(string keys)
        {
            SendCount++;
            if (ThrowOnSend)
            {
                throw new InvalidOperationException("fake send failure");
            }
        }
    }
}
