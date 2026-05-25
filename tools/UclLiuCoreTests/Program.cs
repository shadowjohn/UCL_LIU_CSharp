using System;
using System.Collections.Generic;
using System.IO;
using System.Json;
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
        failed += Run("short mode width is bounded and proportional", TestShortModeWidth);
        failed += Run("custom root validation matches UCL rules", TestCustomRootValidation);
        failed += Run("custom dictionary lowercases and merges values", TestCustomDictionaryMerge);
        failed += Run("custom dictionary save writes deterministic json", TestCustomDictionarySave);

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

    private static void TestCustomDictionaryMerge()
    {
        JsonValue root = JsonValue.Parse("{\"chardefs\":{\"ucl\":[\"肥\"],\"abc\":[\"一\"]}}");
        Dictionary<string, List<string>> custom = new Dictionary<string, List<string>>();
        custom["UCL"] = new List<string>() { "肥宅", "肥" };
        custom["new"] = new List<string>() { "新詞" };

        int merged = CustomDictionaryStore.MergeInto(root, custom);

        AssertEqual(2, merged);
        AssertSequence(new string[] { "肥", "肥宅" }, CustomDictionaryStore.JsonValueToStrings(root["chardefs"]["ucl"]).ToArray());
        AssertSequence(new string[] { "新詞" }, CustomDictionaryStore.JsonValueToStrings(root["chardefs"]["new"]).ToArray());
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
}
