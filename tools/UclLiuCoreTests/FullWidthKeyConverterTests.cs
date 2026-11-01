using System;
using uclliu;

internal static class FullWidthKeyConverterTests
{
    public static void TestSpace()
    {
        AssertForAllModifiers(32, "　", "　");
    }

    public static void TestLetters()
    {
        const string lower = "ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚ";
        const string upper = "ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ";

        for (int i = 0; i < lower.Length; i++)
        {
            AssertConverted(65 + i, false, false, lower[i].ToString());
            AssertConverted(65 + i, true, false, upper[i].ToString());
            AssertConverted(65 + i, false, true, upper[i].ToString());
            AssertConverted(65 + i, true, true, lower[i].ToString());
        }
    }

    public static void TestDigits()
    {
        const string unshifted = "０１２３４５６７８９";
        const string shifted = "）！＠＃＄％＾＆＊（";

        for (int i = 0; i < unshifted.Length; i++)
        {
            AssertForAllModifiers(48 + i, unshifted[i].ToString(), shifted[i].ToString());
        }
    }

    public static void TestSymbols()
    {
        int[] keys = { 186, 187, 188, 189, 190, 191, 192, 219, 220, 221, 222 };
        string[] unshifted = { "；", "＝", "，", "－", "．", "／", "｀", "［", "＼", "］", "＇" };
        string[] shifted = { "：", "＋", "＜", "＿", "＞", "？", "～", "｛", "｜", "｝", "＂" };

        for (int i = 0; i < keys.Length; i++)
        {
            AssertForAllModifiers(keys[i], unshifted[i], shifted[i]);
        }
    }

    public static void TestNumpad()
    {
        const string digits = "０１２３４５６７８９";
        for (int i = 0; i < digits.Length; i++)
        {
            AssertForAllModifiers(96 + i, digits[i].ToString(), digits[i].ToString());
        }

        AssertForAllModifiers(106, "＊", "＊");
        AssertForAllModifiers(107, "＋", "＋");
        AssertForAllModifiers(109, "－", "－");
        AssertForAllModifiers(110, "．", "．");
        AssertForAllModifiers(111, "／", "／");
    }

    public static void TestNonPrintableKeys()
    {
        int[] keys =
        {
            Int32.MinValue, -1, 0, 8, 9, 12, 13, 16, 17, 18, 19, 20, 27,
            31, 33, 34, 35, 36, 37, 38, 39, 40, 44, 45, 46, 47,
            58, 64, 91, 92, 93, 95, 108, 124, 135, 136, 144, 145,
            160, 161, 162, 163, 164, 165, 185, 193, 218, 223, 255, 256, Int32.MaxValue
        };

        foreach (int key in keys)
        {
            AssertNotConvertedForAllModifiers(key);
        }
        for (int functionKey = 112; functionKey <= 135; functionKey++)
        {
            AssertNotConvertedForAllModifiers(functionKey);
        }
    }

    private static void AssertForAllModifiers(int virtualKey, string unshifted, string shifted)
    {
        AssertConverted(virtualKey, false, false, unshifted);
        AssertConverted(virtualKey, true, false, shifted);
        AssertConverted(virtualKey, false, true, unshifted);
        AssertConverted(virtualKey, true, true, shifted);
    }

    private static void AssertConverted(int virtualKey, bool shiftDown, bool capsLock, string expected)
    {
        string actual;
        bool converted = FullWidthKeyConverter.TryConvert(virtualKey, shiftDown, capsLock, out actual);
        if (!converted || actual != expected)
        {
            throw new Exception(DescribeKey(virtualKey, shiftDown, capsLock)
                + " expected '" + expected + "', got converted=" + converted + ", text='" + actual + "'");
        }
    }

    private static void AssertNotConvertedForAllModifiers(int virtualKey)
    {
        AssertNotConverted(virtualKey, false, false);
        AssertNotConverted(virtualKey, true, false);
        AssertNotConverted(virtualKey, false, true);
        AssertNotConverted(virtualKey, true, true);
    }

    private static void AssertNotConverted(int virtualKey, bool shiftDown, bool capsLock)
    {
        string actual = "previous text";
        bool converted = FullWidthKeyConverter.TryConvert(virtualKey, shiftDown, capsLock, out actual);
        if (converted || actual != null)
        {
            throw new Exception(DescribeKey(virtualKey, shiftDown, capsLock)
                + " should remain native with null text, got converted=" + converted + ", text='" + actual + "'");
        }
    }

    private static string DescribeKey(int virtualKey, bool shiftDown, bool capsLock)
    {
        return "VK=" + virtualKey + ", Shift=" + shiftDown + ", CapsLock=" + capsLock;
    }
}
