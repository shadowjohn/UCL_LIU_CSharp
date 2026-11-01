using System;
using System.Collections.Generic;
using uclliu;

internal static class JapaneseAliasCompatibilityTests
{
    public static void TestJsonPreservesStandardJapaneseRoots()
    {
        Dictionary<string, List<string>> table = LiuJsonTable.ParseChardefsJson(
            "{\"chardefs\":{\"i,\":[\"い\",\"ぃ\",\"ゐ\"],\"ji,\":[\"じ\",\"ぢ\"],"
            + "\"i.\":[\"イ\",\"ィ\",\"ヰ\"],\"ji.\":[\"ジ\",\"ヂ\"]}}");

        AssertValues(table, "i,", "い", "ぃ", "ゐ");
        AssertValues(table, "ji,", "じ", "ぢ");
        AssertValues(table, "i.", "イ", "ィ", "ヰ");
        AssertValues(table, "ji.", "ジ", "ヂ");
        AssertRootCount(table, 4);
    }

    public static void TestJsonDoesNotInferLegacyPrefixFromRomanizedJa()
    {
        Dictionary<string, List<string>> table = LiuJsonTable.ParseChardefsJson(
            "{\"chardefs\":{\"a,\":[\"あ\",\"ぁ\"],\"ja,\":[\"じゃ\"],"
            + "\"a.\":[\"ア\",\"ァ\"],\"ja.\":[\"ジャ\"],\"jka,\":[\"仮\"]}}");

        AssertValues(table, "a,", "あ", "ぁ");
        AssertValues(table, "a.", "ア", "ァ");
        AssertMissing(table, "ka,");
        AssertRootCount(table, 5);
    }

    public static void TestJsonLegacyAliasesRemainStableAfterReload()
    {
        Dictionary<string, List<string>> table = LiuJsonTable.ParseChardefsJson(
            "{\"chardefs\":{\"a,\":[\"\uF6F8\"],\"ja,\":[\"あ\",\"ぁ\"],"
            + "\"i,\":[\"\uF6FA\"],\"ji,\":[\"い\",\"ぃ\"],\"jji,\":[\"じ\",\"ぢ\"],"
            + "\"ka,\":[\"\uF701\"],\"jka,\":[\"か\"]}}");

        AssertValues(table, "a,", "あ", "ぁ");
        AssertValues(table, "i,", "い", "ぃ");
        AssertValues(table, "ji,", "じ", "ぢ");
        AssertValues(table, "ka,", "か");

        Dictionary<string, List<string>> reloaded = LiuJsonTable.ParseChardefsRoot(
            new Dictionary<string, object> { { "chardefs", table } });
        AssertValues(reloaded, "i,", "い", "ぃ");
        AssertValues(reloaded, "ji,", "じ", "ぢ");
    }

    public static void TestJsonDetectsLegacyNamespacePerScript()
    {
        Dictionary<string, List<string>> table = LiuJsonTable.ParseChardefsJson(
            "{\"chardefs\":{\"ja,\":[\"あ\"],\"jka,\":[\"か\"],"
            + "\"i.\":[\"イ\",\"ィ\"],\"ji.\":[\"ジ\",\"ヂ\"]}}");

        AssertValues(table, "a,", "あ");
        AssertValues(table, "ka,", "か");
        AssertValues(table, "i.", "イ", "ィ");
        AssertValues(table, "ji.", "ジ", "ヂ");
        AssertRootCount(table, 6);
    }

    public static void TestFcitxPreservesStandardKanaAndSymbolRoots()
    {
        Dictionary<string, List<string>> table = ParseFcitxWithoutLoaderAliases(
            "[數據]\na, あ\na, ぁ\nka, か\ni, い ぃ ゐ\nji, じ ぢ\n"
            + "a. ア ァ\nka. カ\ni. イ ィ ヰ\nji. ジ ヂ\n, ，\n. 。\n,a α\nc. ！\n");

        AssertValues(table, "a,", "あ", "ぁ");
        AssertValues(table, "ka,", "か");
        AssertValues(table, "i,", "い", "ぃ", "ゐ");
        AssertValues(table, "ji,", "じ", "ぢ");
        AssertValues(table, "a.", "ア", "ァ");
        AssertValues(table, "ka.", "カ");
        AssertValues(table, "i.", "イ", "ィ", "ヰ");
        AssertValues(table, "ji.", "ジ", "ヂ");
        AssertValues(table, ",", "，");
        AssertValues(table, ".", "。");
        AssertValues(table, ",a", "α");
        AssertValues(table, "c.", "！");
        AssertRootCount(table, 12);
    }

    public static void TestLegacyFcitxPreservesUnrelatedSymbolRoots()
    {
        string cin = LiuTableConverter.ConvertFcitxBoshiamyTextToCinText(
            "[數據]\na, \uF6F8\na. \uF78B\nja, あ ぁ\nja. ア ァ\n"
            + "ji, い ぃ\njji, じ ぢ\njka, か\njka. カ\n"
            + ", ，\n. 。\n,a α\nc. ！\n");
        string json = LiuTableConverter.ConvertCinTextToJson(cin);
        Dictionary<string, List<string>> raw = ParseWithoutLoaderAliases(json);

        AssertValues(raw, "a,", "あ", "ぁ");
        AssertValues(raw, "a.", "ア", "ァ");
        AssertValues(raw, "i,", "い", "ぃ");
        AssertValues(raw, "ji,", "じ", "ぢ");
        AssertValues(raw, "ka,", "か");
        AssertValues(raw, "ka.", "カ");
        AssertValues(raw, "ja,", "あ", "ぁ");
        AssertValues(raw, "jka,", "か");
        AssertValues(raw, ",", "，");
        AssertValues(raw, ".", "。");
        AssertValues(raw, ",a", "α");
        AssertValues(raw, "c.", "！");
        AssertValues(LiuJsonTable.ParseChardefsJson(json), "i,", "い", "ぃ");
    }

    public static void TestFcitxDetectsLegacyNamespacePerScript()
    {
        Dictionary<string, List<string>> table = ParseFcitxWithoutLoaderAliases(
            "[數據]\na. \uF78B\nja. ア\njka. カ\n"
            + "i, い ぃ\nji, じ ぢ\nja, じゃ\n");

        AssertValues(table, "a.", "ア");
        AssertValues(table, "ka.", "カ");
        AssertValues(table, "i,", "い", "ぃ");
        AssertValues(table, "ji,", "じ", "ぢ");
        AssertMissing(table, "a,");
        AssertRootCount(table, 7);
    }

    private static Dictionary<string, List<string>> ParseFcitxWithoutLoaderAliases(string fcitx)
    {
        return ParseWithoutLoaderAliases(LiuTableConverter.ConvertCinTextToJson(
            LiuTableConverter.ConvertFcitxBoshiamyTextToCinText(fcitx)));
    }

    private static Dictionary<string, List<string>> ParseWithoutLoaderAliases(string json)
    {
        Dictionary<string, object> root = (Dictionary<string, object>)LiuJsonTable.DecodeJson(json);
        return LiuJsonTable.ParseStringListDictionary(root["chardefs"]);
    }

    private static void AssertValues(Dictionary<string, List<string>> table, string root, params string[] expected)
    {
        List<string> actual;
        if (!table.TryGetValue(root, out actual)) throw new Exception("missing root " + root);
        if (actual.Count != expected.Length)
        {
            throw new Exception(root + ": expected [" + String.Join("|", expected)
                + "], got [" + String.Join("|", actual.ToArray()) + "]");
        }
        for (int i = 0; i < expected.Length; i++)
        {
            if (actual[i] != expected[i])
            {
                throw new Exception(root + ": expected [" + String.Join("|", expected)
                    + "], got [" + String.Join("|", actual.ToArray()) + "]");
            }
        }
    }

    private static void AssertMissing(Dictionary<string, List<string>> table, string root)
    {
        if (table.ContainsKey(root)) throw new Exception("unexpected alias " + root);
    }

    private static void AssertRootCount(Dictionary<string, List<string>> table, int expected)
    {
        if (table.Count != expected) throw new Exception("expected " + expected + " roots, got " + table.Count);
    }
}
