namespace uclliu
{
    public static class UclLiuAppInfo
    {
        public const string Version = "0.14";
        public const string AboutTitle = "UCLLIU 肥米輸入法 C# 版";
        public const string ProductName = "UCLLIU Input Method";
        public const string FileDescription = "Fastest Chinese Input Method";
        public const string Author = "羽山秋人";
        public const string AuthorAlias = "FeatherMountain";
        public const string Website = "https://3wa.tw";
        public const string CompanyName = "Magic 3WA The legend of trainer (https://3wa.tw)";
        public const string Copyright = "Copyright (c) MIT 3WA Studio (https://3wa.tw)";
        public const string Comments = "Author: FeatherMountain (https://3wa.tw)";

        public static string BuildAboutText()
        {
            string msg = AboutTitle + "\n\n";
            msg += "作者：" + Author + " (" + Website + ")\n";
            msg += "版本：" + Version + "\n";
            msg += "\n熱鍵提示：\n\n";
            msg += "「,,,VERSION」目前版本\n";
            msg += "「'ucl」同音字查詢\n";
            msg += "「';zo6」注音查詢\n";
            msg += "「,,,UNLOCK」回到正常模式\n";
            msg += "「,,,LOCK」進入遊戲模式\n";
            msg += "「,,,C」簡體模式\n";
            msg += "「,,,T」繁體模式\n";
            msg += "「,,,S」UI變窄\n";
            msg += "「,,,L」UI變寬\n";
            msg += "「,,,+」UI變大\n";
            msg += "「,,,-」UI變小\n";
            msg += "「,,,X」框字的字根轉回文字\n";
            msg += "「,,,Z」框字的文字變成字根\n";
            msg += "「,,,BOX」開啟自定詞庫\n";
            return msg;
        }
    }
}
