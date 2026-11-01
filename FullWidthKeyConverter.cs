namespace uclliu
{
    public static class FullWidthKeyConverter
    {
        public static bool TryConvert(int virtualKey, bool shiftDown, bool capsLock, out string text)
        {
            text = null;

            if (virtualKey == 32)
            {
                text = "　";
                return true;
            }

            if (virtualKey >= 65 && virtualKey <= 90)
            {
                char firstLetter = shiftDown ^ capsLock ? 'Ａ' : 'ａ';
                text = ((char)(firstLetter + virtualKey - 65)).ToString();
                return true;
            }

            const string digits = "０１２３４５６７８９";
            if (virtualKey >= 48 && virtualKey <= 57)
            {
                const string shiftedDigits = "）！＠＃＄％＾＆＊（";
                text = (shiftDown ? shiftedDigits : digits)[virtualKey - 48].ToString();
                return true;
            }

            if (virtualKey >= 96 && virtualKey <= 105)
            {
                text = digits[virtualKey - 96].ToString();
                return true;
            }

            // 沿用既有台灣／US 鍵盤的 VK 對應；非列印鍵交回原生處理。
            switch (virtualKey)
            {
                case 106: text = "＊"; break;
                case 107: text = "＋"; break;
                case 109: text = "－"; break;
                case 110: text = "．"; break;
                case 111: text = "／"; break;
                case 186: text = shiftDown ? "：" : "；"; break;
                case 187: text = shiftDown ? "＋" : "＝"; break;
                case 188: text = shiftDown ? "＜" : "，"; break;
                case 189: text = shiftDown ? "＿" : "－"; break;
                case 190: text = shiftDown ? "＞" : "．"; break;
                case 191: text = shiftDown ? "？" : "／"; break;
                case 192: text = shiftDown ? "～" : "｀"; break;
                case 219: text = shiftDown ? "｛" : "［"; break;
                case 220: text = shiftDown ? "｜" : "＼"; break;
                case 221: text = shiftDown ? "｝" : "］"; break;
                case 222: text = shiftDown ? "＂" : "＇"; break;
                default: return false;
            }

            return true;
        }
    }
}
