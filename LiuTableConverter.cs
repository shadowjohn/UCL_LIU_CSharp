using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace uclliu
{
    public static class LiuTableConverter
    {
        private const string IndexToKey = " abcdefghijklmnopqrstuvwxyz,.'[]";
        private static readonly Encoding Utf8NoBom = new UTF8Encoding(false);

        public const string CinHead = "%gen_inp \n"
            + "%ename liu\n"
            + "%cname 嘸蝦米\n"
            + "%encoding UTF-8 \n"
            + "%selkey 0123456789 \n"
            + "%keyname begin\n"
            + "a Ａ\n"
            + "b Ｂ\n"
            + "c Ｃ\n"
            + "d Ｄ\n"
            + "e Ｅ\n"
            + "f Ｆ\n"
            + "g Ｇ\n"
            + "h Ｈ\n"
            + "i Ｉ\n"
            + "j Ｊ\n"
            + "k Ｋ\n"
            + "l Ｌ\n"
            + "m Ｍ\n"
            + "n Ｎ\n"
            + "o Ｏ\n"
            + "p Ｐ\n"
            + "q Ｑ\n"
            + "r Ｒ\n"
            + "s Ｓ\n"
            + "t Ｔ\n"
            + "u Ｕ\n"
            + "v Ｖ\n"
            + "w Ｗ\n"
            + "x Ｘ\n"
            + "y Ｙ\n"
            + "z Ｚ\n"
            + ", ，\n"
            + ". ．\n"
            + "' ’\n"
            + "[ 〔\n"
            + "] 〔\n"
            + "%keyname end\n"
            + "%chardef begin\n";

        public const string CinTail = "%chardef end";

        public static bool EnsureLiuJson(string directory, Action<string> log)
        {
            string jsonPath = Path.Combine(directory, "liu.json");
            if (File.Exists(jsonPath))
            {
                return false;
            }

            string cinPath = Path.Combine(directory, "liu.cin");
            if (!File.Exists(cinPath))
            {
                string tabPath = EnsureUnitabFile(directory, log);
                if (File.Exists(tabPath))
                {
                    if (log != null)
                    {
                        log("開始轉換 liu-uni.tab -> liu.cin");
                    }
                    ConvertUnitabToCinFile(tabPath, cinPath);
                }
            }

            if (!File.Exists(cinPath))
            {
                return false;
            }

            if (log != null)
            {
                log("開始轉換 liu.cin -> liu.json");
            }
            ConvertCinToJsonFile(cinPath, jsonPath);
            return true;
        }

        public static void ConvertUnitabToCinFile(string tabPath, string cinPath)
        {
            byte[] tabBytes = File.ReadAllBytes(tabPath);
            string cinText = ConvertUnitabBytesToCin(tabBytes);
            File.WriteAllText(cinPath, cinText, Utf8NoBom);
        }

        public static string ConvertUnitabBytesToCin(byte[] tabBytes)
        {
            if (tabBytes == null)
            {
                throw new ArgumentNullException("tabBytes");
            }
            if (tabBytes.Length < 2048)
            {
                throw new InvalidDataException("liu-uni.tab 長度不足，無法讀取 key table。");
            }

            ushort[] keyTable = new ushort[1024];
            for (int i = 0; i < keyTable.Length; i++)
            {
                keyTable[i] = (ushort)(tabBytes[i * 2] | (tabBytes[i * 2 + 1] << 8));
            }

            int wordCount = keyTable[keyTable.Length - 1];
            int highBitsLength = (int)Math.Ceiling(((wordCount * 2) + 7) / 8.0) + 8;
            int bitBytesLength = (int)Math.Ceiling(wordCount / 8.0);
            int offset = 2048;
            int requiredLength = offset + highBitsLength + bitBytesLength + bitBytesLength + (wordCount * 3);
            if (tabBytes.Length < requiredLength)
            {
                throw new InvalidDataException("liu-uni.tab 長度不足，無法讀取完整字根資料。");
            }

            byte[] highBytes = new byte[highBitsLength];
            Buffer.BlockCopy(tabBytes, offset, highBytes, 0, highBitsLength);
            offset += highBitsLength;
            offset += bitBytesLength;
            offset += bitBytesLength;

            WordRecord[] words = new WordRecord[wordCount];
            for (int i = 0; i < wordCount; i++)
            {
                int record = (tabBytes[offset] << 16) | (tabBytes[offset + 1] << 8) | tabBytes[offset + 2];
                offset += 3;

                int key3 = (record >> 19) & 0x1F;
                int key4 = (record >> 14) & 0x1F;
                int lowWord = record & 0x3FFF;
                int highWord = (GetBit(highBytes, i * 2 + 16) << 1) | GetBit(highBytes, i * 2 + 17);
                int codePoint = (highWord << 14) | lowWord;

                words[i] = new WordRecord(key3, key4, Char.ConvertFromUtf32(codePoint));
            }

            StringBuilder output = new StringBuilder();
            output.Append(CinHead);
            for (int iKey = 32; iKey < keyTable.Length - 1; iKey++)
            {
                int key1 = iKey / 32;
                int key2 = iKey % 32;
                int start = keyTable[iKey];
                int next = keyTable[iKey + 1];

                if (next < start || start >= words.Length)
                {
                    continue;
                }
                if (next > words.Length)
                {
                    next = words.Length;
                }

                for (int i = start; i < next; i++)
                {
                    string keys = BuildKeys(key1, key2, words[i].Key3, words[i].Key4);
                    if (keys.Length > 0 && words[i].Word.Length > 0)
                    {
                        output.Append(keys).Append(' ').Append(words[i].Word).Append('\n');
                    }
                }
            }
            output.Append(CinTail);
            return output.ToString();
        }

        public static void ConvertCinToJsonFile(string cinPath, string jsonPath)
        {
            string cinText = File.ReadAllText(cinPath, Encoding.UTF8);
            string jsonText = ConvertCinTextToJson(cinText);
            File.WriteAllText(jsonPath, jsonText, Utf8NoBom);
        }

        public static string ConvertCinTextToJson(string cinText)
        {
            if (cinText == null)
            {
                throw new ArgumentNullException("cinText");
            }

            string body = ExtractChardefBody(cinText);
            SortedDictionary<string, List<string>> chardefs = new SortedDictionary<string, List<string>>(StringComparer.Ordinal);

            using (StringReader reader = new StringReader(body))
            {
                string line;
                while ((line = reader.ReadLine()) != null)
                {
                    line = line.Trim();
                    if (line.Length == 0 || line.StartsWith("#", StringComparison.Ordinal))
                    {
                        continue;
                    }

                    string[] parts = line.Split(new char[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);
                    if (parts.Length < 2)
                    {
                        continue;
                    }

                    string key = parts[0].Trim();
                    if (!chardefs.ContainsKey(key))
                    {
                        chardefs[key] = new List<string>();
                    }

                    for (int i = 1; i < parts.Length; i++)
                    {
                        string word = parts[i].Trim();
                        if (word.Length > 0)
                        {
                            chardefs[key].Add(word);
                        }
                    }
                }
            }

            return WriteJson(chardefs);
        }

        private static string EnsureUnitabFile(string directory, Action<string> log)
        {
            string tabPath = Path.Combine(directory, "liu-uni.tab");
            if (File.Exists(tabPath))
            {
                return tabPath;
            }

            string[] candidates = new string[]
            {
                @"C:\windows\SysWOW64\liu-uni.tab",
                @"C:\Program Files\BoshiamyTIP\liu-uni.tab",
                @"C:\Program Files (x86)\BoshiamyTIP\liu-uni.tab"
            };

            for (int i = 0; i < candidates.Length; i++)
            {
                if (File.Exists(candidates[i]))
                {
                    if (log != null)
                    {
                        log("找到系統字根檔：" + candidates[i]);
                    }
                    File.Copy(candidates[i], tabPath, true);
                    return tabPath;
                }
            }

            return tabPath;
        }

        private static string ExtractChardefBody(string cinText)
        {
            const string begin = "%chardef begin";
            const string end = "%chardef end";
            int beginIndex = cinText.IndexOf(begin, StringComparison.OrdinalIgnoreCase);
            if (beginIndex < 0)
            {
                throw new InvalidDataException("liu.cin 缺少 %chardef begin。");
            }

            beginIndex += begin.Length;
            int endIndex = cinText.IndexOf(end, beginIndex, StringComparison.OrdinalIgnoreCase);
            if (endIndex < 0)
            {
                throw new InvalidDataException("liu.cin 缺少 %chardef end。");
            }

            return cinText.Substring(beginIndex, endIndex - beginIndex);
        }

        private static string BuildKeys(int key1, int key2, int key3, int key4)
        {
            StringBuilder keys = new StringBuilder(4);
            AppendKey(keys, key1);
            AppendKey(keys, key2);
            AppendKey(keys, key3);
            AppendKey(keys, key4);
            return keys.ToString();
        }

        private static void AppendKey(StringBuilder keys, int index)
        {
            if (index <= 0)
            {
                return;
            }
            if (index >= IndexToKey.Length)
            {
                return;
            }
            keys.Append(IndexToKey[index]);
        }

        private static int GetBit(byte[] data, int bitIndex)
        {
            int byteIndex = bitIndex / 8;
            int shift = 7 - (bitIndex % 8);
            return (data[byteIndex] >> shift) & 0x01;
        }

        private static string WriteJson(SortedDictionary<string, List<string>> chardefs)
        {
            StringBuilder json = new StringBuilder();
            json.Append("{\n");
            json.Append("    \"chardefs\": {\n");

            int keyIndex = 0;
            foreach (KeyValuePair<string, List<string>> pair in chardefs)
            {
                json.Append("        \"").Append(EscapeJson(pair.Key)).Append("\": [");
                for (int i = 0; i < pair.Value.Count; i++)
                {
                    if (i > 0)
                    {
                        json.Append(", ");
                    }
                    json.Append("\"").Append(EscapeJson(pair.Value[i])).Append("\"");
                }
                json.Append("]");
                keyIndex++;
                if (keyIndex < chardefs.Count)
                {
                    json.Append(",");
                }
                json.Append("\n");
            }

            json.Append("    }\n");
            json.Append("}\n");
            return json.ToString();
        }

        private static string EscapeJson(string value)
        {
            StringBuilder escaped = new StringBuilder(value.Length);
            for (int i = 0; i < value.Length; i++)
            {
                char c = value[i];
                switch (c)
                {
                    case '\\':
                        escaped.Append("\\\\");
                        break;
                    case '"':
                        escaped.Append("\\\"");
                        break;
                    case '\b':
                        escaped.Append("\\b");
                        break;
                    case '\f':
                        escaped.Append("\\f");
                        break;
                    case '\n':
                        escaped.Append("\\n");
                        break;
                    case '\r':
                        escaped.Append("\\r");
                        break;
                    case '\t':
                        escaped.Append("\\t");
                        break;
                    default:
                        if (c < 32)
                        {
                            escaped.Append("\\u").Append(((int)c).ToString("x4"));
                        }
                        else
                        {
                            escaped.Append(c);
                        }
                        break;
                }
            }
            return escaped.ToString();
        }

        private struct WordRecord
        {
            public readonly int Key3;
            public readonly int Key4;
            public readonly string Word;

            public WordRecord(int key3, int key4, string word)
            {
                Key3 = key3;
                Key4 = key4;
                Word = word;
            }
        }
    }
}
