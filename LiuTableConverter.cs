using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace uclliu
{
    public static class LiuTableConverter
    {
        private const string IndexToKey = " abcdefghijklmnopqrstuvwxyz,.'[]";
        private const string ValidRootKeys = "abcdefghijklmnopqrstuvwxyz,.'[]";
        private static readonly Encoding Utf8NoBom = new UTF8Encoding(false);
        private static readonly Encoding StrictUtf8 = new UTF8Encoding(false, true);

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
                EnsureImportedCinFile(directory, cinPath, log);
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
            string cinText = ReadTextBestEffort(cinPath);
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

                    string key = NormalizeRootKey(parts[0].Trim());
                    if (!IsRootText(key))
                    {
                        continue;
                    }
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

        public static void ConvertLooseTextTableToCinFile(string tablePath, string cinPath, string[] startMarkers)
        {
            string tableText = ReadTextBestEffort(tablePath);
            string cinText = ConvertLooseTextTableToCinText(tableText, startMarkers);
            File.WriteAllText(cinPath, cinText, Utf8NoBom);
        }

        public static string ConvertLooseTextTableToCinText(string tableText, string[] startMarkers)
        {
            if (tableText == null)
            {
                throw new ArgumentNullException("tableText");
            }

            string body = ExtractTableBody(tableText, startMarkers);
            StringBuilder rows = new StringBuilder();
            int rowCount = 0;

            using (StringReader reader = new StringReader(body))
            {
                string line;
                while ((line = reader.ReadLine()) != null)
                {
                    if (TryAppendLooseTableLine(rows, line))
                    {
                        rowCount++;
                    }
                }
            }

            if (rowCount == 0)
            {
                throw new InvalidDataException("外部字根檔沒有可轉換的字碼列。");
            }

            return CinHead + rows.ToString() + CinTail;
        }

        public static void ConvertRimeYamlToCinFile(string yamlPath, string cinPath)
        {
            string yamlText = ReadTextBestEffort(yamlPath);
            string cinText = ConvertRimeYamlToCinText(yamlText);
            File.WriteAllText(cinPath, cinText, Utf8NoBom);
        }

        public static string ConvertRimeYamlToCinText(string yamlText)
        {
            if (yamlText == null)
            {
                throw new ArgumentNullException("yamlText");
            }

            string body = ExtractRimeBody(yamlText);
            StringBuilder rows = new StringBuilder();
            int rowCount = 0;

            using (StringReader reader = new StringReader(body))
            {
                string line;
                while ((line = reader.ReadLine()) != null)
                {
                    string key;
                    string word;
                    if (TryParseRimeLine(line, out key, out word))
                    {
                        rows.Append(key).Append(' ').Append(word).Append('\n');
                        rowCount++;
                    }
                }
            }

            if (rowCount == 0)
            {
                throw new InvalidDataException("RIME 字根檔沒有可轉換的字碼列。");
            }

            return CinHead + rows.ToString() + CinTail;
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

        private static void EnsureImportedCinFile(string directory, string cinPath, Action<string> log)
        {
            string sourcePath = Path.Combine(directory, "wuxiami.txt");
            if (File.Exists(sourcePath))
            {
                ImportLooseTextTable(sourcePath, cinPath, log, new string[] { "#修正錯誤：2018-4-15,17" });
                return;
            }

            sourcePath = Path.Combine(directory, "liur_trad.dict.yaml");
            if (File.Exists(sourcePath))
            {
                ImportRimeYaml(sourcePath, cinPath, log);
                return;
            }

            sourcePath = Path.Combine(directory, "liur_Trad.dict.yaml");
            if (File.Exists(sourcePath))
            {
                ImportRimeYaml(sourcePath, cinPath, log);
                return;
            }

            sourcePath = Path.Combine(directory, "terry_boshiamy.txt");
            if (File.Exists(sourcePath))
            {
                ImportLooseTextTable(sourcePath, cinPath, log, new string[] { "## 無蝦米-大五碼-常用漢字：" });
                return;
            }

            sourcePath = Path.Combine(directory, "fcitx_boshiamy.txt");
            if (File.Exists(sourcePath))
            {
                ImportLooseTextTable(sourcePath, cinPath, log, new string[] { "[數據]", "[数据]", "[Data]", "[DATA]" });
                return;
            }

            sourcePath = Path.Combine(directory, "uniliu.txt");
            if (File.Exists(sourcePath))
            {
                ImportLooseTextTable(sourcePath, cinPath, log, new string[] { "[數據]", "[数据]", "[Data]", "[DATA]" });
            }
        }

        private static void ImportLooseTextTable(string sourcePath, string cinPath, Action<string> log, string[] startMarkers)
        {
            if (log != null)
            {
                log("開始轉換 " + Path.GetFileName(sourcePath) + " -> liu.cin");
            }
            ConvertLooseTextTableToCinFile(sourcePath, cinPath, startMarkers);
        }

        private static void ImportRimeYaml(string sourcePath, string cinPath, Action<string> log)
        {
            if (log != null)
            {
                log("開始轉換 " + Path.GetFileName(sourcePath) + " -> liu.cin");
            }
            ConvertRimeYamlToCinFile(sourcePath, cinPath);
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

        private static string ExtractTableBody(string tableText, string[] startMarkers)
        {
            if (startMarkers == null || startMarkers.Length == 0)
            {
                return tableText;
            }

            for (int i = 0; i < startMarkers.Length; i++)
            {
                string marker = startMarkers[i];
                int markerIndex = tableText.IndexOf(marker, StringComparison.OrdinalIgnoreCase);
                if (markerIndex >= 0)
                {
                    return tableText.Substring(markerIndex + marker.Length);
                }
            }

            return tableText;
        }

        private static string ExtractRimeBody(string yamlText)
        {
            const string rimeMarker = "#字碼格式: 字 + Tab + 字碼";
            int markerIndex = yamlText.IndexOf(rimeMarker, StringComparison.OrdinalIgnoreCase);
            if (markerIndex >= 0)
            {
                return yamlText.Substring(markerIndex + rimeMarker.Length);
            }

            using (StringReader reader = new StringReader(yamlText))
            {
                StringBuilder body = new StringBuilder();
                bool inBody = false;
                string line;
                while ((line = reader.ReadLine()) != null)
                {
                    if (!inBody)
                    {
                        if (line.Trim() == "...")
                        {
                            inBody = true;
                        }
                        continue;
                    }
                    body.Append(line).Append('\n');
                }

                if (inBody)
                {
                    return body.ToString();
                }
            }

            return yamlText;
        }

        private static bool TryAppendLooseTableLine(StringBuilder rows, string line)
        {
            string key;
            string[] words;
            if (!TryParseLooseTableLine(line, out key, out words))
            {
                return false;
            }

            rows.Append(key);
            for (int i = 0; i < words.Length; i++)
            {
                rows.Append(' ').Append(words[i]);
            }
            rows.Append('\n');
            return true;
        }

        private static bool TryParseLooseTableLine(string line, out string key, out string[] words)
        {
            key = null;
            words = null;

            if (line == null)
            {
                return false;
            }

            string trimmed = line.Trim();
            if (trimmed.Length == 0)
            {
                return false;
            }
            if (trimmed.StartsWith("#", StringComparison.Ordinal) || trimmed.StartsWith(";", StringComparison.Ordinal))
            {
                return false;
            }
            if (trimmed.StartsWith("%", StringComparison.Ordinal))
            {
                return false;
            }
            if (trimmed.StartsWith("[", StringComparison.Ordinal) && trimmed.EndsWith("]", StringComparison.Ordinal))
            {
                return false;
            }
            if (trimmed.StartsWith("鍵碼=", StringComparison.Ordinal) || trimmed.StartsWith("键码=", StringComparison.Ordinal) || trimmed.StartsWith("碼長=", StringComparison.Ordinal) || trimmed.StartsWith("码长=", StringComparison.Ordinal))
            {
                return false;
            }

            int equalIndex = trimmed.IndexOf('=');
            if (equalIndex > 0)
            {
                string left = trimmed.Substring(0, equalIndex).Trim();
                string right = trimmed.Substring(equalIndex + 1).Trim();
                string normalizedLeft = NormalizeRootKey(left);
                if (IsRootText(normalizedLeft) && right.Length > 0)
                {
                    key = normalizedLeft;
                    words = SplitWords(right);
                    return words.Length > 0;
                }
            }

            string[] parts = trimmed.Split(new char[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);
            if (parts.Length < 2)
            {
                return false;
            }

            string first = NormalizeRootKey(parts[0]);
            if (IsRootText(first))
            {
                key = first;
                words = CopyWords(parts, 1);
                return words.Length > 0;
            }

            string second = NormalizeRootKey(parts[1]);
            if (IsRootText(second))
            {
                key = second;
                words = new string[] { CleanWord(parts[0]) };
                return words[0].Length > 0;
            }

            return false;
        }

        private static bool TryParseRimeLine(string line, out string key, out string word)
        {
            key = null;
            word = null;

            if (line == null)
            {
                return false;
            }

            string trimmed = line.Trim();
            if (trimmed.Length == 0 || trimmed.StartsWith("#", StringComparison.Ordinal))
            {
                return false;
            }
            if (trimmed == "---" || trimmed == "...")
            {
                return false;
            }

            string[] parts;
            if (trimmed.IndexOf('\t') >= 0)
            {
                parts = trimmed.Split(new char[] { '\t' }, StringSplitOptions.RemoveEmptyEntries);
            }
            else
            {
                parts = trimmed.Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
            }

            if (parts.Length < 2)
            {
                return false;
            }

            string normalizedKey = NormalizeRootKey(parts[1]);
            if (!IsRootText(normalizedKey))
            {
                return false;
            }

            string normalizedWord = CleanWord(parts[0]);
            if (normalizedWord.StartsWith("~", StringComparison.Ordinal))
            {
                normalizedWord = normalizedWord.Substring(1);
            }
            if (normalizedWord.Length == 0)
            {
                return false;
            }

            key = normalizedKey;
            word = normalizedWord;
            return true;
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

        private static string[] SplitWords(string text)
        {
            string[] parts = text.Split(new char[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);
            List<string> result = new List<string>();
            for (int i = 0; i < parts.Length; i++)
            {
                string word = CleanWord(parts[i]);
                if (word.Length > 0)
                {
                    result.Add(word);
                }
            }
            return result.ToArray();
        }

        private static string[] CopyWords(string[] parts, int startIndex)
        {
            List<string> result = new List<string>();
            for (int i = startIndex; i < parts.Length; i++)
            {
                string word = CleanWord(parts[i]);
                if (word.Length > 0)
                {
                    result.Add(word);
                }
            }
            return result.ToArray();
        }

        private static string CleanWord(string word)
        {
            if (word == null)
            {
                return "";
            }
            return word.Trim();
        }

        private static string NormalizeRootKey(string key)
        {
            if (key == null)
            {
                return "";
            }
            return key.Trim().ToLowerInvariant();
        }

        private static bool IsRootText(string key)
        {
            if (key == null || key.Length == 0 || key.Length > 5)
            {
                return false;
            }
            for (int i = 0; i < key.Length; i++)
            {
                if (ValidRootKeys.IndexOf(key[i]) < 0)
                {
                    return false;
                }
            }
            return true;
        }

        private static string ReadTextBestEffort(string path)
        {
            byte[] data = File.ReadAllBytes(path);
            try
            {
                return StrictUtf8.GetString(data);
            }
            catch (DecoderFallbackException)
            {
                return Encoding.Default.GetString(data);
            }
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
