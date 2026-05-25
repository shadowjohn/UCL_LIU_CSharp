using System;
using System.Collections.Generic;
using System.IO;
using System.Json;
using System.Text;

namespace uclliu
{
    public static class CustomDictionaryStore
    {
        private const string AllowedRootChars = "abcdefghijklmnopqrstuvwxyz,.]['";
        private static readonly Encoding Utf8NoBom = new UTF8Encoding(false);

        public static string NormalizeRootKey(string key)
        {
            if (key == null)
            {
                return "";
            }
            return key.Trim().ToLowerInvariant();
        }

        public static bool IsValidRootKey(string key)
        {
            key = NormalizeRootKey(key);
            if (key.Length < 1 || key.Length > 5)
            {
                return false;
            }

            for (int i = 0; i < key.Length; i++)
            {
                if (AllowedRootChars.IndexOf(key[i]) < 0)
                {
                    return false;
                }
            }
            return true;
        }

        public static Dictionary<string, List<string>> Load(string path, Action<string> log)
        {
            Dictionary<string, List<string>> entries = new Dictionary<string, List<string>>(StringComparer.Ordinal);
            if (!File.Exists(path))
            {
                return entries;
            }

            string json = File.ReadAllText(path, Encoding.UTF8);
            if (json.Trim().Length == 0)
            {
                return entries;
            }

            JsonObject root = JsonValue.Parse(json) as JsonObject;
            if (root == null)
            {
                if (log != null)
                {
                    log("custom.json 根節點不是 JSON object，已忽略。");
                }
                return entries;
            }

            foreach (KeyValuePair<string, JsonValue> pair in root)
            {
                string key = NormalizeRootKey(pair.Key);
                if (!IsValidRootKey(key))
                {
                    if (log != null)
                    {
                        log("custom.json 略過非法字根：" + pair.Key);
                    }
                    continue;
                }

                if (!entries.ContainsKey(key))
                {
                    entries[key] = new List<string>();
                }

                foreach (string value in JsonValueToStrings(pair.Value))
                {
                    AddUnique(entries[key], value);
                }
            }

            return entries;
        }

        public static void Save(string path, IDictionary<string, List<string>> entries)
        {
            string directory = Path.GetDirectoryName(path);
            if (!string.IsNullOrEmpty(directory) && !Directory.Exists(directory))
            {
                Directory.CreateDirectory(directory);
            }

            File.WriteAllText(path, ToJson(entries), Utf8NoBom);
        }

        public static int MergeInto(JsonValue uclcode, IDictionary<string, List<string>> customEntries)
        {
            if (uclcode == null || customEntries == null || !uclcode.ContainsKey("chardefs"))
            {
                return 0;
            }

            JsonObject chardefs = uclcode["chardefs"] as JsonObject;
            if (chardefs == null)
            {
                return 0;
            }

            int addedCount = 0;
            foreach (KeyValuePair<string, List<string>> pair in customEntries)
            {
                string key = NormalizeRootKey(pair.Key);
                if (!IsValidRootKey(key))
                {
                    continue;
                }

                List<string> merged = new List<string>();
                if (chardefs.ContainsKey(key))
                {
                    merged.AddRange(JsonValueToStrings(chardefs[key]));
                }

                foreach (string value in pair.Value)
                {
                    if (AddUnique(merged, value))
                    {
                        addedCount++;
                    }
                }

                chardefs[key] = ParseJsonArray(merged);
            }

            return addedCount;
        }

        public static List<string> JsonValueToStrings(JsonValue value)
        {
            List<string> result = new List<string>();
            JsonArray array = value as JsonArray;
            if (array != null)
            {
                foreach (JsonValue item in array)
                {
                    AddUnique(result, ReadJsonString(item));
                }
            }
            else
            {
                AddUnique(result, ReadJsonString(value));
            }
            return result;
        }

        public static string ToJson(IDictionary<string, List<string>> entries)
        {
            SortedDictionary<string, List<string>> sorted = new SortedDictionary<string, List<string>>(StringComparer.Ordinal);
            foreach (KeyValuePair<string, List<string>> pair in entries)
            {
                string key = NormalizeRootKey(pair.Key);
                if (!IsValidRootKey(key))
                {
                    continue;
                }
                if (!sorted.ContainsKey(key))
                {
                    sorted[key] = new List<string>();
                }
                foreach (string value in pair.Value)
                {
                    AddUnique(sorted[key], value);
                }
            }

            StringBuilder json = new StringBuilder();
            json.Append("{\n");
            int keyIndex = 0;
            foreach (KeyValuePair<string, List<string>> pair in sorted)
            {
                json.Append("  \"").Append(EscapeJson(pair.Key)).Append("\": [");
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
                if (keyIndex < sorted.Count)
                {
                    json.Append(",");
                }
                json.Append("\n");
            }
            json.Append("}\n");
            return json.ToString();
        }

        private static JsonValue ParseJsonArray(List<string> values)
        {
            StringBuilder json = new StringBuilder();
            json.Append("[");
            for (int i = 0; i < values.Count; i++)
            {
                if (i > 0)
                {
                    json.Append(",");
                }
                json.Append("\"").Append(EscapeJson(values[i])).Append("\"");
            }
            json.Append("]");
            return JsonValue.Parse(json.ToString());
        }

        private static bool AddUnique(List<string> values, string value)
        {
            if (value == null)
            {
                return false;
            }
            value = value.Trim();
            if (value.Length == 0 || values.Contains(value))
            {
                return false;
            }
            values.Add(value);
            return true;
        }

        private static string ReadJsonString(JsonValue value)
        {
            if (value == null)
            {
                return "";
            }
            try
            {
                return (string)value;
            }
            catch
            {
                return value.ToString().Trim('"');
            }
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
    }
}
