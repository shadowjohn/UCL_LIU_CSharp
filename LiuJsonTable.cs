using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
#if NETCOREAPP
using System.Text.Json;
#else
using System.Web.Script.Serialization;
#endif

namespace uclliu
{
    public static class LiuJsonTable
    {
        public static object DecodeJson(string json)
        {
            if (json == null)
            {
                throw new ArgumentNullException("json");
            }

#if NETCOREAPP
            using (JsonDocument document = JsonDocument.Parse(json))
            {
                return ConvertJsonElement(document.RootElement);
            }
#else
            JavaScriptSerializer serializer = new JavaScriptSerializer();
            serializer.MaxJsonLength = int.MaxValue;
            return serializer.DeserializeObject(json);
#endif
        }

        public static Dictionary<string, List<string>> ParseChardefsJson(string json)
        {
            return ParseChardefsRoot(DecodeJson(json));
        }

        public static Dictionary<string, List<string>> ParseChardefsRoot(object root)
        {
            Dictionary<string, object> rootObject = AsDictionary(root);
            object chardefs;
            if (rootObject == null || !rootObject.TryGetValue("chardefs", out chardefs))
            {
                throw new InvalidDataException("liu.json 缺少 chardefs。");
            }

            return ApplyFcitxJapaneseAliases(NormalizeChardefKeys(ParseStringListDictionary(chardefs)), false);
        }

        public static Dictionary<string, List<string>> ParseStringListDictionaryJson(string json)
        {
            return ParseStringListDictionary(DecodeJson(json));
        }

        public static Dictionary<string, List<string>> ParseStringListDictionary(object root)
        {
            Dictionary<string, object> rootObject = AsDictionary(root);
            if (rootObject == null)
            {
                throw new InvalidDataException("JSON 根節點不是 object。");
            }

            Dictionary<string, List<string>> result = new Dictionary<string, List<string>>(StringComparer.Ordinal);
            foreach (KeyValuePair<string, object> pair in rootObject)
            {
                result[pair.Key] = ValuesToStrings(pair.Value);
            }
            return result;
        }

        public static List<string> ValuesToStrings(object value)
        {
            List<string> result = new List<string>();
            if (value == null)
            {
                return result;
            }

            string text = value as string;
            if (text != null)
            {
                result.Add(text);
                return result;
            }

            IDictionary dictionary = value as IDictionary;
            if (dictionary != null)
            {
                return result;
            }

            IEnumerable values = value as IEnumerable;
            if (values != null)
            {
                foreach (object item in values)
                {
                    result.Add(ConvertJsonValueToString(item));
                }
                return result;
            }

            result.Add(ConvertJsonValueToString(value));
            return result;
        }

        private static Dictionary<string, object> AsDictionary(object value)
        {
            Dictionary<string, object> typed = value as Dictionary<string, object>;
            if (typed != null)
            {
                return typed;
            }

            IDictionary dictionary = value as IDictionary;
            if (dictionary == null)
            {
                return null;
            }

            Dictionary<string, object> result = new Dictionary<string, object>(StringComparer.Ordinal);
            foreach (DictionaryEntry entry in dictionary)
            {
                result[Convert.ToString(entry.Key, CultureInfo.InvariantCulture)] = entry.Value;
            }
            return result;
        }

        private static Dictionary<string, List<string>> NormalizeChardefKeys(Dictionary<string, List<string>> chardefs)
        {
            Dictionary<string, List<string>> result = new Dictionary<string, List<string>>(StringComparer.Ordinal);
            foreach (KeyValuePair<string, List<string>> pair in chardefs)
            {
                string key = pair.Key == null ? "" : pair.Key.Trim().ToLowerInvariant();
                if (key.Length == 0)
                {
                    continue;
                }
                if (!result.ContainsKey(key))
                {
                    result[key] = new List<string>();
                }
                result[key].AddRange(pair.Value);
            }
            return result;
        }

        internal static Dictionary<string, List<string>> ApplyFcitxJapaneseAliases(
            Dictionary<string, List<string>> chardefs, bool replaceUnicodeAliases)
        {
            Dictionary<string, List<string>> result = new Dictionary<string, List<string>>(StringComparer.Ordinal);
            foreach (KeyValuePair<string, List<string>> pair in chardefs)
            {
                result[pair.Key] = new List<string>(pair.Value);
            }

            bool hasLegacyHiragana = HasFcitxJapaneseAnchor(chardefs, "ja,", "あ");
            bool hasLegacyKatakana = HasFcitxJapaneseAnchor(chardefs, "ja.", "ア");
            foreach (KeyValuePair<string, List<string>> pair in chardefs)
            {
                string alias;
                if (!TryGetFcitxJapaneseAlias(pair.Key, out alias)
                    || (pair.Key[pair.Key.Length - 1] == ',' ? !hasLegacyHiragana : !hasLegacyKatakana))
                {
                    continue;
                }

                List<string> currentValues;
                if (!replaceUnicodeAliases && chardefs.TryGetValue(alias, out currentValues)
                    && currentValues.Count > 0 && alias[0] != 'j' && !ContainsPrivateUseCharacters(currentValues))
                {
                    // 轉換後的標準 Unicode alias 已可直接使用，重新載入時不可再把 ji, 的 j 去掉。
                    continue;
                }
                result[alias] = new List<string>(pair.Value);
            }
            return result;
        }

        private static bool HasFcitxJapaneseAnchor(Dictionary<string, List<string>> chardefs, string key, string kana)
        {
            List<string> values;
            // 一般羅馬字 ja, 是「じゃ」；只有舊 fcitx 的 ja, 才表示「あ」，兩種假名需分別辨識。
            return chardefs.TryGetValue(key, out values) && values.Contains(kana);
        }

        private static bool ContainsPrivateUseCharacters(List<string> values)
        {
            foreach (string value in values)
            {
                for (int i = 0; i < value.Length; i++)
                {
                    if (CharUnicodeInfo.GetUnicodeCategory(value, i) == UnicodeCategory.PrivateUse)
                    {
                        return true;
                    }
                }
            }
            return false;
        }

        private static bool TryGetFcitxJapaneseAlias(string key, out string alias)
        {
            alias = null;
            if (String.IsNullOrEmpty(key) || key.Length < 2 || key[0] != 'j')
            {
                return false;
            }

            char suffix = key[key.Length - 1];
            if (suffix != ',' && suffix != '.')
            {
                return false;
            }

            for (int i = 1; i < key.Length - 1; i++)
            {
                if (key[i] < 'a' || key[i] > 'z')
                {
                    return false;
                }
            }

            alias = key.Substring(1);
            return true;
        }

        private static string ConvertJsonValueToString(object value)
        {
            if (value == null)
            {
                return "";
            }
            return Convert.ToString(value, CultureInfo.InvariantCulture);
        }

#if NETCOREAPP
        private static object ConvertJsonElement(JsonElement element)
        {
            switch (element.ValueKind)
            {
                case JsonValueKind.Object:
                    Dictionary<string, object> objectValue = new Dictionary<string, object>(StringComparer.Ordinal);
                    foreach (JsonProperty property in element.EnumerateObject())
                    {
                        objectValue[property.Name] = ConvertJsonElement(property.Value);
                    }
                    return objectValue;
                case JsonValueKind.Array:
                    List<object> arrayValue = new List<object>();
                    foreach (JsonElement item in element.EnumerateArray())
                    {
                        arrayValue.Add(ConvertJsonElement(item));
                    }
                    return arrayValue;
                case JsonValueKind.String:
                    return element.GetString();
                case JsonValueKind.Number:
                    return element.GetRawText();
                case JsonValueKind.True:
                    return true;
                case JsonValueKind.False:
                    return false;
                default:
                    return null;
            }
        }
#endif
    }
}
