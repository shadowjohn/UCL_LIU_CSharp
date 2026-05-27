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

            return NormalizeChardefKeys(ParseStringListDictionary(chardefs));
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
