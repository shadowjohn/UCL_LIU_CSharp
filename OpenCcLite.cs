using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;

namespace uclliu
{
    public sealed class OpenCcLite
    {
        private const string PhraseResourceSuffix = "third_party.opencc.dictionary.STPhrases.txt";
        private const string CharacterResourceSuffix = "third_party.opencc.dictionary.STCharacters.txt";
        private static readonly UTF8Encoding StrictUtf8 = new UTF8Encoding(false, true);
        private static readonly Lazy<OpenCcLite> DefaultInstance = new Lazy<OpenCcLite>(LoadDefault, true);
        private static readonly Regex SplitCharsRegex = new Regex(
            @"(\s+|-|,|\.|\?|!|\*|　|，|。|、|；|：|？|！|…|“|”|‘|’|『|』|「|」|﹁|﹂|—|－|（|）|《|》|〈|〉|～|．|／|＼|︒|︑|︔|︓|︿|﹀|︹|︺|︙|︐|［|﹇|］|﹈|︕|︖|︰|︳|︴|︽|︾|︵|︶|｛|︷|｝|︸|﹃|﹄|【|︻|】|︼)",
            RegexOptions.Compiled);

        private readonly ConversionDictionary phraseDictionary;
        private readonly ConversionDictionary characterDictionary;

        private OpenCcLite(ConversionDictionary phrases, ConversionDictionary characters)
        {
            phraseDictionary = phrases;
            characterDictionary = characters;
        }

        public static OpenCcLite Default
        {
            get { return DefaultInstance.Value; }
        }

        public static void WarmUpAsync(Action<Exception> onError)
        {
            ThreadPool.QueueUserWorkItem(delegate
            {
                try
                {
                    GC.KeepAlive(Default);
                }
                catch (Exception ex)
                {
                    if (onError != null)
                    {
                        onError(ex);
                    }
                }
            });
        }

        public static OpenCcLite FromDictionaryText(string phraseText, string characterText)
        {
            return new OpenCcLite(ParseDictionary(phraseText), ParseDictionary(characterText));
        }

        public string ConvertSimplifiedToTraditional(string text)
        {
            if (string.IsNullOrEmpty(text))
            {
                return text ?? string.Empty;
            }

            string[] parts = SplitCharsRegex.Split(text);
            StringBuilder output = new StringBuilder(text.Length);
            for (int i = 0; i < parts.Length; i++)
            {
                string part = parts[i];
                if (part.Length == 0)
                {
                    continue;
                }

                if (IsSeparatorSegment(part))
                {
                    output.Append(part);
                }
                else
                {
                    output.Append(ConvertSegment(part));
                }
            }

            return output.ToString();
        }

        private static OpenCcLite LoadDefault()
        {
            string phrases = ReadEmbeddedResource(PhraseResourceSuffix);
            string characters = ReadEmbeddedResource(CharacterResourceSuffix);
            return FromDictionaryText(phrases, characters);
        }

        private string ConvertSegment(string text)
        {
            List<Segment> segments = new List<Segment>();
            segments.Add(new Segment(text, false));
            segments = ApplyDictionary(segments, phraseDictionary);
            segments = ApplyDictionary(segments, characterDictionary);

            StringBuilder output = new StringBuilder(text.Length);
            for (int i = 0; i < segments.Count; i++)
            {
                output.Append(segments[i].Text);
            }

            return output.ToString();
        }

        private static List<Segment> ApplyDictionary(List<Segment> segments, ConversionDictionary dictionary)
        {
            List<Segment> result = new List<Segment>();
            for (int i = 0; i < segments.Count; i++)
            {
                Segment segment = segments[i];
                if (segment.IsConverted)
                {
                    result.Add(segment);
                }
                else
                {
                    AddConvertedPieces(segment.Text, dictionary, result);
                }
            }

            return result;
        }

        private static void AddConvertedPieces(string text, ConversionDictionary dictionary, List<Segment> result)
        {
            if (text.Length == 0)
            {
                return;
            }

            OpenCcMatch match = FindMatch(text, dictionary);
            if (match == null)
            {
                result.Add(new Segment(text, false));
                return;
            }

            if (match.Index > 0)
            {
                AddConvertedPieces(text.Substring(0, match.Index), dictionary, result);
            }

            result.Add(new Segment(match.Value, true));

            int rightStart = match.Index + match.Length;
            if (rightStart < text.Length)
            {
                AddConvertedPieces(text.Substring(rightStart), dictionary, result);
            }
        }

        private static OpenCcMatch FindMatch(string text, ConversionDictionary dictionary)
        {
            if (dictionary.MaxKeyLength == 0)
            {
                return null;
            }

            int maxLength = Math.Min(text.Length, dictionary.MaxKeyLength);
            for (int length = maxLength; length >= dictionary.MinKeyLength; length--)
            {
                for (int index = 0; index <= text.Length - length; index++)
                {
                    string key = text.Substring(index, length);
                    string value;
                    if (dictionary.TryGetValue(key, out value))
                    {
                        return new OpenCcMatch(index, length, value);
                    }
                }
            }

            return null;
        }

        private static ConversionDictionary ParseDictionary(string dictionaryText)
        {
            Dictionary<string, string> entries = new Dictionary<string, string>(StringComparer.Ordinal);
            int maxKeyLength = 0;
            int minKeyLength = int.MaxValue;

            using (StringReader reader = new StringReader(dictionaryText ?? string.Empty))
            {
                string line;
                while ((line = reader.ReadLine()) != null)
                {
                    line = line.Trim();
                    if (line.Length == 0 || line.StartsWith("#", StringComparison.Ordinal))
                    {
                        continue;
                    }

                    int tabIndex = line.IndexOf('\t');
                    if (tabIndex <= 0 || tabIndex >= line.Length - 1)
                    {
                        continue;
                    }

                    string key = line.Substring(0, tabIndex);
                    string value = FirstConversionValue(line.Substring(tabIndex + 1).Trim());
                    if (value.Length == 0)
                    {
                        continue;
                    }

                    entries[key] = value;
                    if (key.Length > maxKeyLength)
                    {
                        maxKeyLength = key.Length;
                    }
                    if (key.Length < minKeyLength)
                    {
                        minKeyLength = key.Length;
                    }
                }
            }

            if (entries.Count == 0)
            {
                minKeyLength = 0;
            }

            return new ConversionDictionary(entries, maxKeyLength, minKeyLength);
        }

        private static string FirstConversionValue(string value)
        {
            string[] values = value.Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
            if (values.Length == 0)
            {
                return string.Empty;
            }

            return values[0];
        }

        private static bool IsSeparatorSegment(string text)
        {
            Match match = SplitCharsRegex.Match(text);
            return match.Success && match.Index == 0 && match.Length == text.Length;
        }

        private static string ReadEmbeddedResource(string resourceSuffix)
        {
            Assembly assembly = typeof(OpenCcLite).Assembly;
            string[] resourceNames = assembly.GetManifestResourceNames();
            for (int i = 0; i < resourceNames.Length; i++)
            {
                string resourceName = resourceNames[i];
                if (resourceName.EndsWith(resourceSuffix, StringComparison.OrdinalIgnoreCase))
                {
                    using (Stream stream = assembly.GetManifestResourceStream(resourceName))
                    {
                        if (stream == null)
                        {
                            break;
                        }
                        using (StreamReader reader = new StreamReader(stream, StrictUtf8))
                        {
                            return reader.ReadToEnd();
                        }
                    }
                }
            }

            string fileText = ReadDictionaryFileFallback(resourceSuffix);
            if (fileText != null)
            {
                return fileText;
            }

            throw new FileNotFoundException("OpenCC Lite dictionary resource missing: " + resourceSuffix);
        }

        private static string ReadDictionaryFileFallback(string resourceSuffix)
        {
            string fileName = resourceSuffix.EndsWith("STPhrases.txt", StringComparison.OrdinalIgnoreCase)
                ? "STPhrases.txt"
                : "STCharacters.txt";
            string path = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, Path.Combine("third_party\\opencc\\dictionary", fileName));
            if (!File.Exists(path))
            {
                return null;
            }

            return File.ReadAllText(path, StrictUtf8);
        }

        private sealed class ConversionDictionary
        {
            private readonly Dictionary<string, string> entries;

            public ConversionDictionary(Dictionary<string, string> entries, int maxKeyLength, int minKeyLength)
            {
                this.entries = entries;
                MaxKeyLength = maxKeyLength;
                MinKeyLength = minKeyLength;
            }

            public int MaxKeyLength { get; private set; }
            public int MinKeyLength { get; private set; }

            public bool TryGetValue(string key, out string value)
            {
                return entries.TryGetValue(key, out value);
            }
        }

        private sealed class Segment
        {
            public Segment(string text, bool isConverted)
            {
                Text = text;
                IsConverted = isConverted;
            }

            public string Text { get; private set; }
            public bool IsConverted { get; private set; }
        }

        private sealed class OpenCcMatch
        {
            public OpenCcMatch(int index, int length, string value)
            {
                Index = index;
                Length = length;
                Value = value;
            }

            public int Index { get; private set; }
            public int Length { get; private set; }
            public string Value { get; private set; }
        }
    }
}
