using System;
using System.Collections.Generic;

namespace uclliu
{
    public sealed class LiuReverseLookupTable
    {
        private LiuReverseLookupTable(Dictionary<string, string> wordToRoot, Dictionary<string, string> codeToWord)
        {
            WordToRoot = wordToRoot;
            CodeToWord = codeToWord;
        }

        public Dictionary<string, string> WordToRoot { get; private set; }
        public Dictionary<string, string> CodeToWord { get; private set; }

        public static LiuReverseLookupTable Build(Dictionary<string, List<string>> chardefs)
        {
            Dictionary<string, string> wordToRoot = new Dictionary<string, string>(StringComparer.Ordinal);
            Dictionary<string, string> codeToWord = new Dictionary<string, string>(StringComparer.Ordinal);

            if (chardefs == null)
            {
                return new LiuReverseLookupTable(wordToRoot, codeToWord);
            }

            foreach (KeyValuePair<string, List<string>> pair in chardefs)
            {
                if (String.IsNullOrEmpty(pair.Key) || pair.Value == null)
                {
                    continue;
                }

                string root = pair.Key.ToLowerInvariant();
                for (int i = 0; i < pair.Value.Count; i++)
                {
                    string word = pair.Value[i];
                    if (String.IsNullOrEmpty(word))
                    {
                        continue;
                    }

                    string code = i == 0 ? root : root + i.ToString();
                    if (!codeToWord.ContainsKey(code))
                    {
                        codeToWord.Add(code, word);
                    }

                    string oldCode;
                    if (!wordToRoot.TryGetValue(word, out oldCode) || code.Length < oldCode.Length)
                    {
                        wordToRoot[word] = code;
                    }
                }
            }

            return new LiuReverseLookupTable(wordToRoot, codeToWord);
        }
    }
}
