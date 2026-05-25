using System;
using System.Collections.Generic;

namespace uclliu
{
    public sealed class PhoneCodeTable
    {
        private readonly Dictionary<string, List<string>> candidatesByKeyboardCode;
        private readonly Dictionary<string, List<string>> phonesByWord;
        private readonly Dictionary<char, string> phoneByKeyboard;
        private readonly Dictionary<string, string> keyboardByPhone;

        private PhoneCodeTable(
            Dictionary<string, List<string>> candidatesByKeyboardCode,
            Dictionary<string, List<string>> phonesByWord,
            Dictionary<char, string> phoneByKeyboard,
            Dictionary<string, string> keyboardByPhone)
        {
            this.candidatesByKeyboardCode = candidatesByKeyboardCode;
            this.phonesByWord = phonesByWord;
            this.phoneByKeyboard = phoneByKeyboard;
            this.keyboardByPhone = keyboardByPhone;
            IsAvailable = candidatesByKeyboardCode.Count > 0 && phoneByKeyboard.Count > 0;
        }

        public bool IsAvailable { get; private set; }

        public static PhoneCodeTable Empty()
        {
            return new PhoneCodeTable(
                new Dictionary<string, List<string>>(StringComparer.Ordinal),
                new Dictionary<string, List<string>>(StringComparer.Ordinal),
                new Dictionary<char, string>(),
                new Dictionary<string, string>(StringComparer.Ordinal));
        }

        public static PhoneCodeTable Parse(IEnumerable<string> lines)
        {
            if (lines == null)
            {
                return Empty();
            }

            List<string> rows = new List<string>();
            foreach (string line in lines)
            {
                string row = (line ?? "").Trim();
                if (row.Length > 0)
                {
                    rows.Add(row);
                }
            }

            if (rows.Count < 3 || rows[0].IndexOf("VERSION_0.01", StringComparison.OrdinalIgnoreCase) < 0)
            {
                return Empty();
            }

            string[] keyboardTokens = SplitTokens(rows[1]);
            string[] phoneTokens = SplitTokens(rows[2]);
            Dictionary<char, string> phoneByKeyboard = new Dictionary<char, string>();
            Dictionary<string, string> keyboardByPhone = new Dictionary<string, string>(StringComparer.Ordinal);
            int maxToken = Math.Min(keyboardTokens.Length, phoneTokens.Length);
            for (int i = 0; i < maxToken; i++)
            {
                if (keyboardTokens[i].Length == 0 || phoneTokens[i].Length == 0)
                {
                    continue;
                }

                char keyboard = Char.ToLowerInvariant(keyboardTokens[i][0]);
                phoneByKeyboard[keyboard] = phoneTokens[i];
                if (!keyboardByPhone.ContainsKey(phoneTokens[i]))
                {
                    keyboardByPhone.Add(phoneTokens[i], keyboard.ToString());
                }
            }

            Dictionary<string, List<string>> candidatesByKeyboardCode = new Dictionary<string, List<string>>(StringComparer.Ordinal);
            Dictionary<string, List<string>> phonesByWord = new Dictionary<string, List<string>>(StringComparer.Ordinal);
            for (int i = 3; i < rows.Count; i++)
            {
                string[] tokens = SplitTokens(rows[i]);
                if (tokens.Length < 2)
                {
                    continue;
                }

                string keyboardCode = tokens[0].ToLowerInvariant();
                string phoneCode = KeyboardToPhoneCode(keyboardCode, phoneByKeyboard);
                List<string> candidates = new List<string>();
                for (int j = 1; j < tokens.Length; j++)
                {
                    string word = tokens[j];
                    if (word.Length == 0 || IsBopomofoToken(word))
                    {
                        continue;
                    }

                    AddUnique(candidates, word);
                    if (phoneCode.Length > 0)
                    {
                        List<string> phones;
                        if (!phonesByWord.TryGetValue(word, out phones))
                        {
                            phones = new List<string>();
                            phonesByWord.Add(word, phones);
                        }
                        AddUnique(phones, phoneCode);
                    }
                }

                if (candidates.Count > 0)
                {
                    candidatesByKeyboardCode[keyboardCode] = candidates;
                }
            }

            return new PhoneCodeTable(candidatesByKeyboardCode, phonesByWord, phoneByKeyboard, keyboardByPhone);
        }

        public bool CanAcceptKeyboardKey(char key)
        {
            return phoneByKeyboard.ContainsKey(Char.ToLowerInvariant(key));
        }

        public bool TryKeyboardKeyToPhone(char key, out string phone)
        {
            return phoneByKeyboard.TryGetValue(Char.ToLowerInvariant(key), out phone);
        }

        public string KeyboardToPhoneCode(string keyboardCode)
        {
            return KeyboardToPhoneCode(keyboardCode, phoneByKeyboard);
        }

        public string PhoneCodeToKeyboard(string phoneCode)
        {
            if (String.IsNullOrEmpty(phoneCode))
            {
                return "";
            }

            string output = "";
            for (int i = 0; i < phoneCode.Length; i++)
            {
                string phone = phoneCode[i].ToString();
                string keyboard;
                if (!keyboardByPhone.TryGetValue(phone, out keyboard))
                {
                    return "";
                }
                output += keyboard;
            }
            return output;
        }

        public List<string> FindCandidatesByPhoneCode(string phoneCode)
        {
            string keyboardCode = PhoneCodeToKeyboard(phoneCode);
            List<string> candidates;
            if (keyboardCode.Length == 0 || !candidatesByKeyboardCode.TryGetValue(keyboardCode, out candidates))
            {
                return new List<string>();
            }
            return new List<string>(candidates);
        }

        public List<string> GetPhonesForWord(string word)
        {
            List<string> phones;
            if (String.IsNullOrEmpty(word) || !phonesByWord.TryGetValue(word, out phones))
            {
                return new List<string>();
            }
            return new List<string>(phones);
        }

        private static string KeyboardToPhoneCode(string keyboardCode, Dictionary<char, string> phoneByKeyboard)
        {
            if (String.IsNullOrEmpty(keyboardCode))
            {
                return "";
            }

            string output = "";
            for (int i = 0; i < keyboardCode.Length; i++)
            {
                string phone;
                if (!phoneByKeyboard.TryGetValue(Char.ToLowerInvariant(keyboardCode[i]), out phone))
                {
                    return "";
                }
                output += phone;
            }
            return output;
        }

        private static string[] SplitTokens(string value)
        {
            return (value ?? "").Split(new char[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);
        }

        private static void AddUnique(List<string> values, string value)
        {
            if (!values.Contains(value))
            {
                values.Add(value);
            }
        }

        private static bool IsBopomofoToken(string value)
        {
            for (int i = 0; i < value.Length; i++)
            {
                char c = value[i];
                if (!((c >= '\u3100' && c <= '\u312F') || c == '\u02CA' || c == '\u02C7' || c == '\u02CB' || c == '\u02D9'))
                {
                    return false;
                }
            }
            return value.Length > 0;
        }
    }
}
