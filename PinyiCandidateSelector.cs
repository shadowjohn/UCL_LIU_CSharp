using System;
using System.Collections.Generic;

namespace uclliu
{
    public static class PinyiCandidateSelector
    {
        public static List<string> FindCandidates(IEnumerable<string> sameSoundData, string targetWord)
        {
            List<string> lines = new List<string>();
            foreach (string line in sameSoundData)
            {
                lines.Add(line ?? "");
            }

            bool isVersion001 = lines.Count > 0 && lines[0].IndexOf("VERSION_0.01", StringComparison.OrdinalIgnoreCase) >= 0;
            int startLine = isVersion001 ? Math.Min(3, lines.Count) : 0;
            List<PinyiLineMatch> matches = new List<PinyiLineMatch>();

            for (int i = startLine; i < lines.Count; i++)
            {
                string[] tokens = SplitTokens(lines[i]);
                if (tokens.Length == 0)
                {
                    continue;
                }

                int candidateStart = isVersion001 ? 1 : 0;
                if (tokens.Length <= candidateStart)
                {
                    continue;
                }

                int targetIndex = IndexOf(tokens, targetWord, candidateStart);
                if (targetIndex < 0)
                {
                    continue;
                }

                List<string> candidates = new List<string>();
                for (int j = candidateStart; j < tokens.Length; j++)
                {
                    if (isVersion001 && IsBopomofoToken(tokens[j]))
                    {
                        continue;
                    }
                    candidates.Add(tokens[j]);
                }

                matches.Add(new PinyiLineMatch(targetIndex - candidateStart, candidates));
            }

            matches.Sort(delegate(PinyiLineMatch left, PinyiLineMatch right)
            {
                int byIndex = left.TargetIndex.CompareTo(right.TargetIndex);
                if (byIndex != 0)
                {
                    return byIndex;
                }
                return 0;
            });

            List<string> output = new List<string>();
            HashSet<string> seen = new HashSet<string>();
            foreach (PinyiLineMatch match in matches)
            {
                foreach (string candidate in match.Candidates)
                {
                    if (seen.Add(candidate))
                    {
                        output.Add(candidate);
                    }
                }
            }

            return output;
        }

        public static List<string> PageCandidates(List<string> candidates, int startIndex, int pageSize, out bool hasMorePage, out int nextIndex)
        {
            if (startIndex < 0 || startIndex >= candidates.Count)
            {
                startIndex = 0;
            }

            int maxIndex = startIndex + pageSize;
            if (maxIndex >= candidates.Count)
            {
                maxIndex = candidates.Count;
                hasMorePage = false;
            }
            else
            {
                hasMorePage = true;
            }

            List<string> page = new List<string>();
            for (int i = startIndex; i < maxIndex; i++)
            {
                page.Add(candidates[i]);
            }

            nextIndex = startIndex + pageSize;
            if (nextIndex >= candidates.Count)
            {
                nextIndex = 0;
            }

            return page;
        }

        private static string[] SplitTokens(string line)
        {
            return (line ?? "").Trim().Split(new char[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);
        }

        private static int IndexOf(string[] tokens, string targetWord, int startIndex)
        {
            for (int i = startIndex; i < tokens.Length; i++)
            {
                if (tokens[i] == targetWord)
                {
                    return i;
                }
            }
            return -1;
        }

        private static bool IsBopomofoToken(string token)
        {
            if (String.IsNullOrEmpty(token))
            {
                return false;
            }

            for (int i = 0; i < token.Length; i++)
            {
                char c = token[i];
                bool isBopomofo = (c >= '\u3100' && c <= '\u312F') || c == '\u02CA' || c == '\u02C7' || c == '\u02CB' || c == '\u02D9';
                if (!isBopomofo)
                {
                    return false;
                }
            }

            return true;
        }

        private sealed class PinyiLineMatch
        {
            public PinyiLineMatch(int targetIndex, List<string> candidates)
            {
                TargetIndex = targetIndex;
                Candidates = candidates;
            }

            public int TargetIndex { get; private set; }
            public List<string> Candidates { get; private set; }
        }
    }
}
