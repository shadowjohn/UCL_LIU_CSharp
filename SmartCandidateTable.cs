using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace uclliu
{
    public sealed class SmartCandidateTable
    {
        private readonly Dictionary<string, List<string>> candidatesByContext;

        private SmartCandidateTable(Dictionary<string, List<string>> candidatesByContext, int invalidLineCount)
        {
            this.candidatesByContext = candidatesByContext;
            InvalidLineCount = invalidLineCount;
        }

        public int InvalidLineCount { get; private set; }

        public bool IsAvailable
        {
            get { return candidatesByContext.Count > 0; }
        }

        public static SmartCandidateTable Empty()
        {
            return new SmartCandidateTable(
                new Dictionary<string, List<string>>(StringComparer.Ordinal),
                0);
        }

        public static SmartCandidateTable Load(string path)
        {
            return File.Exists(path) ? Parse(File.ReadAllLines(path, Encoding.UTF8)) : Empty();
        }

        public static SmartCandidateTable Parse(IEnumerable<string> lines)
        {
            if (lines == null)
            {
                return Empty();
            }

            Dictionary<string, List<string>> candidatesByContext =
                new Dictionary<string, List<string>>(StringComparer.Ordinal);
            int invalidLineCount = 0;

            foreach (string sourceLine in lines)
            {
                string line = sourceLine ?? "";
                string trimmedLine = line.Trim();
                if (trimmedLine.Length == 0 || trimmedLine.StartsWith("#", StringComparison.Ordinal))
                {
                    continue;
                }

                string[] fields = line.Split('\t');
                string context = fields[0].Trim();
                List<string> rowCandidates = new List<string>();
                for (int i = 1; i < fields.Length; i++)
                {
                    string candidate = fields[i].Trim();
                    if (candidate.Length > 0 && CountScalars(candidate) <= 3 && !rowCandidates.Contains(candidate))
                    {
                        rowCandidates.Add(candidate);
                    }
                }

                if (context.Length == 0 || rowCandidates.Count == 0)
                {
                    invalidLineCount++;
                    continue;
                }

                List<string> candidates;
                if (!candidatesByContext.TryGetValue(context, out candidates))
                {
                    candidates = new List<string>();
                    candidatesByContext.Add(context, candidates);
                }

                for (int i = 0; i < rowCandidates.Count; i++)
                {
                    if (!candidates.Contains(rowCandidates[i]))
                    {
                        candidates.Add(rowCandidates[i]);
                    }
                }
            }

            return new SmartCandidateTable(candidatesByContext, invalidLineCount);
        }

        public List<string> Find(string context)
        {
            List<string> candidates;
            if (String.IsNullOrEmpty(context) || !candidatesByContext.TryGetValue(context, out candidates))
            {
                return new List<string>();
            }
            return new List<string>(candidates);
        }

        private static int CountScalars(string value)
        {
            int count = 0;
            for (int i = 0; i < value.Length; count++)
            {
                i += char.IsHighSurrogate(value[i])
                    && i + 1 < value.Length
                    && char.IsLowSurrogate(value[i + 1]) ? 2 : 1;
            }
            return count;
        }
    }
}
