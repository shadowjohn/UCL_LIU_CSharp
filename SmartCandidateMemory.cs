using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Web.Script.Serialization;

namespace uclliu
{
    public sealed class SmartCandidateMemoryEntry
    {
        public string Key { get; set; }
        public string Candidate { get; set; }
        public int Score { get; set; }
        public int Order { get; set; }
    }

    public sealed class SmartCandidateMemory
    {
        public SmartCandidateMemory()
        {
            PredictionEntries = new List<SmartCandidateMemoryEntry>();
            RootEntries = new List<SmartCandidateMemoryEntry>();
        }

        public List<SmartCandidateMemoryEntry> PredictionEntries { get; set; }
        public List<SmartCandidateMemoryEntry> RootEntries { get; set; }

        [ScriptIgnore]
        public bool IsDirty { get; private set; }

        public void ObserveSequence(string text)
        {
            EnsureLists();
            if (string.IsNullOrEmpty(text))
            {
                return;
            }

            for (int suffixStart = 1; suffixStart < text.Length; suffixStart++)
            {
                string candidate = text.Substring(suffixStart);
                int maxContextLength = Math.Min(3, suffixStart);
                for (int contextLength = 1; contextLength <= maxContextLength; contextLength++)
                {
                    AddScore(PredictionEntries, text.Substring(suffixStart - contextLength, contextLength), candidate, 1);
                }
            }
        }

        public void RecordPredictionChoice(string context, string candidate)
        {
            EnsureLists();
            AddScore(PredictionEntries, context, candidate, 5);
        }

        public void RecordRootChoice(string root, string candidate)
        {
            EnsureLists();
            AddScore(RootEntries, root, candidate, 5);
        }

        public List<string> GetPredictions(string context)
        {
            EnsureLists();
            return RankKnown(PredictionEntries, context);
        }

        public List<string> RankRootCandidates(string root, IEnumerable<string> original)
        {
            EnsureLists();
            List<RankedCandidate> ranked = new List<RankedCandidate>();
            if (original == null)
            {
                return new List<string>();
            }

            int index = 0;
            foreach (string candidate in original)
            {
                SmartCandidateMemoryEntry entry = Find(RootEntries, root, candidate);
                ranked.Add(new RankedCandidate(candidate, entry == null ? 0 : entry.Score, entry == null ? int.MaxValue : entry.Order, index++));
            }

            ranked.Sort(CompareRankedCandidates);
            List<string> result = new List<string>(ranked.Count);
            foreach (RankedCandidate item in ranked)
            {
                result.Add(item.Candidate);
            }
            return result;
        }

        public void MarkSaved()
        {
            IsDirty = false;
        }

        private void AddScore(List<SmartCandidateMemoryEntry> entries, string key, string candidate, int score)
        {
            EnsureLists();
            if (string.IsNullOrEmpty(key) || string.IsNullOrEmpty(candidate))
            {
                return;
            }

            SmartCandidateMemoryEntry entry = Find(entries, key, candidate);
            if (entry == null)
            {
                entry = new SmartCandidateMemoryEntry
                {
                    Key = key,
                    Candidate = candidate,
                    Order = NextOrder()
                };
                entries.Add(entry);
            }
            entry.Score += score;
            IsDirty = true;
        }

        private List<string> RankKnown(List<SmartCandidateMemoryEntry> entries, string key)
        {
            EnsureLists();
            List<SmartCandidateMemoryEntry> matches = new List<SmartCandidateMemoryEntry>();
            foreach (SmartCandidateMemoryEntry entry in entries)
            {
                if (entry != null && string.Equals(entry.Key, key, StringComparison.Ordinal))
                {
                    matches.Add(entry);
                }
            }
            matches.Sort(delegate(SmartCandidateMemoryEntry left, SmartCandidateMemoryEntry right)
            {
                int score = right.Score.CompareTo(left.Score);
                return score != 0 ? score : left.Order.CompareTo(right.Order);
            });

            List<string> result = new List<string>(matches.Count);
            foreach (SmartCandidateMemoryEntry entry in matches)
            {
                result.Add(entry.Candidate);
            }
            return result;
        }

        private static SmartCandidateMemoryEntry Find(List<SmartCandidateMemoryEntry> entries, string key, string candidate)
        {
            if (entries == null)
            {
                return null;
            }
            foreach (SmartCandidateMemoryEntry entry in entries)
            {
                if (entry != null && string.Equals(entry.Key, key, StringComparison.Ordinal) && string.Equals(entry.Candidate, candidate, StringComparison.Ordinal))
                {
                    return entry;
                }
            }
            return null;
        }

        private int NextOrder()
        {
            int next = 0;
            next = NextOrder(PredictionEntries, next);
            return NextOrder(RootEntries, next);
        }

        private static int NextOrder(List<SmartCandidateMemoryEntry> entries, int next)
        {
            foreach (SmartCandidateMemoryEntry entry in entries)
            {
                if (entry != null && entry.Order >= next)
                {
                    next = entry.Order + 1;
                }
            }
            return next;
        }

        private void EnsureLists()
        {
            if (PredictionEntries == null)
            {
                PredictionEntries = new List<SmartCandidateMemoryEntry>();
            }
            if (RootEntries == null)
            {
                RootEntries = new List<SmartCandidateMemoryEntry>();
            }
        }

        private static int CompareRankedCandidates(RankedCandidate left, RankedCandidate right)
        {
            int score = right.Score.CompareTo(left.Score);
            if (score != 0)
            {
                return score;
            }
            int order = left.Order.CompareTo(right.Order);
            return order != 0 ? order : left.OriginalIndex.CompareTo(right.OriginalIndex);
        }

        private sealed class RankedCandidate
        {
            public RankedCandidate(string candidate, int score, int order, int originalIndex)
            {
                Candidate = candidate;
                Score = score;
                Order = order;
                OriginalIndex = originalIndex;
            }

            public string Candidate { get; private set; }
            public int Score { get; private set; }
            public int Order { get; private set; }
            public int OriginalIndex { get; private set; }
        }
    }

    public static class SmartCandidateMemoryStore
    {
        public static SmartCandidateMemory Load(string path)
        {
            if (!File.Exists(path))
            {
                return new SmartCandidateMemory();
            }

            try
            {
                SmartCandidateMemory memory = new JavaScriptSerializer().Deserialize<SmartCandidateMemory>(File.ReadAllText(path, Encoding.UTF8));
                if (memory == null)
                {
                    throw new InvalidOperationException("Smart candidate memory JSON is empty.");
                }
                memory.MarkSaved();
                return memory;
            }
            catch (InvalidOperationException)
            {
                BackupBroken(path);
                return new SmartCandidateMemory();
            }
            catch (ArgumentException)
            {
                BackupBroken(path);
                return new SmartCandidateMemory();
            }
        }

        public static void SaveAtomic(string path, SmartCandidateMemory memory)
        {
            if (memory == null)
            {
                throw new ArgumentNullException("memory");
            }

            string tempPath = path + ".tmp";
            try
            {
                File.WriteAllText(tempPath, new JavaScriptSerializer().Serialize(memory), new UTF8Encoding(false));
                if (File.Exists(path))
                {
                    File.Replace(tempPath, path, null);
                }
                else
                {
                    File.Move(tempPath, path);
                }
                memory.MarkSaved();
            }
            finally
            {
                if (File.Exists(tempPath))
                {
                    File.Delete(tempPath);
                }
            }
        }

        private static void BackupBroken(string path)
        {
            string brokenPath = path + ".broken";
            if (File.Exists(brokenPath))
            {
                File.Delete(brokenPath);
            }
            File.Move(path, brokenPath);
        }
    }
}
