using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Json;
using System.Xml;

namespace uclliu
{
    public sealed class SmartCandidateMemory
    {
        // 每個情境最多保留 16 個候選，避免單一輸入污染整份記憶。
        public const int MaxCandidatesPerKey = 16;
        // 預測與字根各最多 1024 筆，讓 JSON 固定低於序列化器物件數上限。
        public const int MaxEntriesPerScope = 1024;
        // key 與候選各限制 64 字元，避免單筆異常輸入放大記憶檔。
        public const int MaxKeyLength = 64;
        public const int MaxCandidateLength = 64;

        private readonly Dictionary<string, Dictionary<string, Entry>> _predictions;
        private readonly Dictionary<string, Dictionary<string, Entry>> _roots;
        private int _predictionCount;
        private int _rootCount;
        private long _nextOrder;

        public SmartCandidateMemory()
        {
            _predictions = NewScope();
            _roots = NewScope();
        }

        public bool IsDirty { get; private set; }

        public void ObserveSequence(string text)
        {
            if (string.IsNullOrEmpty(text))
            {
                return;
            }

            List<int> scalarStarts;
            if (!TryGetScalarStarts(text, out scalarStarts))
            {
                return;
            }

            int firstSuffix = Math.Max(1, scalarStarts.Count - MaxCandidateLength);
            for (int suffixStart = firstSuffix; suffixStart < scalarStarts.Count; suffixStart++)
            {
                int suffixIndex = scalarStarts[suffixStart];
                string candidate = text.Substring(suffixIndex);
                int maxContextLength = Math.Min(3, suffixStart);
                for (int contextLength = 1; contextLength <= maxContextLength; contextLength++)
                {
                    int contextIndex = scalarStarts[suffixStart - contextLength];
                    AddScore(_predictions, text.Substring(contextIndex, suffixIndex - contextIndex), candidate, 1);
                }
            }
        }

        public void RecordPredictionChoice(string context, string candidate)
        {
            AddScore(_predictions, context, candidate, 5);
        }

        public void RecordRootChoice(string root, string candidate)
        {
            AddScore(_roots, root, candidate, 5);
        }

        public List<string> GetPredictions(string context)
        {
            Dictionary<string, Entry> candidates;
            if (string.IsNullOrEmpty(context) || !_predictions.TryGetValue(context, out candidates))
            {
                return new List<string>();
            }

            List<Entry> ranked = new List<Entry>(candidates.Values);
            ranked.Sort(CompareForRanking);
            List<string> result = new List<string>(ranked.Count);
            foreach (Entry entry in ranked)
            {
                result.Add(entry.Candidate);
            }
            return result;
        }

        public List<string> RankRootCandidates(string root, IEnumerable<string> original)
        {
            List<RankedCandidate> ranked = new List<RankedCandidate>();
            if (original == null)
            {
                return new List<string>();
            }

            Dictionary<string, Entry> known;
            _roots.TryGetValue(root ?? string.Empty, out known);
            int index = 0;
            foreach (string candidate in original)
            {
                Entry entry = null;
                if (known != null && candidate != null)
                {
                    known.TryGetValue(candidate, out entry);
                }
                ranked.Add(new RankedCandidate(candidate, entry, index++));
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

        internal CandidateMemoryData ToData()
        {
            return new CandidateMemoryData
            {
                Version = 1,
                NextOrder = _nextOrder,
                Predictions = ToData(_predictions),
                Roots = ToData(_roots)
            };
        }

        internal static SmartCandidateMemory FromData(CandidateMemoryData data)
        {
            SmartCandidateMemory memory = new SmartCandidateMemory();
            if (data == null)
            {
                return memory;
            }

            long nextOrder = 0;
            memory.LoadEntries(memory._predictions, data.Predictions, ref nextOrder);
            memory.LoadEntries(memory._roots, data.Roots, ref nextOrder);
            memory._nextOrder = Math.Max(data.NextOrder, nextOrder);
            memory.TrimScope(memory._predictions);
            memory.TrimScope(memory._roots);
            memory.MarkSaved();
            return memory;
        }

        private static Dictionary<string, Dictionary<string, Entry>> NewScope()
        {
            return new Dictionary<string, Dictionary<string, Entry>>(StringComparer.Ordinal);
        }

        private void AddScore(Dictionary<string, Dictionary<string, Entry>> scope, string key, string candidate, int amount)
        {
            if (string.IsNullOrEmpty(key) || !HasValidScalarLength(key, MaxKeyLength)
                || string.IsNullOrEmpty(candidate) || !HasValidScalarLength(candidate, MaxCandidateLength))
            {
                return;
            }

            Dictionary<string, Entry> candidates;
            if (!scope.TryGetValue(key, out candidates))
            {
                candidates = new Dictionary<string, Entry>(StringComparer.Ordinal);
                scope.Add(key, candidates);
            }

            Entry entry;
            long order = NextOrder();
            if (!candidates.TryGetValue(candidate, out entry))
            {
                entry = new Entry(key, candidate, 0, order, order);
                candidates.Add(candidate, entry);
                AdjustCount(scope, 1);
            }
            else
            {
                entry.LastUsed = order;
            }
            entry.Score = entry.Score > int.MaxValue - amount ? int.MaxValue : entry.Score + amount;
            TrimBucket(scope, key, candidates);
            TrimScope(scope);
            IsDirty = true;
        }

        private long NextOrder()
        {
            return _nextOrder == long.MaxValue ? long.MaxValue : _nextOrder++;
        }

        private void TrimBucket(Dictionary<string, Dictionary<string, Entry>> scope, string key, Dictionary<string, Entry> candidates)
        {
            while (candidates.Count > MaxCandidatesPerKey)
            {
                candidates.Remove(FindWorst(candidates.Values).Candidate);
                AdjustCount(scope, -1);
            }
            if (candidates.Count == 0)
            {
                scope.Remove(key);
            }
        }

        private void TrimScope(Dictionary<string, Dictionary<string, Entry>> scope)
        {
            while (GetCount(scope) > MaxEntriesPerScope)
            {
                Entry worst = FindWorst(AllEntries(scope));
                Dictionary<string, Entry> candidates = scope[worst.Key];
                candidates.Remove(worst.Candidate);
                AdjustCount(scope, -1);
                if (candidates.Count == 0)
                {
                    scope.Remove(worst.Key);
                }
            }
        }

        private static Entry FindWorst(IEnumerable<Entry> entries)
        {
            Entry worst = null;
            foreach (Entry entry in entries)
            {
                if (worst == null || CompareForRetention(entry, worst) < 0)
                {
                    worst = entry;
                }
            }
            return worst;
        }

        private static int CompareForRetention(Entry left, Entry right)
        {
            int score = left.Score.CompareTo(right.Score);
            if (score != 0)
            {
                return score;
            }
            int recent = left.LastUsed.CompareTo(right.LastUsed);
            if (recent != 0)
            {
                return recent;
            }
            int firstSeen = right.FirstSeen.CompareTo(left.FirstSeen);
            if (firstSeen != 0)
            {
                return firstSeen;
            }
            int key = string.CompareOrdinal(right.Key, left.Key);
            return key != 0 ? key : string.CompareOrdinal(right.Candidate, left.Candidate);
        }

        private static int CompareForRanking(Entry left, Entry right)
        {
            int score = right.Score.CompareTo(left.Score);
            if (score != 0)
            {
                return score;
            }
            int firstSeen = left.FirstSeen.CompareTo(right.FirstSeen);
            return firstSeen != 0 ? firstSeen : string.CompareOrdinal(left.Candidate, right.Candidate);
        }

        private static int CompareRankedCandidates(RankedCandidate left, RankedCandidate right)
        {
            int leftScore = left.Entry == null ? 0 : left.Entry.Score;
            int rightScore = right.Entry == null ? 0 : right.Entry.Score;
            int score = rightScore.CompareTo(leftScore);
            if (score != 0)
            {
                return score;
            }
            long leftOrder = left.Entry == null ? long.MaxValue : left.Entry.FirstSeen;
            long rightOrder = right.Entry == null ? long.MaxValue : right.Entry.FirstSeen;
            int order = leftOrder.CompareTo(rightOrder);
            return order != 0 ? order : left.OriginalIndex.CompareTo(right.OriginalIndex);
        }

        private int GetCount(Dictionary<string, Dictionary<string, Entry>> scope)
        {
            return object.ReferenceEquals(scope, _predictions) ? _predictionCount : _rootCount;
        }

        private void AdjustCount(Dictionary<string, Dictionary<string, Entry>> scope, int amount)
        {
            if (object.ReferenceEquals(scope, _predictions))
            {
                _predictionCount += amount;
            }
            else
            {
                _rootCount += amount;
            }
        }

        private static List<Entry> AllEntries(Dictionary<string, Dictionary<string, Entry>> scope)
        {
            List<Entry> entries = new List<Entry>();
            foreach (Dictionary<string, Entry> candidates in scope.Values)
            {
                entries.AddRange(candidates.Values);
            }
            return entries;
        }

        private static List<CandidateEntryData> ToData(Dictionary<string, Dictionary<string, Entry>> scope)
        {
            List<Entry> entries = AllEntries(scope);
            entries.Sort(delegate(Entry left, Entry right)
            {
                int firstSeen = left.FirstSeen.CompareTo(right.FirstSeen);
                if (firstSeen != 0)
                {
                    return firstSeen;
                }
                int key = string.CompareOrdinal(left.Key, right.Key);
                return key != 0 ? key : string.CompareOrdinal(left.Candidate, right.Candidate);
            });

            List<CandidateEntryData> data = new List<CandidateEntryData>(entries.Count);
            foreach (Entry entry in entries)
            {
                data.Add(new CandidateEntryData
                {
                    Key = entry.Key,
                    Candidate = entry.Candidate,
                    Score = entry.Score,
                    FirstSeen = entry.FirstSeen,
                    LastUsed = entry.LastUsed
                });
            }
            return data;
        }

        private void LoadEntries(Dictionary<string, Dictionary<string, Entry>> scope, List<CandidateEntryData> data, ref long nextOrder)
        {
            if (data == null)
            {
                return;
            }
            foreach (CandidateEntryData item in data)
            {
                if (item == null || string.IsNullOrEmpty(item.Key) || !HasValidScalarLength(item.Key, MaxKeyLength)
                    || string.IsNullOrEmpty(item.Candidate) || !HasValidScalarLength(item.Candidate, MaxCandidateLength)
                    || item.Score <= 0)
                {
                    continue;
                }

                Dictionary<string, Entry> candidates;
                if (!scope.TryGetValue(item.Key, out candidates))
                {
                    candidates = new Dictionary<string, Entry>(StringComparer.Ordinal);
                    scope.Add(item.Key, candidates);
                }
                Entry existing;
                if (!candidates.TryGetValue(item.Candidate, out existing))
                {
                    candidates.Add(item.Candidate, new Entry(item.Key, item.Candidate, item.Score, item.FirstSeen, item.LastUsed));
                    AdjustCount(scope, 1);
                }
                else
                {
                    existing.Score = Math.Max(existing.Score, item.Score);
                    existing.FirstSeen = Math.Min(existing.FirstSeen, item.FirstSeen);
                    existing.LastUsed = Math.Max(existing.LastUsed, item.LastUsed);
                }
                long itemOrder = Math.Max(item.FirstSeen, item.LastUsed);
                nextOrder = Math.Max(nextOrder, itemOrder == long.MaxValue ? long.MaxValue : itemOrder + 1);
                TrimBucket(scope, item.Key, candidates);
            }
        }

        private static bool TryGetScalarStarts(string value, out List<int> starts)
        {
            starts = new List<int>();
            for (int i = 0; i < value.Length;)
            {
                starts.Add(i);
                if (char.IsHighSurrogate(value[i]))
                {
                    if (i + 1 >= value.Length || !char.IsLowSurrogate(value[i + 1]))
                    {
                        return false;
                    }
                    i += 2;
                }
                else
                {
                    if (char.IsLowSurrogate(value[i]))
                    {
                        return false;
                    }
                    i++;
                }
            }
            return true;
        }

        private static bool HasValidScalarLength(string value, int maxLength)
        {
            int count = 0;
            for (int i = 0; i < value.Length;)
            {
                if (++count > maxLength)
                {
                    return false;
                }
                if (char.IsHighSurrogate(value[i]))
                {
                    if (i + 1 >= value.Length || !char.IsLowSurrogate(value[i + 1]))
                    {
                        return false;
                    }
                    i += 2;
                }
                else
                {
                    if (char.IsLowSurrogate(value[i]))
                    {
                        return false;
                    }
                    i++;
                }
            }
            return true;
        }

        private sealed class Entry
        {
            public Entry(string key, string candidate, int score, long firstSeen, long lastUsed)
            {
                Key = key;
                Candidate = candidate;
                Score = score;
                FirstSeen = firstSeen;
                LastUsed = lastUsed;
            }

            public string Key { get; private set; }
            public string Candidate { get; private set; }
            public int Score { get; set; }
            public long FirstSeen { get; set; }
            public long LastUsed { get; set; }
        }

        private sealed class RankedCandidate
        {
            public RankedCandidate(string candidate, Entry entry, int originalIndex)
            {
                Candidate = candidate;
                Entry = entry;
                OriginalIndex = originalIndex;
            }

            public string Candidate { get; private set; }
            public Entry Entry { get; private set; }
            public int OriginalIndex { get; private set; }
        }
    }

    [DataContract]
    internal sealed class CandidateMemoryData
    {
        [DataMember(Name = "version", Order = 0)]
        public int Version { get; set; }

        [DataMember(Name = "nextOrder", Order = 1)]
        public long NextOrder { get; set; }

        [DataMember(Name = "predictions", Order = 2)]
        public List<CandidateEntryData> Predictions { get; set; }

        [DataMember(Name = "roots", Order = 3)]
        public List<CandidateEntryData> Roots { get; set; }
    }

    [DataContract]
    internal sealed class CandidateEntryData
    {
        [DataMember(Name = "key", Order = 0)]
        public string Key { get; set; }

        [DataMember(Name = "candidate", Order = 1)]
        public string Candidate { get; set; }

        [DataMember(Name = "score", Order = 2)]
        public int Score { get; set; }

        [DataMember(Name = "firstSeen", Order = 3)]
        public long FirstSeen { get; set; }

        [DataMember(Name = "lastUsed", Order = 4)]
        public long LastUsed { get; set; }
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
                using (FileStream stream = File.OpenRead(path))
                {
                    CandidateMemoryData data = CreateSerializer().ReadObject(stream) as CandidateMemoryData;
                    if (data == null)
                    {
                        throw new SerializationException("Smart candidate memory JSON is empty.");
                    }
                    return SmartCandidateMemory.FromData(data);
                }
            }
            catch (SerializationException)
            {
                BackupBroken(path);
                return new SmartCandidateMemory();
            }
            catch (XmlException)
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
                using (FileStream stream = File.Create(tempPath))
                {
                    CreateSerializer().WriteObject(stream, memory.ToData());
                }
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

        public static bool TryClearAtomic(string path, out string error)
        {
            try
            {
                SaveAtomic(path, new SmartCandidateMemory());
                error = "";
                return true;
            }
            catch (Exception ex)
            {
                error = ex.Message;
                return false;
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

        private static DataContractJsonSerializer CreateSerializer()
        {
            return new DataContractJsonSerializer(typeof(CandidateMemoryData));
        }
    }
}
