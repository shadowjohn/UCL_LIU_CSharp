using System;
using System.Collections.Generic;
using System.Text;

namespace uclliu
{
    public sealed class SmartCandidateSession
    {
        private const int MaxContextScalars = 3;
        private const int MaxChineseRunScalars = MaxContextScalars + SmartCandidateMemory.MaxCandidateLength;
        private readonly SmartCandidateTable _table;
        private readonly SmartCandidateMemory _memory;
        private readonly int _pageSize;
        private readonly List<string> _candidates = new List<string>();
        private readonly List<string> _visibleCandidates = new List<string>();
        private bool _enabled;
        private bool _continuousEnabled;
        private int _pageOffset;
        private string _contextKey = "";
        private string _chineseRun = "";

        public SmartCandidateSession(SmartCandidateTable table, SmartCandidateMemory memory, int pageSize = 5)
        {
            if (table == null)
            {
                throw new ArgumentNullException("table");
            }
            if (memory == null)
            {
                throw new ArgumentNullException("memory");
            }
            if (pageSize <= 0)
            {
                throw new ArgumentOutOfRangeException("pageSize");
            }

            _table = table;
            _memory = memory;
            _pageSize = pageSize;
            _enabled = table.IsAvailable;
            _continuousEnabled = true;
            LastActivityUtc = DateTime.UtcNow;
        }

        public bool Enabled
        {
            get { return _enabled; }
            set
            {
                _enabled = value;
                RefreshCandidates();
            }
        }

        public bool ContinuousEnabled
        {
            get { return _continuousEnabled; }
            set
            {
                _continuousEnabled = value;
                RefreshCandidates();
            }
        }

        public string Context { get; private set; } = "";

        public IList<string> VisibleCandidates
        {
            get { return new List<string>(_visibleCandidates).AsReadOnly(); }
        }

        public bool HasNextPage
        {
            get { return _pageOffset + _visibleCandidates.Count < _candidates.Count; }
        }

        public DateTime LastActivityUtc { get; private set; }

        public void ObserveCommittedText(string text)
        {
            if (!_enabled || string.IsNullOrEmpty(text))
            {
                return;
            }

            for (int i = 0; i < text.Length;)
            {
                int scalarLength = GetScalarLength(text, i);
                int scalar = scalarLength == 2 ? char.ConvertToUtf32(text, i) : text[i];
                string scalarText = text.Substring(i, scalarLength);
                i += scalarLength;
                if (IsChinese(scalar))
                {
                    _chineseRun = AppendBoundedScalars(_chineseRun, scalarText, MaxChineseRunScalars);
                    Context = AppendBoundedScalars(Context, scalarText, MaxContextScalars);
                    _memory.ObserveSequence(_chineseRun);
                    RefreshCandidates();
                }
                else if (scalar == '，')
                {
                    RefreshCandidates();
                }
                else if (IsSentenceBoundary(scalar))
                {
                    ClearContext();
                }
                else
                {
                    // 非中文會中斷連續學習，避免隔著垃圾字元仍被視為同一詞組。
                    _chineseRun = "";
                    Cancel();
                }
            }

            LastActivityUtc = DateTime.UtcNow;
        }

        public string Select(int oneBasedIndex)
        {
            if (oneBasedIndex < 1 || oneBasedIndex > _visibleCandidates.Count || string.IsNullOrEmpty(_contextKey))
            {
                return "";
            }

            string selected = _visibleCandidates[oneBasedIndex - 1];
            string selectedContextKey = _contextKey;
            _memory.RecordPredictionChoice(selectedContextKey, selected);
            ObserveCommittedText(selected);
            return selected;
        }

        public bool NextPage()
        {
            if (!HasNextPage)
            {
                return false;
            }

            _pageOffset += _pageSize;
            UpdateVisiblePage();
            LastActivityUtc = DateTime.UtcNow;
            return true;
        }

        public void Cancel()
        {
            _candidates.Clear();
            _visibleCandidates.Clear();
            _contextKey = "";
            _pageOffset = 0;
        }

        public void EndContext()
        {
            ClearContext();
        }

        public bool ShouldFlush(DateTime utcNow, TimeSpan idle)
        {
            if (utcNow - LastActivityUtc < idle)
            {
                return false;
            }

            EndContext();
            return _memory.IsDirty;
        }

        private void RefreshCandidates()
        {
            Cancel();
            if (!_enabled || !_continuousEnabled || Context.Length == 0)
            {
                return;
            }

            int maxLength = Math.Min(MaxContextScalars, CountScalars(Context));
            for (int length = maxLength; length >= 1; length--)
            {
                string key = TakeLastScalars(Context, length);
                List<string> merged = _memory.GetPredictions(key);
                MergeUnique(merged, _table.Find(key));
                if (merged.Count > 0)
                {
                    _contextKey = key;
                    _candidates.AddRange(merged);
                    UpdateVisiblePage();
                    return;
                }
            }
        }

        private void UpdateVisiblePage()
        {
            _visibleCandidates.Clear();
            int end = Math.Min(_pageOffset + _pageSize, _candidates.Count);
            for (int i = _pageOffset; i < end; i++)
            {
                _visibleCandidates.Add(_candidates[i]);
            }
        }

        private void ClearContext()
        {
            Context = "";
            _chineseRun = "";
            Cancel();
        }

        private static string AppendBoundedScalars(string current, string value, int maxScalars)
        {
            return TakeLastScalars(current + value, maxScalars);
        }

        private static string TakeLastScalars(string value, int count)
        {
            int scalarCount = CountScalars(value);
            if (scalarCount <= count)
            {
                return value;
            }

            int skip = scalarCount - count;
            int index = 0;
            while (skip-- > 0)
            {
                index += GetScalarLength(value, index);
            }
            return value.Substring(index);
        }

        private static int CountScalars(string value)
        {
            int count = 0;
            for (int i = 0; i < value.Length; count++)
            {
                i += GetScalarLength(value, i);
            }
            return count;
        }

        private static int GetScalarLength(string value, int index)
        {
            return char.IsHighSurrogate(value[index])
                && index + 1 < value.Length
                && char.IsLowSurrogate(value[index + 1])
                ? 2
                : 1;
        }

        private static void MergeUnique(List<string> target, IEnumerable<string> source)
        {
            foreach (string candidate in source)
            {
                if (!target.Contains(candidate))
                {
                    target.Add(candidate);
                }
            }
        }

        private static bool IsChinese(int value)
        {
            return (value >= '\u3400' && value <= '\u4dbf')
                || (value >= '\u4e00' && value <= '\u9fff')
                || (value >= '\uf900' && value <= '\ufaff')
                || (value >= 0x20000 && value <= 0x2ebef)
                || (value >= 0x2f800 && value <= 0x2fa1f)
                || (value >= 0x30000 && value <= 0x323af);
        }

        private static bool IsSentenceBoundary(int value)
        {
            return value == '。' || value == '！' || value == '？' || value == '；' || value == '\r' || value == '\n';
        }
    }

    public static class SmartCandidateSettings
    {
        public static void EnsureDefaults(SimpleIniData config)
        {
            if (config == null)
            {
                throw new ArgumentNullException("config");
            }

            EnsureDefault(config["DEFAULT"], "SMART_CANDIDATE_ENABLE");
            EnsureDefault(config["DEFAULT"], "SMART_CANDIDATE_CONTINUOUS");
            EnsureDefault(config["DEFAULT"], "SMART_ROOT_ENABLE");
        }

        public static bool IsEnabled(string value)
        {
            return value == "1";
        }

        private static void EnsureDefault(SimpleIniSection section, string key)
        {
            if (!section.ContainsKey(key))
            {
                section[key] = "1";
            }
        }
    }

    public static class SmartCandidateDisplay
    {
        public static string Format(IEnumerable<string> candidates, bool hasNextPage)
        {
            StringBuilder text = new StringBuilder();
            int number = 1;
            if (candidates != null)
            {
                foreach (string candidate in candidates)
                {
                    if (text.Length > 0)
                    {
                        text.Append(' ');
                    }
                    text.Append(number++).Append(candidate);
                }
            }
            if (hasNextPage && text.Length > 0)
            {
                text.Append(" ...");
            }
            return text.ToString();
        }
    }
}
