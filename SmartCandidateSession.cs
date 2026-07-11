using System;
using System.Collections.Generic;

namespace uclliu
{
    public sealed class SmartCandidateSession
    {
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

            for (int i = 0; i < text.Length; i++)
            {
                char value = text[i];
                if (IsChinese(value))
                {
                    _chineseRun += value;
                    Context = AppendContext(Context, value.ToString());
                    _memory.ObserveSequence(_chineseRun);
                    RefreshCandidates();
                }
                else if (value == '，')
                {
                    RefreshCandidates();
                }
                else if (IsSentenceBoundary(value))
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
            return _memory.IsDirty && utcNow - LastActivityUtc >= idle;
        }

        private void RefreshCandidates()
        {
            Cancel();
            if (!_enabled || !_continuousEnabled || Context.Length == 0)
            {
                return;
            }

            int maxLength = Math.Min(3, Context.Length);
            for (int length = maxLength; length >= 1; length--)
            {
                string key = Context.Substring(Context.Length - length, length);
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

        private static string AppendContext(string context, string text)
        {
            string result = context + text;
            return result.Length <= 3 ? result : result.Substring(result.Length - 3);
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

        private static bool IsChinese(char value)
        {
            return (value >= '\u3400' && value <= '\u4dbf')
                || (value >= '\u4e00' && value <= '\u9fff')
                || (value >= '\uf900' && value <= '\ufaff');
        }

        private static bool IsSentenceBoundary(char value)
        {
            return value == '。' || value == '！' || value == '？' || value == '；' || value == '\r' || value == '\n';
        }
    }
}
