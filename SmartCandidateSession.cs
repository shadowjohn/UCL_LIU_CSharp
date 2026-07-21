using System;
using System.Collections.Generic;
using System.Text;

namespace uclliu
{
    public sealed class SmartCandidateSession
    {
        private const int MaxContextScalars = 3;
        private readonly SmartCandidateTable _table;
        private readonly SmartCandidateMemory _memory;
        private readonly int _pageSize;
        private readonly List<string> _candidates = new List<string>();
        private readonly List<string> _visibleCandidates = new List<string>();
        private bool _enabled;
        private bool _continuousEnabled;
        private int _pageOffset;
        private string _contextKey = "";

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
                    Context = AppendBoundedScalars(Context, scalarText, MaxContextScalars);
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
                    // 非中文會中斷候選上下文，避免跨越無關字元繼續預測。
                    Cancel();
                }
            }

            LastActivityUtc = DateTime.UtcNow;
        }

        public string Select(int oneBasedIndex)
        {
            SmartCandidateSelection selection = PrepareSelection(oneBasedIndex);
            if (selection == null)
            {
                return "";
            }

            CommitSelection(selection, selection.Text);
            return selection.Text;
        }

        public SmartCandidateSelection PrepareSelection(int oneBasedIndex)
        {
            if (oneBasedIndex < 1 || oneBasedIndex > _visibleCandidates.Count || string.IsNullOrEmpty(_contextKey))
            {
                return null;
            }
            return new SmartCandidateSelection(_contextKey, _visibleCandidates[oneBasedIndex - 1]);
        }

        internal bool CommitSelection(SmartCandidateSelection selection, string committedText)
        {
            if (selection == null || selection.IsCommitted)
            {
                return false;
            }
            ObserveCommittedText(committedText);
            selection.IsCommitted = true;
            return true;
        }

        public bool NextPage()
        {
            if (!HasNextPage)
            {
                return false;
            }

            _pageOffset += _visibleCandidates.Count;
            UpdateVisiblePage();
            LastActivityUtc = DateTime.UtcNow;
            return true;
        }

        public void LimitCurrentPage(int count)
        {
            if (count < 1)
            {
                count = 1;
            }
            if (count > _pageSize)
            {
                count = _pageSize;
            }
            UpdateVisiblePage(count);
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
                List<string> candidates = _table.Find(key);
                for (int i = 0; i < candidates.Count; i++)
                {
                    string candidate = candidates[i];
                    if (!Context.EndsWith(candidate, StringComparison.Ordinal))
                    {
                        _candidates.Add(candidate);
                    }
                }
                if (_candidates.Count > 0)
                {
                    _contextKey = key;
                    UpdateVisiblePage();
                    return;
                }
            }
        }

        private void UpdateVisiblePage()
        {
            UpdateVisiblePage(_pageSize);
        }

        private void UpdateVisiblePage(int count)
        {
            _visibleCandidates.Clear();
            int end = Math.Min(_pageOffset + count, _candidates.Count);
            for (int i = _pageOffset; i < end; i++)
            {
                _visibleCandidates.Add(_candidates[i]);
            }
        }

        private void ClearContext()
        {
            Context = "";
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

    public sealed class SmartCandidateSelection
    {
        internal SmartCandidateSelection(string contextKey, string text)
        {
            ContextKey = contextKey;
            Text = text;
        }

        internal string ContextKey { get; private set; }
        internal bool IsCommitted { get; set; }
        public string Text { get; private set; }
    }

    public sealed class SmartCandidateRootChoice
    {
        private SmartCandidateRootChoice(string root, string candidate)
        {
            Root = root;
            Candidate = candidate;
        }

        internal string Root { get; private set; }
        internal string Candidate { get; private set; }

        public static SmartCandidateRootChoice Capture(string root, string candidate, IEnumerable<string> candidates)
        {
            if (string.IsNullOrEmpty(root) || string.IsNullOrEmpty(candidate) || candidates == null)
            {
                return null;
            }
            foreach (string available in candidates)
            {
                if (available == candidate)
                {
                    return new SmartCandidateRootChoice(root, candidate);
                }
            }
            return null;
        }
    }

    public sealed class SmartCandidateOutputCommit
    {
        private readonly SmartCandidateRootChoice _rootChoice;
        private readonly SmartCandidateSelection _selection;
        private bool _completed;

        private SmartCandidateOutputCommit(SmartCandidateRootChoice rootChoice, SmartCandidateSelection selection)
        {
            _rootChoice = rootChoice;
            _selection = selection;
        }

        public static SmartCandidateOutputCommit ForNormal(SmartCandidateRootChoice rootChoice)
        {
            return new SmartCandidateOutputCommit(rootChoice, null);
        }

        public static SmartCandidateOutputCommit ForSelection(SmartCandidateSelection selection)
        {
            return selection == null ? null : new SmartCandidateOutputCommit(null, selection);
        }

        public bool Complete(bool outputSucceeded, SmartCandidateSession session, SmartCandidateMemory memory, string preparedText)
        {
            if (!outputSucceeded || _completed || session == null || memory == null)
            {
                return false;
            }

            if (_selection != null)
            {
                if (!session.CommitSelection(_selection, preparedText))
                {
                    return false;
                }
            }
            else
            {
                session.ObserveCommittedText(preparedText);
            }
            _completed = true;
            return true;
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

            SimpleIniSection section = config["DEFAULT"];
            if (section["SMART_CANDIDATE_POLICY_VERSION"] != "2")
            {
                section["SMART_CANDIDATE_ENABLE"] = "0";
                section["SMART_CANDIDATE_CONTINUOUS"] = "1";
                section["SMART_ROOT_ENABLE"] = "0";
                section["SMART_CANDIDATE_POLICY_VERSION"] = "2";
            }
        }

        public static void ApplyLoadedPolicy(SimpleIniData config, SimpleIniData loaded)
        {
            if (config == null)
            {
                throw new ArgumentNullException("config");
            }
            if (loaded == null)
            {
                throw new ArgumentNullException("loaded");
            }

            foreach (KeyValuePair<string, string> key in loaded["DEFAULT"].Keys)
            {
                config["DEFAULT"][key.Key] = key.Value.Trim();
            }

            // 必須看載入檔本身的版本，不能被啟動時預先填入的 v2 預設掩蓋。
            if (loaded["DEFAULT"]["SMART_CANDIDATE_POLICY_VERSION"] != "2")
            {
                config["DEFAULT"]["SMART_CANDIDATE_POLICY_VERSION"] = "1";
            }
            EnsureDefaults(config);
        }

        public static bool IsEnabled(string value)
        {
            return value == "1";
        }

        public static bool ShouldUseSmartRoot(string totalEnabled, string rootEnabled, bool tableAvailable, bool sessionEnabled)
        {
            return IsEnabled(totalEnabled) && IsEnabled(rootEnabled) && tableAvailable && sessionEnabled;
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
