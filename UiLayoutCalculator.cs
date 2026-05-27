using System;
using System.Drawing;

namespace uclliu
{
    public enum ShortModeWordLayoutKind
    {
        Hint,
        Candidates
    }

    public struct ShortModeLabelLayout
    {
        public ShortModeLabelLayout(int width, bool visible)
        {
            Width = width;
            Visible = visible;
        }

        public int Width { get; private set; }
        public bool Visible { get; private set; }
    }

    public static class UiLayoutCalculator
    {
        public static int ShortModeTextWidth(string text, double zoom, int charWidth, int minWidth, int maxWidth)
        {
            if (text == null)
            {
                text = "";
            }
            if (zoom <= 0)
            {
                zoom = 1;
            }
            if (charWidth < 1)
            {
                charWidth = 1;
            }
            if (maxWidth < minWidth)
            {
                maxWidth = minWidth;
            }

            int width = Convert.ToInt32(text.Length * charWidth * zoom);
            if (width < minWidth)
            {
                return minWidth;
            }
            if (width > maxWidth)
            {
                return maxWidth;
            }
            return width;
        }

        public static ShortModeLabelLayout ShortModeTypeLayout(string text, double zoom, int maxWidth)
        {
            return ShortModeLayout(text, zoom, 18, maxWidth);
        }

        public static ShortModeLabelLayout ShortModeWordLayout(string text, double zoom, ShortModeWordLayoutKind kind, bool hasMorePage, int maxWidth)
        {
            int charWidth = 15;
            if (kind == ShortModeWordLayoutKind.Candidates)
            {
                charWidth = hasMorePage ? 13 : 12;
            }

            return ShortModeLayout(text, zoom, charWidth, maxWidth);
        }

        private static ShortModeLabelLayout ShortModeLayout(string text, double zoom, int charWidth, int maxWidth)
        {
            bool visible = !String.IsNullOrEmpty(text);
            int width = visible ? ShortModeTextWidth(text, zoom, charWidth, 0, maxWidth) : 0;
            return new ShortModeLabelLayout(width, visible);
        }
    }

    public sealed class UiLabelUpdateSnapshot
    {
        public bool UpdateType { get; set; }
        public string TypeText { get; set; }
        public Color TypeColor { get; set; }
        public bool UpdateWord { get; set; }
        public string WordText { get; set; }
        public bool WordHasColor { get; set; }
        public Color WordColor { get; set; }
        public ShortModeWordLayoutKind WordLayoutKind { get; set; }
        public bool WordHasMorePage { get; set; }
    }

    public sealed class UiLabelUpdateBatcher
    {
        private readonly Action<Action> post;
        private readonly Action<UiLabelUpdateSnapshot> apply;
        private bool scheduled;
        private bool updateType;
        private string typeText = "";
        private Color typeColor = Color.Black;
        private bool updateWord;
        private string wordText = "";
        private bool wordHasColor;
        private Color wordColor = Color.Black;
        private ShortModeWordLayoutKind wordLayoutKind = ShortModeWordLayoutKind.Hint;
        private bool wordHasMorePage;

        public UiLabelUpdateBatcher(Action<Action> post, Action<UiLabelUpdateSnapshot> apply)
        {
            this.post = post;
            this.apply = apply;
        }

        public void QueueType(string text, Color color)
        {
            updateType = true;
            typeText = text ?? "";
            typeColor = color;
            Schedule();
        }

        public void QueueWord(string text, Color? color, ShortModeWordLayoutKind layoutKind, bool hasMorePage)
        {
            updateWord = true;
            wordText = text ?? "";
            wordHasColor = color.HasValue;
            if (color.HasValue)
            {
                wordColor = color.Value;
            }
            wordLayoutKind = layoutKind;
            wordHasMorePage = hasMorePage;
            Schedule();
        }

        private void Schedule()
        {
            if (scheduled)
            {
                return;
            }

            scheduled = true;
            if (post != null)
            {
                post(Flush);
            }
            else
            {
                Flush();
            }
        }

        private void Flush()
        {
            UiLabelUpdateSnapshot snapshot = new UiLabelUpdateSnapshot();
            snapshot.UpdateType = updateType;
            snapshot.TypeText = typeText;
            snapshot.TypeColor = typeColor;
            snapshot.UpdateWord = updateWord;
            snapshot.WordText = wordText;
            snapshot.WordHasColor = wordHasColor;
            snapshot.WordColor = wordColor;
            snapshot.WordLayoutKind = wordLayoutKind;
            snapshot.WordHasMorePage = wordHasMorePage;

            scheduled = false;
            updateType = false;
            updateWord = false;

            if (apply != null)
            {
                apply(snapshot);
            }
        }
    }
}
