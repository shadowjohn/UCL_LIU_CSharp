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

    public struct ShortModeColumnState
    {
        public ShortModeColumnState(int typeWidth, bool typeVisible, int wordWidth, bool wordVisible)
        {
            TypeWidth = typeWidth;
            TypeVisible = typeVisible;
            WordWidth = wordWidth;
            WordVisible = wordVisible;
        }

        public int TypeWidth { get; private set; }
        public bool TypeVisible { get; private set; }
        public int WordWidth { get; private set; }
        public bool WordVisible { get; private set; }
    }

    public sealed class ShortModePackedLayoutPlan
    {
        public ShortModePackedLayoutPlan(
            int[] columnWidths,
            int typeColumn,
            int typeColumnSpan,
            int wordColumn,
            int wordColumnSpan,
            int simpleColumn,
            int simpleColumnSpan,
            int closeColumn,
            int closeColumnSpan)
        {
            ColumnWidths = columnWidths ?? new int[0];
            TypeColumn = typeColumn;
            TypeColumnSpan = typeColumnSpan;
            WordColumn = wordColumn;
            WordColumnSpan = wordColumnSpan;
            SimpleColumn = simpleColumn;
            SimpleColumnSpan = simpleColumnSpan;
            CloseColumn = closeColumn;
            CloseColumnSpan = closeColumnSpan;
        }

        public int[] ColumnWidths { get; private set; }
        public int TypeColumn { get; private set; }
        public int TypeColumnSpan { get; private set; }
        public int WordColumn { get; private set; }
        public int WordColumnSpan { get; private set; }
        public int SimpleColumn { get; private set; }
        public int SimpleColumnSpan { get; private set; }
        public int CloseColumn { get; private set; }
        public int CloseColumnSpan { get; private set; }
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

        public static ShortModeLabelLayout ShortModeMeasuredTypeLayout(string text, int measuredTextWidth, double zoom, int padding, int maxWidth)
        {
            return ShortModeMeasuredLayout(text, measuredTextWidth, zoom, padding, 18, maxWidth);
        }

        public static ShortModeLabelLayout ShortModeMeasuredWordLayout(string text, int measuredTextWidth, double zoom, int padding, ShortModeWordLayoutKind kind, bool hasMorePage, int maxWidth)
        {
            int charWidth = 15;
            if (kind == ShortModeWordLayoutKind.Candidates)
            {
                charWidth = hasMorePage ? 13 : 12;
            }

            return ShortModeMeasuredLayout(text, measuredTextWidth, zoom, padding, charWidth, maxWidth);
        }

        private static ShortModeLabelLayout ShortModeLayout(string text, double zoom, int charWidth, int maxWidth)
        {
            bool visible = !String.IsNullOrEmpty(text);
            int width = visible ? ShortModeTextWidth(text, zoom, charWidth, 0, maxWidth) : 0;
            return new ShortModeLabelLayout(width, visible);
        }

        private static ShortModeLabelLayout ShortModeMeasuredLayout(string text, int measuredTextWidth, double zoom, int padding, int fallbackCharWidth, int maxWidth)
        {
            bool visible = !String.IsNullOrEmpty(text);
            if (!visible)
            {
                return new ShortModeLabelLayout(0, false);
            }
            if (measuredTextWidth < 0)
            {
                measuredTextWidth = 0;
            }
            if (padding < 0)
            {
                padding = 0;
            }

            int fallbackWidth = ShortModeTextWidth(text, zoom, fallbackCharWidth, 0, maxWidth);
            int measuredWidth = measuredTextWidth + Convert.ToInt32(padding * zoom);
            int width = Math.Max(fallbackWidth, measuredWidth);
            if (width > maxWidth)
            {
                width = maxWidth;
            }
            return new ShortModeLabelLayout(width, true);
        }

        public static bool HasShortModeLayoutChange(ShortModeColumnState current, ShortModeColumnState next)
        {
            return current.TypeWidth != next.TypeWidth
                || current.TypeVisible != next.TypeVisible
                || current.WordWidth != next.WordWidth
                || current.WordVisible != next.WordVisible;
        }

        public static ShortModePackedLayoutPlan BuildShortModePackedLayout(
            ShortModeLabelLayout typeLayout,
            ShortModeLabelLayout wordLayout,
            bool simpleVisible,
            int buttonWidth,
            int columnCount)
        {
            if (columnCount < 3)
            {
                columnCount = 3;
            }
            if (buttonWidth < 0)
            {
                buttonWidth = 0;
            }

            int[] widths = new int[columnCount];
            widths[0] = buttonWidth;
            widths[1] = buttonWidth;

            int nextColumn = 2;
            int typeColumn = 2;
            int wordColumn = Math.Min(3, columnCount - 1);
            int simpleColumn = Math.Min(4, columnCount - 1);

            if (typeLayout.Visible && nextColumn < columnCount)
            {
                typeColumn = nextColumn;
                widths[nextColumn] = typeLayout.Width;
                nextColumn++;
            }

            if (wordLayout.Visible && nextColumn < columnCount)
            {
                wordColumn = nextColumn;
                widths[nextColumn] = wordLayout.Width;
                nextColumn++;
            }

            if (simpleVisible && nextColumn < columnCount)
            {
                simpleColumn = nextColumn;
                widths[nextColumn] = buttonWidth;
                nextColumn++;
            }

            int closeColumn = nextColumn < columnCount ? nextColumn : columnCount - 1;
            widths[closeColumn] = buttonWidth;
            int closeColumnSpan = columnCount - closeColumn;
            if (closeColumnSpan < 1)
            {
                closeColumnSpan = 1;
            }

            return new ShortModePackedLayoutPlan(
                widths,
                typeColumn,
                1,
                wordColumn,
                1,
                simpleColumn,
                1,
                closeColumn,
                closeColumnSpan);
        }
    }

    public sealed class OutputHintComposer
    {
        private string currentHint = "";

        public string CurrentHint
        {
            get { return currentHint; }
        }

        public void BeginOutput()
        {
            currentHint = "";
        }

        public string SetShortRootHint(string hint)
        {
            currentHint = hint ?? "";
            return currentHint;
        }

        public string SetPhoneHint(string phone)
        {
            string phoneHint = "音:" + (phone ?? "");
            if (String.IsNullOrEmpty(currentHint) || currentHint == "注:")
            {
                currentHint = phoneHint;
            }
            else
            {
                currentHint = currentHint + "," + phoneHint;
            }

            return currentHint;
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
