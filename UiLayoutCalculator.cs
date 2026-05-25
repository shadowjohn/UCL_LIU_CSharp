using System;

namespace uclliu
{
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
    }
}
