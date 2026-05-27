using System;
using System.Windows.Forms;

namespace uclliu
{
    public enum ChromeButtonTextKind
    {
        MainCjk,
        Close
    }

    public static class ChromeButtonTextAlignment
    {
        public static Padding LongModePadding()
        {
            return Padding.Empty;
        }

        public static Padding ShortModePadding(ChromeButtonTextKind kind, double zoom)
        {
            int baseBottomPadding = kind == ChromeButtonTextKind.Close ? 2 : 5;
            return new Padding(0, 0, 0, Scale(baseBottomPadding, zoom));
        }

        private static int Scale(int pixels, double zoom)
        {
            if (zoom <= 0)
            {
                zoom = 1;
            }

            int scaled = Convert.ToInt32(pixels * zoom);
            return Math.Max(1, scaled);
        }
    }
}
