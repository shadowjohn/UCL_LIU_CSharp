using System;
using System.Windows.Forms;

namespace uclliu
{
    public static class TableLayoutModeTransition
    {
        public static void RestoreLongModeColumns(
            TableLayoutPanel panel,
            Control typeLabel,
            Control wordLabel,
            Control simpleButton,
            Control gameModeButton,
            Control closeButton)
        {
            if (panel == null)
            {
                return;
            }

            panel.SuspendLayout();
            try
            {
                SetColumnSpanBeforeMove(panel, closeButton, 6, 1);
                SetColumnSpanBeforeMove(panel, typeLabel, 2, 1);
                SetColumnSpanBeforeMove(panel, wordLabel, 3, 1);
                SetColumnSpanBeforeMove(panel, simpleButton, 4, 1);
                SetColumnSpanBeforeMove(panel, gameModeButton, 5, 1);
            }
            finally
            {
                panel.ResumeLayout(false);
            }
        }

        private static void SetColumnSpanBeforeMove(TableLayoutPanel panel, Control control, int column, int span)
        {
            if (control == null)
            {
                return;
            }

            int columnCount = panel.ColumnCount;
            if (columnCount <= 0)
            {
                columnCount = panel.ColumnStyles.Count;
            }
            if (columnCount <= 0)
            {
                return;
            }

            if (column < 0)
            {
                column = 0;
            }
            if (column >= columnCount)
            {
                column = columnCount - 1;
            }
            if (span < 1)
            {
                span = 1;
            }
            if (column + span > columnCount)
            {
                span = columnCount - column;
            }

            // 從短版跨欄回長版時，先縮 span 再移 column，避免 FixedSize TableLayout 被判定格子重疊。
            if (panel.GetColumnSpan(control) != span)
            {
                panel.SetColumnSpan(control, span);
            }
            if (panel.GetColumn(control) != column)
            {
                panel.SetColumn(control, column);
            }
        }
    }
}
