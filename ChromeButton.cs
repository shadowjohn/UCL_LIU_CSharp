using System;
using System.Windows.Forms;

namespace uclliu
{
    public class ChromeButton : Button
    {
        private bool suppressNextClickFromDrag;

        public ChromeButton()
        {
            TabStop = false;
            SetStyle(ControlStyles.Selectable, false);
        }

        protected override bool ShowFocusCues
        {
            get { return false; }
        }

        protected override bool ShowKeyboardCues
        {
            get { return false; }
        }

        public void SuppressNextClickFromDrag()
        {
            suppressNextClickFromDrag = true;
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {
            if (e.Button == MouseButtons.Left)
            {
                suppressNextClickFromDrag = false;
            }

            base.OnMouseDown(e);
        }

        protected override void OnClick(EventArgs e)
        {
            if (suppressNextClickFromDrag)
            {
                suppressNextClickFromDrag = false;
                return;
            }

            base.OnClick(e);
        }
    }
}
