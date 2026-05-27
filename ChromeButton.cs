using System.Windows.Forms;

namespace uclliu
{
    public class ChromeButton : Button
    {
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
    }
}
