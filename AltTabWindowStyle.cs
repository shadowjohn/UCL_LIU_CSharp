namespace uclliu
{
    public static class AltTabWindowStyle
    {
        public const int WsExToolWindow = 0x00000080;
        public const int WsExAppWindow = 0x00040000;

        public static int HideFromSwitcher(int exStyle)
        {
            return (exStyle | WsExToolWindow) & ~WsExAppWindow;
        }
    }
}
