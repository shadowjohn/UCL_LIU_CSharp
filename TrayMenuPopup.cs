using System;
using System.Runtime.InteropServices;

namespace uclliu
{
    public static class TrayMenuPopup
    {
        private const uint WM_NULL = 0;

        [DllImport("user32.dll")]
        [return: MarshalAs(UnmanagedType.Bool)]
        private static extern bool SetForegroundWindow(IntPtr hWnd);

        [DllImport("user32.dll", CharSet = CharSet.Auto)]
        [return: MarshalAs(UnmanagedType.Bool)]
        private static extern bool PostMessage(IntPtr hWnd, uint message, IntPtr wParam, IntPtr lParam);

        public static void Show(IntPtr ownerHandle, Action showMenu)
        {
            Show(ownerHandle, showMenu,
                delegate(IntPtr handle) { SetForegroundWindow(handle); },
                delegate(IntPtr handle) { PostMessage(handle, WM_NULL, IntPtr.Zero, IntPtr.Zero); });
        }

        internal static void Show(IntPtr ownerHandle, Action showMenu,
            Action<IntPtr> activateOwner, Action<IntPtr> completeMenu)
        {
            // Tray 選單的 owner 必須在前景，Windows 才會在點選外部時取消選單。
            activateOwner(ownerHandle);
            try
            {
                showMenu();
            }
            finally
            {
                // 結束後送 WM_NULL，避免下次開啟時被前一次選單狀態立即關閉。
                completeMenu(ownerHandle);
            }
        }
    }
}
