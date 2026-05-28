using System.Windows.Forms;

namespace uclliu
{
    public static class TrayMenuText
    {
        public static string Mark(bool enabled)
        {
            return enabled ? "●" : "　";
        }

        public static string ToggleItem(string prefix, bool enabled, string text)
        {
            return prefix + "【" + Mark(enabled) + "】" + text;
        }

        public static string RestartAsAdministrator()
        {
            return "★以系統管理員身分重新啟動肥米";
        }

        public static string OutputModeDefault(string outputType)
        {
            return "【" + Mark(IsOutputType(outputType, "DEFAULT")) + "】正常出字模式（Unicode）";
        }

        public static string OutputModeBig5(string outputType)
        {
            return "【" + Mark(IsOutputType(outputType, "BIG5")) + "】BIG5模式";
        }

        public static string OutputModePaste(string outputType)
        {
            return "【" + Mark(IsOutputType(outputType, "PASTE")) + "】複製貼上模式";
        }

        public static string OutputModeTsf(string outputType)
        {
            return "【" + Mark(IsOutputType(outputType, "TSF")) + "】TSF出字模式";
        }

        public static string TsfBridgeStatus(bool isRegistered)
        {
            return isRegistered ? "TSF Bridge 已註冊" : "TSF Bridge 未註冊";
        }

        private static bool IsOutputType(string outputType, string expected)
        {
            return (outputType ?? "DEFAULT").Trim().ToUpperInvariant() == expected;
        }
    }

    public static class TrayMenuClickPolicy
    {
        public static bool ShouldOpenMenu(MouseButtons button)
        {
            return button == MouseButtons.Left || button == MouseButtons.Right;
        }
    }
}
