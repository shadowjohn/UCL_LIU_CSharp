using System.Windows.Forms;

namespace uclliu
{
    public static class TrayMenuText
    {
        public const string CandidateMenu = "12. 候選字相關";
        public const string CandidateDownload = "請先下載候選字";
        public const string CandidateClearMemory = "清除智慧選字記憶";
        public const string Exit = "13. 離開(Quit)";

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

        public static string CandidateEnable(bool enabled)
        {
            return ToggleItem("", enabled, "啟動候選字表");
        }

        public static string CandidateContinuous(bool enabled)
        {
            return ToggleItem("", enabled, "連續出字功能");
        }

        public static string SmartRoot(bool enabled)
        {
            return ToggleItem("", enabled, "智慧字根功能");
        }

        public static string[] CandidateItems(bool tableAvailable, bool enabled, bool continuousEnabled, bool smartRootEnabled)
        {
            if (!tableAvailable)
            {
                return new string[] { CandidateDownload };
            }
            return new string[]
            {
                CandidateEnable(enabled),
                CandidateContinuous(continuousEnabled),
                SmartRoot(smartRootEnabled),
                CandidateClearMemory
            };
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
