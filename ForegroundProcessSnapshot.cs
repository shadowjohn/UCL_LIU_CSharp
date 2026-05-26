using System.Collections.Generic;

namespace uclliu
{
    public static class ForegroundProcessSnapshot
    {
        public static Dictionary<string, string> Create(string title, string processName, uint processId)
        {
            Dictionary<string, string> output = new Dictionary<string, string>();
            output["PROCESS_TITLE"] = title ?? "";
            output["PROCESS_NAME"] = NormalizeProcessName(processName);
            output["PROCESS_PID"] = processId.ToString();
            return output;
        }

        public static string NormalizeProcessName(string processName)
        {
            return (processName ?? "").Trim().ToLowerInvariant();
        }
    }
}
