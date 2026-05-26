using System;

namespace uclliu
{
    public static class ShortRootDisplaySetting
    {
        public static bool IsEnabled(string value)
        {
            return Normalize(value) == "1";
        }

        public static string Normalize(string value)
        {
            int intValue;
            if (!Int32.TryParse((value ?? "").Trim(), out intValue) || intValue <= 0)
            {
                return "0";
            }

            return "1";
        }

        public static bool Toggle(SimpleIniData config, bool currentValue, Action save)
        {
            bool nextValue = !currentValue;
            if (config != null)
            {
                config["DEFAULT"]["SP"] = nextValue ? "1" : "0";
            }
            if (save != null)
            {
                save();
            }
            return nextValue;
        }
    }
}
