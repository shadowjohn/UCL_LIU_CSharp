using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace uclliu
{
    public sealed class SimpleIniData
    {
        private readonly Dictionary<string, SimpleIniSection> sections = new Dictionary<string, SimpleIniSection>(StringComparer.OrdinalIgnoreCase);
        private readonly List<string> sectionOrder = new List<string>();

        public SimpleIniSection this[string sectionName]
        {
            get { return GetOrCreateSection(sectionName); }
        }

        public IEnumerable<KeyValuePair<string, SimpleIniSection>> Sections
        {
            get
            {
                foreach (string sectionName in sectionOrder)
                {
                    yield return new KeyValuePair<string, SimpleIniSection>(sectionName, sections[sectionName]);
                }
            }
        }

        public static SimpleIniData Parse(string text)
        {
            SimpleIniData data = new SimpleIniData();
            string currentSection = "DEFAULT";
            data.GetOrCreateSection(currentSection);

            using (StringReader reader = new StringReader(text ?? ""))
            {
                string line;
                while ((line = reader.ReadLine()) != null)
                {
                    line = line.Trim();
                    if (line.Length == 0 || line.StartsWith(";", StringComparison.Ordinal) || line.StartsWith("#", StringComparison.Ordinal))
                    {
                        continue;
                    }

                    if (line.StartsWith("[", StringComparison.Ordinal) && line.EndsWith("]", StringComparison.Ordinal))
                    {
                        currentSection = line.Substring(1, line.Length - 2).Trim();
                        if (currentSection.Length == 0)
                        {
                            currentSection = "DEFAULT";
                        }
                        data.GetOrCreateSection(currentSection);
                        continue;
                    }

                    int equalsIndex = line.IndexOf('=');
                    if (equalsIndex <= 0)
                    {
                        continue;
                    }

                    string key = line.Substring(0, equalsIndex).Trim();
                    string value = line.Substring(equalsIndex + 1).Trim();
                    if (key.Length > 0)
                    {
                        data[currentSection][key] = value;
                    }
                }
            }

            return data;
        }

        public override string ToString()
        {
            StringBuilder output = new StringBuilder();
            foreach (KeyValuePair<string, SimpleIniSection> section in Sections)
            {
                output.Append("[").Append(section.Key).Append("]\r\n");
                foreach (KeyValuePair<string, string> key in section.Value.Keys)
                {
                    output.Append(key.Key).Append("=").Append(key.Value).Append("\r\n");
                }
                output.Append("\r\n");
            }
            return output.ToString();
        }

        private SimpleIniSection GetOrCreateSection(string sectionName)
        {
            if (string.IsNullOrEmpty(sectionName))
            {
                sectionName = "DEFAULT";
            }

            SimpleIniSection section;
            if (!sections.TryGetValue(sectionName, out section))
            {
                section = new SimpleIniSection();
                sections[sectionName] = section;
                sectionOrder.Add(sectionName);
            }
            return section;
        }
    }

    public sealed class SimpleIniSection
    {
        private readonly Dictionary<string, string> values = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);
        private readonly List<string> keyOrder = new List<string>();

        public string this[string key]
        {
            get
            {
                if (key == null)
                {
                    return "";
                }

                string value;
                if (values.TryGetValue(key, out value))
                {
                    return value ?? "";
                }
                return "";
            }
            set
            {
                if (string.IsNullOrEmpty(key))
                {
                    return;
                }

                if (!values.ContainsKey(key))
                {
                    keyOrder.Add(key);
                }
                values[key] = value ?? "";
            }
        }

        public IEnumerable<KeyValuePair<string, string>> Keys
        {
            get
            {
                foreach (string key in keyOrder)
                {
                    yield return new KeyValuePair<string, string>(key, values[key]);
                }
            }
        }
    }

    public static class SimpleIniFile
    {
        private static readonly Encoding Utf8NoBom = new UTF8Encoding(false);

        public static SimpleIniData ReadFile(string path)
        {
            return SimpleIniData.Parse(File.ReadAllText(path, Encoding.UTF8));
        }

        public static void WriteFile(string path, SimpleIniData data)
        {
            string directory = Path.GetDirectoryName(path);
            if (!string.IsNullOrEmpty(directory) && !Directory.Exists(directory))
            {
                Directory.CreateDirectory(directory);
            }

            File.WriteAllText(path, data.ToString(), Utf8NoBom);
        }
    }
}
