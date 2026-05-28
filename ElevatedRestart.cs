using System;
using System.Text;

namespace uclliu
{
    public sealed class ElevatedRestartRequest
    {
        public ElevatedRestartRequest(string fileName, string arguments, string workingDirectory, string verb)
        {
            FileName = fileName;
            Arguments = arguments;
            WorkingDirectory = workingDirectory;
            Verb = verb;
        }

        public string FileName { get; private set; }
        public string Arguments { get; private set; }
        public string WorkingDirectory { get; private set; }
        public string Verb { get; private set; }
    }

    public static class ElevatedRestartRequestBuilder
    {
        public static ElevatedRestartRequest Build(string executablePath, string[] arguments, string workingDirectory)
        {
            if (string.IsNullOrWhiteSpace(executablePath))
            {
                throw new ArgumentException("executablePath is required", "executablePath");
            }

            return new ElevatedRestartRequest(
                executablePath,
                BuildArgumentString(arguments),
                string.IsNullOrWhiteSpace(workingDirectory) ? AppDomain.CurrentDomain.BaseDirectory : workingDirectory,
                "runas");
        }

        private static string BuildArgumentString(string[] arguments)
        {
            if (arguments == null || arguments.Length == 0)
            {
                return "";
            }

            StringBuilder builder = new StringBuilder();
            for (int i = 0; i < arguments.Length; i++)
            {
                if (i > 0)
                {
                    builder.Append(" ");
                }
                builder.Append(QuoteArgument(arguments[i]));
            }
            return builder.ToString();
        }

        private static string QuoteArgument(string argument)
        {
            string value = argument ?? "";
            return "\"" + value.Replace("\"", "\\\"") + "\"";
        }
    }
}
