using System;
using System.Diagnostics;
using System.IO;
using System.Text;
using System.Threading;

namespace uclliu
{
    public static class KeyboardHookPerformancePolicy
    {
        public const int ForegroundProcessCacheMilliseconds = 500;
        public const int SlowHookThresholdMilliseconds = 20;
        public const int SlowHookLogIntervalMilliseconds = 1000;

        public static ProcessPriorityClass ProcessPriorityClass
        {
            get { return ProcessPriorityClass.AboveNormal; }
        }

        public static ThreadPriority UiThreadPriority
        {
            get { return ThreadPriority.AboveNormal; }
        }
    }

    public sealed class KeyboardHookLatencyMonitor
    {
        private readonly int slowThresholdMilliseconds;
        private readonly int logIntervalMilliseconds;
        private long lastLogTicks = Int64.MinValue;

        public KeyboardHookLatencyMonitor(int slowThresholdMilliseconds, int logIntervalMilliseconds)
        {
            this.slowThresholdMilliseconds = slowThresholdMilliseconds;
            this.logIntervalMilliseconds = logIntervalMilliseconds;
        }

        public static long GetTimestamp()
        {
            return Stopwatch.GetTimestamp();
        }

        public static long MillisecondsToTicks(int milliseconds)
        {
            return (long)((milliseconds / 1000.0) * Stopwatch.Frequency);
        }

        public bool ShouldLog(long startedTicks, out int elapsedMilliseconds)
        {
            long nowTicks = GetTimestamp();
            elapsedMilliseconds = TicksToMilliseconds(nowTicks - startedTicks);
            return ShouldLogElapsedMilliseconds(elapsedMilliseconds, nowTicks);
        }

        public bool ShouldLogElapsedMilliseconds(int elapsedMilliseconds, long nowTicks)
        {
            if (elapsedMilliseconds < slowThresholdMilliseconds)
            {
                return false;
            }

            if (lastLogTicks != Int64.MinValue && TicksToMilliseconds(nowTicks - lastLogTicks) < logIntervalMilliseconds)
            {
                return false;
            }

            lastLogTicks = nowTicks;
            return true;
        }

        public static int TicksToMilliseconds(long ticks)
        {
            return (int)((ticks * 1000.0) / Stopwatch.Frequency);
        }
    }

    public static class RuntimePriorityTuning
    {
        public static void ApplyBestEffort(Action<string> log)
        {
            try
            {
                Process currentProcess = Process.GetCurrentProcess();
                if (currentProcess.PriorityClass == ProcessPriorityClass.Idle
                    || currentProcess.PriorityClass == ProcessPriorityClass.BelowNormal
                    || currentProcess.PriorityClass == ProcessPriorityClass.Normal)
                {
                    currentProcess.PriorityClass = KeyboardHookPerformancePolicy.ProcessPriorityClass;
                }
            }
            catch (Exception ex)
            {
                WriteLog(log, "set process priority failed: " + ex.Message);
            }

            try
            {
                if (Thread.CurrentThread.Priority == ThreadPriority.Lowest
                    || Thread.CurrentThread.Priority == ThreadPriority.BelowNormal
                    || Thread.CurrentThread.Priority == ThreadPriority.Normal)
                {
                    Thread.CurrentThread.Priority = KeyboardHookPerformancePolicy.UiThreadPriority;
                }
            }
            catch (Exception ex)
            {
                WriteLog(log, "set UI thread priority failed: " + ex.Message);
            }
        }

        private static void WriteLog(Action<string> log, string message)
        {
            if (log != null)
            {
                log(message);
            }
        }
    }

    public sealed class AsyncPerformanceLogger
    {
        private readonly string logPath;

        public AsyncPerformanceLogger(string logPath)
        {
            this.logPath = logPath;
        }

        public void Log(string message)
        {
            if (string.IsNullOrWhiteSpace(logPath) || string.IsNullOrWhiteSpace(message))
            {
                return;
            }

            ThreadPool.QueueUserWorkItem(delegate
            {
                try
                {
                    string directory = Path.GetDirectoryName(logPath);
                    if (!string.IsNullOrWhiteSpace(directory) && !Directory.Exists(directory))
                    {
                        Directory.CreateDirectory(directory);
                    }

                    string line = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss.fff") + " " + message + Environment.NewLine;
                    File.AppendAllText(logPath, line, Encoding.UTF8);
                }
                catch
                {
                }
            });
        }
    }
}
