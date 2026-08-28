using System;
using System.Collections.Generic;
using System.Threading;

namespace uclliu
{
    public sealed class BackgroundDebugWriter : IDisposable
    {
        private readonly Action<string> sink;
        private readonly Queue<string> pending = new Queue<string>();
        private readonly AutoResetEvent wakeSignal = new AutoResetEvent(false);
        private readonly Thread worker;
        private bool stopping;

        public BackgroundDebugWriter(Action<string> sink)
        {
            if (sink == null)
            {
                throw new ArgumentNullException("sink");
            }

            this.sink = sink;
            worker = new Thread(run);
            worker.IsBackground = true;
            worker.Name = "UCLLIU debug output";
            worker.Start();
        }

        public void Write(string message)
        {
            lock (pending)
            {
                if (stopping)
                {
                    return;
                }
                pending.Enqueue(message ?? "");
                wakeSignal.Set();
            }
        }

        private void run()
        {
            while (true)
            {
                string message = null;
                lock (pending)
                {
                    if (pending.Count > 0)
                    {
                        message = pending.Dequeue();
                    }
                    else if (stopping)
                    {
                        return;
                    }
                }

                if (message == null)
                {
                    wakeSignal.WaitOne();
                    continue;
                }

                try
                {
                    sink(message);
                }
                catch
                {
                    // Debug 輸出失敗不能反過來影響輸入法主流程。
                }
            }
        }

        public void Dispose()
        {
            lock (pending)
            {
                if (stopping)
                {
                    return;
                }
                stopping = true;
                wakeSignal.Set();
            }

            if (Thread.CurrentThread != worker)
            {
                worker.Join(2000);
            }
            wakeSignal.Close();
        }
    }

    public sealed class OutputPerformanceTelemetry
    {
        private sealed class Bucket
        {
            public readonly List<int> Samples = new List<int>();
            public long FirstSampleMilliseconds;
            public int SuccessCount;
            public int FailureCount;
        }

        private readonly int reportIntervalMilliseconds;
        private readonly int reportSampleCount;
        private readonly Dictionary<string, Bucket> buckets = new Dictionary<string, Bucket>(StringComparer.Ordinal);

        public OutputPerformanceTelemetry(int reportIntervalMilliseconds, int reportSampleCount)
        {
            if (reportIntervalMilliseconds <= 0)
            {
                throw new ArgumentOutOfRangeException("reportIntervalMilliseconds");
            }
            if (reportSampleCount <= 0)
            {
                throw new ArgumentOutOfRangeException("reportSampleCount");
            }

            this.reportIntervalMilliseconds = reportIntervalMilliseconds;
            this.reportSampleCount = reportSampleCount;
        }

        public string Record(string outputMode, string processName, int elapsedMilliseconds, bool succeeded, long nowMilliseconds)
        {
            outputMode = string.IsNullOrEmpty(outputMode) ? "Unknown" : outputMode;
            processName = string.IsNullOrEmpty(processName) ? "unknown" : processName;
            if (elapsedMilliseconds < 0)
            {
                elapsedMilliseconds = 0;
            }

            string key = outputMode + "\n" + processName;
            lock (buckets)
            {
                Bucket bucket;
                if (!buckets.TryGetValue(key, out bucket))
                {
                    bucket = new Bucket();
                    bucket.FirstSampleMilliseconds = nowMilliseconds;
                    buckets[key] = bucket;
                }

                bucket.Samples.Add(elapsedMilliseconds);
                if (succeeded)
                {
                    bucket.SuccessCount++;
                }
                else
                {
                    bucket.FailureCount++;
                }

                bool sampleLimitReached = bucket.Samples.Count >= reportSampleCount;
                bool intervalReached = nowMilliseconds - bucket.FirstSampleMilliseconds >= reportIntervalMilliseconds;
                if (!sampleLimitReached && !intervalReached)
                {
                    return null;
                }

                string summary = build_summary(outputMode, processName, bucket);
                buckets.Remove(key);
                return summary;
            }
        }

        private static string build_summary(string outputMode, string processName, Bucket bucket)
        {
            int[] sorted = bucket.Samples.ToArray();
            Array.Sort(sorted);
            long total = 0;
            for (int i = 0; i < sorted.Length; i++)
            {
                total += sorted[i];
            }

            int average = (int)(total / sorted.Length);
            int p50 = percentile(sorted, 0.50);
            int p95 = percentile(sorted, 0.95);
            int max = sorted[sorted.Length - 1];
            return "PERF output mode=" + outputMode
                + " process=" + processName
                + " count=" + sorted.Length.ToString()
                + " success=" + bucket.SuccessCount.ToString()
                + " failure=" + bucket.FailureCount.ToString()
                + " avg=" + average.ToString() + "ms"
                + " p50=" + p50.ToString() + "ms"
                + " p95=" + p95.ToString() + "ms"
                + " max=" + max.ToString() + "ms";
        }

        private static int percentile(int[] sorted, double percentileValue)
        {
            int index = (int)Math.Ceiling(sorted.Length * percentileValue) - 1;
            if (index < 0)
            {
                index = 0;
            }
            return sorted[index];
        }
    }
}
