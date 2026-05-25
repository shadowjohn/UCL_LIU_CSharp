using System;
using System.Collections.Generic;
using System.IO;
using System.Media;
using System.Security.Cryptography;
using System.Text;
using System.Threading;

namespace uclliu
{
    public static class TypingSoundVolume
    {
        public static int Normalize(string value, int defaultValue)
        {
            int parsed;
            if (!Int32.TryParse(value, out parsed))
            {
                parsed = defaultValue;
            }
            return Clamp(parsed);
        }

        public static int Clamp(int volume)
        {
            if (volume < 0)
            {
                return 0;
            }
            if (volume > 100)
            {
                return 100;
            }
            return volume;
        }
    }

    public sealed class TypingSoundKeyState
    {
        private readonly HashSet<int> pressedKeys = new HashSet<int>();

        public bool ShouldPlayKeyDown(int keyCode)
        {
            if (pressedKeys.Contains(keyCode))
            {
                return false;
            }

            pressedKeys.Add(keyCode);
            return true;
        }

        public void HandleKeyUp(int keyCode)
        {
            pressedKeys.Remove(keyCode);
        }

        public void Clear()
        {
            pressedKeys.Clear();
        }
    }

    public static class TypingSoundCatalog
    {
        public const int EnterKeyCode = 13;
        public const int DeleteKeyCode = 46;
        public const int BackspaceKeyCode = 8;
        public const int SpaceKeyCode = 32;

        public static int? GetSpecialKeyCode(string fileName)
        {
            string mainName = Path.GetFileNameWithoutExtension(fileName);
            if (mainName == null)
            {
                return null;
            }

            mainName = mainName.ToLowerInvariant();
            switch (mainName)
            {
                case "enter":
                case "return":
                    return EnterKeyCode;
                case "delete":
                case "del":
                    return DeleteKeyCode;
                case "backspace":
                case "bs":
                    return BackspaceKeyCode;
                case "space":
                case "sp":
                    return SpaceKeyCode;
                default:
                    return null;
            }
        }
    }

    public static class WavPcmVolumeScaler
    {
        public static byte[] ScalePcm16Data(byte[] pcmData, int volume)
        {
            int safeVolume = TypingSoundVolume.Clamp(volume);
            byte[] scaled = new byte[pcmData.Length];
            Buffer.BlockCopy(pcmData, 0, scaled, 0, pcmData.Length);

            for (int i = 0; i + 1 < scaled.Length; i += 2)
            {
                short sample = BitConverter.ToInt16(scaled, i);
                int adjusted = (int)Math.Round(sample * (safeVolume / 100.0));
                if (adjusted > short.MaxValue)
                {
                    adjusted = short.MaxValue;
                }
                if (adjusted < short.MinValue)
                {
                    adjusted = short.MinValue;
                }

                byte[] bytes = BitConverter.GetBytes((short)adjusted);
                scaled[i] = bytes[0];
                scaled[i + 1] = bytes[1];
            }

            return scaled;
        }

        public static bool TryScalePcm16WavFile(string sourcePath, string targetPath, int volume)
        {
            byte[] wav = File.ReadAllBytes(sourcePath);
            int fmtOffset;
            int fmtSize;
            int dataOffset;
            int dataSize;

            if (!TryFindChunk(wav, "fmt ", out fmtOffset, out fmtSize) ||
                !TryFindChunk(wav, "data", out dataOffset, out dataSize))
            {
                return false;
            }

            if (fmtSize < 16)
            {
                return false;
            }

            ushort audioFormat = BitConverter.ToUInt16(wav, fmtOffset);
            ushort bitsPerSample = BitConverter.ToUInt16(wav, fmtOffset + 14);
            if (audioFormat != 1 || bitsPerSample != 16)
            {
                return false;
            }

            byte[] pcm = new byte[dataSize];
            Buffer.BlockCopy(wav, dataOffset, pcm, 0, dataSize);
            byte[] scaled = ScalePcm16Data(pcm, volume);
            Buffer.BlockCopy(scaled, 0, wav, dataOffset, scaled.Length);

            string directory = Path.GetDirectoryName(targetPath);
            if (!String.IsNullOrEmpty(directory))
            {
                Directory.CreateDirectory(directory);
            }
            File.WriteAllBytes(targetPath, wav);
            return true;
        }

        private static bool TryFindChunk(byte[] wav, string chunkId, out int dataOffset, out int dataSize)
        {
            dataOffset = 0;
            dataSize = 0;
            if (wav.Length < 12 || Encoding.ASCII.GetString(wav, 0, 4) != "RIFF")
            {
                return false;
            }

            byte[] idBytes = Encoding.ASCII.GetBytes(chunkId);
            int offset = 12;
            while (offset + 8 <= wav.Length)
            {
                bool isMatch = true;
                for (int i = 0; i < 4; i++)
                {
                    if (wav[offset + i] != idBytes[i])
                    {
                        isMatch = false;
                        break;
                    }
                }

                int size = BitConverter.ToInt32(wav, offset + 4);
                int nextOffset = offset + 8 + size;
                if (isMatch)
                {
                    if (size < 0 || offset + 8 + size > wav.Length)
                    {
                        return false;
                    }
                    dataOffset = offset + 8;
                    dataSize = size;
                    return true;
                }

                if (size < 0 || nextOffset <= offset || nextOffset > wav.Length + 1)
                {
                    return false;
                }
                offset = nextOffset + (size % 2);
            }

            return false;
        }
    }

    public sealed class TypingSoundPlayer
    {
        private readonly object syncRoot = new object();
        private readonly Random random = new Random();
        private readonly TypingSoundKeyState keyState = new TypingSoundKeyState();
        private readonly List<string> normalSounds = new List<string>();
        private readonly Dictionary<int, string> specialSounds = new Dictionary<int, string>();
        private readonly Dictionary<string, string> scaledSoundCache = new Dictionary<string, string>();
        private readonly string baseDirectory;
        private readonly string cacheDirectory;
        private int activePlayCount;
        private const int MaxActivePlayCount = 3;

        public TypingSoundPlayer()
            : this(AppDomain.CurrentDomain.BaseDirectory)
        {
        }

        public TypingSoundPlayer(string baseDirectory)
        {
            this.baseDirectory = baseDirectory;
            cacheDirectory = Path.Combine(Path.GetTempPath(), "UCL_LIU_CSharp", "typing-sound");
            Reload();
        }

        public void Reload()
        {
            lock (syncRoot)
            {
                normalSounds.Clear();
                specialSounds.Clear();

                HashSet<string> seenFiles = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
                foreach (string directory in GetSoundDirectories())
                {
                    if (!Directory.Exists(directory))
                    {
                        continue;
                    }

                    foreach (string file in Directory.GetFiles(directory, "*.wav"))
                    {
                        string fullPath = Path.GetFullPath(file);
                        if (!seenFiles.Add(fullPath))
                        {
                            continue;
                        }

                        int? specialKey = TypingSoundCatalog.GetSpecialKeyCode(fullPath);
                        if (specialKey.HasValue)
                        {
                            specialSounds[specialKey.Value] = fullPath;
                        }
                        else
                        {
                            normalSounds.Add(fullPath);
                        }
                    }
                }
            }
        }

        public void HandleKey(bool isKeyDown, bool isKeyUp, int keyCode, int volume)
        {
            if (isKeyUp)
            {
                keyState.HandleKeyUp(keyCode);
                return;
            }

            if (!isKeyDown || !keyState.ShouldPlayKeyDown(keyCode))
            {
                return;
            }

            PlayForKey(keyCode, volume);
        }

        public void PlayForKey(int keyCode, int volume)
        {
            int safeVolume = TypingSoundVolume.Clamp(volume);
            if (safeVolume <= 0)
            {
                return;
            }

            string soundPath = ChooseSoundPath(keyCode);
            if (String.IsNullOrEmpty(soundPath))
            {
                return;
            }

            string playPath = PrepareSoundPath(soundPath, safeVolume);
            if (String.IsNullOrEmpty(playPath))
            {
                return;
            }

            lock (syncRoot)
            {
                if (activePlayCount >= MaxActivePlayCount)
                {
                    return;
                }
                activePlayCount++;
            }

            ThreadPool.QueueUserWorkItem(delegate
            {
                try
                {
                    using (SoundPlayer player = new SoundPlayer(playPath))
                    {
                        player.PlaySync();
                    }
                }
                catch
                {
                    // 音效只是 UX 補助，播放失敗不可影響輸入法主流程。
                }
                finally
                {
                    lock (syncRoot)
                    {
                        if (activePlayCount > 0)
                        {
                            activePlayCount--;
                        }
                    }
                }
            });
        }

        private string ChooseSoundPath(int keyCode)
        {
            lock (syncRoot)
            {
                string specialPath;
                if (specialSounds.TryGetValue(keyCode, out specialPath))
                {
                    return specialPath;
                }

                if (normalSounds.Count == 0)
                {
                    return null;
                }

                return normalSounds[random.Next(normalSounds.Count)];
            }
        }

        private string PrepareSoundPath(string sourcePath, int volume)
        {
            if (volume >= 100)
            {
                return sourcePath;
            }

            string cacheKey = sourcePath + "|" + volume.ToString() + "|" + File.GetLastWriteTimeUtc(sourcePath).Ticks.ToString();
            lock (syncRoot)
            {
                string cachedPath;
                if (scaledSoundCache.TryGetValue(cacheKey, out cachedPath) && File.Exists(cachedPath))
                {
                    return cachedPath;
                }
            }

            string targetPath = Path.Combine(cacheDirectory, BuildCacheFileName(sourcePath, volume));
            try
            {
                if (!File.Exists(targetPath))
                {
                    if (!WavPcmVolumeScaler.TryScalePcm16WavFile(sourcePath, targetPath, volume))
                    {
                        return sourcePath;
                    }
                }

                lock (syncRoot)
                {
                    scaledSoundCache[cacheKey] = targetPath;
                }
                return targetPath;
            }
            catch
            {
                return sourcePath;
            }
        }

        private string BuildCacheFileName(string sourcePath, int volume)
        {
            string name = Path.GetFileNameWithoutExtension(sourcePath);
            foreach (char invalid in Path.GetInvalidFileNameChars())
            {
                name = name.Replace(invalid, '_');
            }

            string hash;
            using (MD5 md5 = MD5.Create())
            {
                byte[] bytes = Encoding.UTF8.GetBytes(sourcePath + "|" + File.GetLastWriteTimeUtc(sourcePath).Ticks.ToString());
                hash = BitConverter.ToString(md5.ComputeHash(bytes)).Replace("-", "").ToLowerInvariant();
            }

            return name + "-" + volume.ToString() + "-" + hash + ".wav";
        }

        private IEnumerable<string> GetSoundDirectories()
        {
            yield return baseDirectory;
            yield return Path.Combine(baseDirectory, "wavs");
            yield return Directory.GetCurrentDirectory();
            yield return Path.Combine(Directory.GetCurrentDirectory(), "wavs");
        }
    }
}
