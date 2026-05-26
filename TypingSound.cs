using System;
using System.Collections.Generic;
using System.IO;
using System.Media;
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
            byte[] scaledWav;
            if (!TryScalePcm16WavBytes(wav, volume, out scaledWav))
            {
                return false;
            }

            string directory = Path.GetDirectoryName(targetPath);
            if (!String.IsNullOrEmpty(directory))
            {
                Directory.CreateDirectory(directory);
            }
            File.WriteAllBytes(targetPath, scaledWav);
            return true;
        }

        public static bool TryScalePcm16WavBytes(byte[] wav, int volume, out byte[] scaledWav)
        {
            scaledWav = null;
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

            scaledWav = new byte[wav.Length];
            Buffer.BlockCopy(wav, 0, scaledWav, 0, wav.Length);
            byte[] pcm = new byte[dataSize];
            Buffer.BlockCopy(scaledWav, dataOffset, pcm, 0, dataSize);
            byte[] scaled = ScalePcm16Data(pcm, volume);
            Buffer.BlockCopy(scaled, 0, scaledWav, dataOffset, scaled.Length);
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

    public interface ITypingSoundHandle : IDisposable
    {
        void Play();
    }

    public interface ITypingSoundPlaybackEngine
    {
        ITypingSoundHandle Create(byte[] wavData);
    }

    public sealed class SoundPlayerPlaybackEngine : ITypingSoundPlaybackEngine
    {
        public ITypingSoundHandle Create(byte[] wavData)
        {
            return new SoundPlayerTypingSoundHandle(wavData);
        }
    }

    internal sealed class SoundPlayerTypingSoundHandle : ITypingSoundHandle
    {
        private readonly object syncRoot = new object();
        private readonly byte[] wavData;
        private readonly MemoryStream stream;
        private readonly SoundPlayer player;

        public SoundPlayerTypingSoundHandle(byte[] wavData)
        {
            this.wavData = wavData;
            stream = new MemoryStream(this.wavData, false);
            player = new SoundPlayer(stream);
            player.Load();
        }

        public void Play()
        {
            lock (syncRoot)
            {
                stream.Position = 0;
                player.Play();
            }
        }

        public void Dispose()
        {
            player.Dispose();
            stream.Dispose();
        }
    }

    public sealed class TypingSoundPlayer
    {
        private readonly object syncRoot = new object();
        private readonly Random random = new Random();
        private readonly TypingSoundKeyState keyState = new TypingSoundKeyState();
        private readonly List<string> normalSounds = new List<string>();
        private readonly Dictionary<int, string> specialSounds = new Dictionary<int, string>();
        private readonly Dictionary<string, long> soundVersions = new Dictionary<string, long>(StringComparer.OrdinalIgnoreCase);
        private readonly Dictionary<string, ITypingSoundHandle> playbackCache = new Dictionary<string, ITypingSoundHandle>(StringComparer.OrdinalIgnoreCase);
        private readonly HashSet<string> pendingCacheKeys = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        private readonly ITypingSoundPlaybackEngine playbackEngine;
        private readonly string baseDirectory;

        public TypingSoundPlayer()
            : this(AppDomain.CurrentDomain.BaseDirectory)
        {
        }

        public TypingSoundPlayer(string baseDirectory)
            : this(baseDirectory, new SoundPlayerPlaybackEngine())
        {
        }

        public TypingSoundPlayer(string baseDirectory, ITypingSoundPlaybackEngine playbackEngine)
        {
            this.baseDirectory = baseDirectory;
            this.playbackEngine = playbackEngine ?? new SoundPlayerPlaybackEngine();
            Reload();
        }

        public void Reload()
        {
            lock (syncRoot)
            {
                normalSounds.Clear();
                specialSounds.Clear();
                soundVersions.Clear();
                ClearPlaybackCache();

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

                        soundVersions[fullPath] = File.GetLastWriteTimeUtc(fullPath).Ticks;
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

        public void WarmUp(int volume)
        {
            int safeVolume = TypingSoundVolume.Clamp(volume);
            if (safeVolume <= 0)
            {
                return;
            }

            List<string> soundPaths = GetAllSoundPathsSnapshot();
            for (int i = 0; i < soundPaths.Count; i++)
            {
                QueuePrepare(soundPaths[i], safeVolume);
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
            PlayForKey(keyCode, volume, false);
        }

        public void PreviewForKey(int keyCode, int volume)
        {
            PlayForKey(keyCode, volume, true);
        }

        private void PlayForKey(int keyCode, int volume, bool allowSynchronousPrepare)
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

            ITypingSoundHandle handle = GetPreparedHandle(soundPath, safeVolume);

            if (handle == null && allowSynchronousPrepare)
            {
                handle = PrepareAndCache(soundPath, safeVolume);
            }

            if (handle == null)
            {
                QueuePrepare(soundPath, safeVolume);
                return;
            }

            try
            {
                handle.Play();
            }
            catch
            {
                // 音效只是 UX 補助，播放失敗不可影響輸入法主流程。
            }
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

        private ITypingSoundHandle GetPreparedHandle(string sourcePath, int volume)
        {
            string cacheKey = BuildCacheKey(sourcePath, volume);
            lock (syncRoot)
            {
                ITypingSoundHandle handle;
                if (playbackCache.TryGetValue(cacheKey, out handle))
                {
                    return handle;
                }
            }

            return null;
        }

        private void QueuePrepare(string sourcePath, int volume)
        {
            string cacheKey = BuildCacheKey(sourcePath, volume);
            lock (syncRoot)
            {
                if (playbackCache.ContainsKey(cacheKey) || pendingCacheKeys.Contains(cacheKey))
                {
                    return;
                }
                pendingCacheKeys.Add(cacheKey);
            }

            ThreadPool.QueueUserWorkItem(delegate
            {
                try
                {
                    PrepareAndCache(sourcePath, volume);
                }
                finally
                {
                    lock (syncRoot)
                    {
                        pendingCacheKeys.Remove(cacheKey);
                    }
                }
            });
        }

        private ITypingSoundHandle PrepareAndCache(string sourcePath, int volume)
        {
            string cacheKey = BuildCacheKey(sourcePath, volume);
            lock (syncRoot)
            {
                ITypingSoundHandle existingHandle;
                if (playbackCache.TryGetValue(cacheKey, out existingHandle))
                {
                    return existingHandle;
                }
            }

            try
            {
                byte[] wavData = File.ReadAllBytes(sourcePath);
                if (volume < 100)
                {
                    byte[] scaledData;
                    if (WavPcmVolumeScaler.TryScalePcm16WavBytes(wavData, volume, out scaledData))
                    {
                        wavData = scaledData;
                    }
                }

                ITypingSoundHandle newHandle = playbackEngine.Create(wavData);
                lock (syncRoot)
                {
                    ITypingSoundHandle existingHandle;
                    if (playbackCache.TryGetValue(cacheKey, out existingHandle))
                    {
                        newHandle.Dispose();
                        return existingHandle;
                    }

                    playbackCache[cacheKey] = newHandle;
                    return newHandle;
                }
            }
            catch
            {
                return null;
            }
        }

        private string BuildCacheKey(string sourcePath, int volume)
        {
            long lastWriteTicks = 0;
            lock (syncRoot)
            {
                soundVersions.TryGetValue(sourcePath, out lastWriteTicks);
            }
            return sourcePath + "|" + volume.ToString() + "|" + lastWriteTicks.ToString();
        }

        private List<string> GetAllSoundPathsSnapshot()
        {
            lock (syncRoot)
            {
                List<string> paths = new List<string>();
                for (int i = 0; i < normalSounds.Count; i++)
                {
                    paths.Add(normalSounds[i]);
                }
                foreach (string path in specialSounds.Values)
                {
                    paths.Add(path);
                }
                return paths;
            }
        }

        private void ClearPlaybackCache()
        {
            foreach (ITypingSoundHandle handle in playbackCache.Values)
            {
                handle.Dispose();
            }
            playbackCache.Clear();
            pendingCacheKeys.Clear();
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
