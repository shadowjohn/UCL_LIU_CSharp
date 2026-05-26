using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.IO.Pipes;
using System.Security.Principal;
using System.Text;
using Microsoft.Win32;

namespace uclliu
{
    public static class TsfBridgeConstants
    {
        public const string Clsid = "{77B90778-7368-4F68-B022-C50005EBBE72}";
        public const string PipeName = "uclliu_tsf_bridge";
        public const string PipeNamePrefix = "uclliu_tsf_bridge_";
        public const int DefaultTimeoutMilliseconds = 80;
    }

    public sealed class TsfBridgeAssets
    {
        public string BaseDirectory { get; internal set; }
        public string BridgeDirectory { get; internal set; }
        public string BridgeDllPath { get; internal set; }
        public string RegisterScriptPath { get; internal set; }
        public string UnregisterScriptPath { get; internal set; }
        public string UnlockScriptPath { get; internal set; }

        public bool HasBridgeDll
        {
            get { return File.Exists(BridgeDllPath); }
        }

        public bool HasRegisterScript
        {
            get { return File.Exists(RegisterScriptPath); }
        }

        public bool HasUnregisterScript
        {
            get { return File.Exists(UnregisterScriptPath); }
        }

        public bool HasUnlockScript
        {
            get { return File.Exists(UnlockScriptPath); }
        }
    }

    public static class TsfBridgeAssetLocator
    {
        public static TsfBridgeAssets Locate(string baseDirectory)
        {
            return Locate(baseDirectory, Environment.Is64BitProcess);
        }

        public static TsfBridgeAssets Locate(string baseDirectory, bool preferX64)
        {
            string normalizedBase = string.IsNullOrWhiteSpace(baseDirectory)
                ? AppDomain.CurrentDomain.BaseDirectory
                : baseDirectory;
            string bridgeDirectory = Path.Combine(normalizedBase, "tsf_bridge");
            string preferredArch = preferX64 ? "x64" : "x86";
            string fallbackArch = preferX64 ? "x86" : "x64";

            string dllPath = FirstExistingPath(
                Path.Combine(bridgeDirectory, preferredArch, "UclTsfBridge.dll"),
                Path.Combine(bridgeDirectory, fallbackArch, "UclTsfBridge.dll"),
                Path.Combine(bridgeDirectory, "UclTsfBridge.dll"));

            return new TsfBridgeAssets
            {
                BaseDirectory = normalizedBase,
                BridgeDirectory = bridgeDirectory,
                BridgeDllPath = dllPath,
                RegisterScriptPath = Path.Combine(bridgeDirectory, "register_tsf_bridge.bat"),
                UnregisterScriptPath = Path.Combine(bridgeDirectory, "unregister_tsf_bridge.bat"),
                UnlockScriptPath = Path.Combine(bridgeDirectory, "unlock_tsf_bridge.ps1")
            };
        }

        private static string FirstExistingPath(params string[] paths)
        {
            for (int i = 0; i < paths.Length; i++)
            {
                if (File.Exists(paths[i]))
                {
                    return paths[i];
                }
            }
            return paths.Length == 0 ? "" : paths[0];
        }
    }

    public sealed class TsfBridgeRegistryStatus
    {
        public bool IsRegistered { get; internal set; }
        public string RegisteredPath { get; internal set; }
        public string RegistryLocation { get; internal set; }
    }

    public sealed class TsfBridgeStatus
    {
        public TsfBridgeAssets Assets { get; internal set; }
        public TsfBridgeRegistryStatus Registry { get; internal set; }
        public bool IsAdministrator { get; internal set; }

        public bool IsReady
        {
            get { return Assets != null && Assets.HasBridgeDll && Registry != null && Registry.IsRegistered; }
        }

        public string ToDisplayText()
        {
            StringBuilder builder = new StringBuilder();
            builder.AppendLine("TSF Bridge 狀態");
            builder.AppendLine("");
            builder.AppendLine("DLL: " + (Assets != null && Assets.HasBridgeDll ? Assets.BridgeDllPath : "找不到"));
            builder.AppendLine("註冊: " + (Registry != null && Registry.IsRegistered ? "已註冊" : "未註冊"));
            if (Registry != null && Registry.IsRegistered)
            {
                builder.AppendLine("註冊位置: " + Registry.RegistryLocation);
                builder.AppendLine("註冊 DLL: " + Registry.RegisteredPath);
            }
            builder.AppendLine("目前權限: " + (IsAdministrator ? "系統管理員" : "一般使用者"));
            builder.AppendLine("");
            builder.AppendLine("使用 TSF 模式時，請在 Windows 輸入法設定中加入 UCLLIU TSF Bridge。");
            builder.AppendLine("若註冊腳本跳出 UAC，請允許後再切換 Windows 輸入法。");
            return builder.ToString();
        }
    }

    public sealed class TsfBridgeManager
    {
        private readonly string baseDirectory;

        public TsfBridgeManager(string baseDirectory)
        {
            this.baseDirectory = baseDirectory;
        }

        public TsfBridgeAssets LocateAssets()
        {
            return TsfBridgeAssetLocator.Locate(baseDirectory);
        }

        public TsfBridgeStatus GetStatus()
        {
            return new TsfBridgeStatus
            {
                Assets = LocateAssets(),
                Registry = GetRegistryStatus(),
                IsAdministrator = IsAdministrator()
            };
        }

        public bool TryRegister(out string message)
        {
            TsfBridgeAssets assets = LocateAssets();
            if (!assets.HasBridgeDll)
            {
                message = "找不到 TSF Bridge DLL：" + assets.BridgeDllPath;
                return false;
            }
            if (!assets.HasRegisterScript)
            {
                message = "找不到註冊腳本：" + assets.RegisterScriptPath;
                return false;
            }

            return TryStartScript(assets.RegisterScriptPath, true, "已啟動 TSF Bridge 註冊流程，請確認 UAC。", out message);
        }

        public bool TryUnregister(out string message)
        {
            TsfBridgeAssets assets = LocateAssets();
            if (!assets.HasUnregisterScript)
            {
                message = "找不到解除註冊腳本：" + assets.UnregisterScriptPath;
                return false;
            }

            return TryStartScript(assets.UnregisterScriptPath, true, "已啟動 TSF Bridge 解除註冊流程，請確認 UAC。", out message);
        }

        public bool TryUnlock(out string message)
        {
            TsfBridgeAssets assets = LocateAssets();
            if (!assets.HasUnlockScript)
            {
                message = "找不到解除 DLL 封鎖腳本：" + assets.UnlockScriptPath;
                return false;
            }

            ProcessStartInfo startInfo = new ProcessStartInfo();
            startInfo.FileName = "powershell.exe";
            startInfo.Arguments = "-ExecutionPolicy Bypass -File " + QuoteArgument(assets.UnlockScriptPath);
            startInfo.WorkingDirectory = assets.BridgeDirectory;
            startInfo.UseShellExecute = true;
            return TryStartProcess(startInfo, "已啟動 TSF Bridge DLL 解除封鎖流程。", out message);
        }

        public bool TryOpenInputMethodSettings(out string message)
        {
            ProcessStartInfo startInfo = new ProcessStartInfo();
            startInfo.FileName = "ms-settings:typing";
            startInfo.UseShellExecute = true;
            return TryStartProcess(startInfo, "已開啟 Windows 輸入設定。", out message);
        }

        public static bool IsAdministrator()
        {
            try
            {
                WindowsIdentity identity = WindowsIdentity.GetCurrent();
                WindowsPrincipal principal = new WindowsPrincipal(identity);
                return principal.IsInRole(WindowsBuiltInRole.Administrator);
            }
            catch
            {
                return false;
            }
        }

        public static TsfBridgeRegistryStatus GetRegistryStatus()
        {
            string subKey = @"Software\Classes\CLSID\" + TsfBridgeConstants.Clsid + @"\InProcServer32";
            RegistryHive[] hives = new RegistryHive[] { RegistryHive.CurrentUser, RegistryHive.LocalMachine };
            RegistryView[] views = GetRegistryViews();

            for (int h = 0; h < hives.Length; h++)
            {
                for (int v = 0; v < views.Length; v++)
                {
                    TsfBridgeRegistryStatus status = TryReadRegistry(hives[h], views[v], subKey);
                    if (status.IsRegistered)
                    {
                        return status;
                    }
                }
            }

            return new TsfBridgeRegistryStatus
            {
                IsRegistered = false,
                RegisteredPath = "",
                RegistryLocation = ""
            };
        }

        private static RegistryView[] GetRegistryViews()
        {
            if (!Environment.Is64BitOperatingSystem)
            {
                return new RegistryView[] { RegistryView.Default };
            }
            return new RegistryView[] { RegistryView.Registry64, RegistryView.Registry32, RegistryView.Default };
        }

        private static TsfBridgeRegistryStatus TryReadRegistry(RegistryHive hive, RegistryView view, string subKey)
        {
            try
            {
                using (RegistryKey baseKey = RegistryKey.OpenBaseKey(hive, view))
                using (RegistryKey key = baseKey.OpenSubKey(subKey))
                {
                    if (key == null)
                    {
                        return EmptyRegistryStatus();
                    }

                    string value = key.GetValue("") as string;
                    if (string.IsNullOrWhiteSpace(value))
                    {
                        return EmptyRegistryStatus();
                    }

                    return new TsfBridgeRegistryStatus
                    {
                        IsRegistered = true,
                        RegisteredPath = value,
                        RegistryLocation = hive.ToString() + " / " + view.ToString()
                    };
                }
            }
            catch
            {
                return EmptyRegistryStatus();
            }
        }

        private static TsfBridgeRegistryStatus EmptyRegistryStatus()
        {
            return new TsfBridgeRegistryStatus
            {
                IsRegistered = false,
                RegisteredPath = "",
                RegistryLocation = ""
            };
        }

        private bool TryStartScript(string scriptPath, bool elevated, string successMessage, out string message)
        {
            ProcessStartInfo startInfo = new ProcessStartInfo();
            startInfo.FileName = scriptPath;
            startInfo.WorkingDirectory = Path.GetDirectoryName(scriptPath);
            startInfo.UseShellExecute = true;
            if (elevated)
            {
                startInfo.Verb = "runas";
            }
            return TryStartProcess(startInfo, successMessage, out message);
        }

        private static bool TryStartProcess(ProcessStartInfo startInfo, string successMessage, out string message)
        {
            try
            {
                Process.Start(startInfo);
                message = successMessage;
                return true;
            }
            catch (Win32Exception ex)
            {
                if (ex.NativeErrorCode == 1223)
                {
                    message = "使用者取消 UAC 或系統拒絕啟動流程。";
                    return false;
                }

                message = "啟動流程失敗：" + ex.Message;
                return false;
            }
            catch (Exception ex)
            {
                message = "啟動流程失敗：" + ex.Message;
                return false;
            }
        }

        private static string QuoteArgument(string value)
        {
            return "\"" + (value ?? "").Replace("\"", "\\\"") + "\"";
        }
    }

    public static class TsfBridgeSettings
    {
        public static int NormalizeTimeout(string value, int fallback)
        {
            int timeout;
            if (!Int32.TryParse((value ?? "").Trim(), out timeout))
            {
                timeout = fallback;
            }
            if (timeout < 10)
            {
                return 10;
            }
            if (timeout > 1000)
            {
                return 1000;
            }
            return timeout;
        }
    }

    public static class TsfBridgeProtocol
    {
        public static string BuildCommitTextRequest(string text)
        {
            return "{\"cmd\":\"commit_text\",\"text\":\"" + EscapeJson(text ?? "") + "\"}\n";
        }

        public static bool IsOkResponse(string response)
        {
            try
            {
                Dictionary<string, object> data = LiuJsonTable.DecodeJson(response ?? "") as Dictionary<string, object>;
                object okValue;
                return data != null
                    && data.TryGetValue("ok", out okValue)
                    && Convert.ToBoolean(okValue, CultureInfo.InvariantCulture);
            }
            catch
            {
                return false;
            }
        }

        public static string GetErrorCode(string response)
        {
            try
            {
                Dictionary<string, object> data = LiuJsonTable.DecodeJson(response ?? "") as Dictionary<string, object>;
                object errorValue;
                if (data != null && data.TryGetValue("error", out errorValue) && errorValue != null)
                {
                    return Convert.ToString(errorValue, CultureInfo.InvariantCulture);
                }
            }
            catch
            {
            }
            return "";
        }

        private static string EscapeJson(string value)
        {
            StringBuilder builder = new StringBuilder();
            for (int i = 0; i < value.Length; i++)
            {
                char c = value[i];
                switch (c)
                {
                    case '\\':
                        builder.Append("\\\\");
                        break;
                    case '"':
                        builder.Append("\\\"");
                        break;
                    case '\n':
                        builder.Append("\\n");
                        break;
                    case '\r':
                        builder.Append("\\r");
                        break;
                    case '\t':
                        builder.Append("\\t");
                        break;
                    default:
                        if (c < 0x20)
                        {
                            builder.Append("\\u");
                            builder.Append(((int)c).ToString("x4", CultureInfo.InvariantCulture));
                        }
                        else
                        {
                            builder.Append(c);
                        }
                        break;
                }
            }
            return builder.ToString();
        }
    }

    internal interface ITsfBridgePipeClientFactory
    {
        ITsfBridgePipeClient Create(string pipeName);
    }

    internal interface ITsfBridgePipeClient : IDisposable
    {
        void Connect(int timeoutMilliseconds);
        void WriteRequest(string request);
        string ReadResponse();
    }

    public sealed class TsfBridgeOutput
    {
        private readonly ITsfBridgePipeClientFactory pipeClientFactory;

        public TsfBridgeOutput()
            : this(new NamedPipeTsfBridgeClientFactory())
        {
        }

        internal TsfBridgeOutput(ITsfBridgePipeClientFactory pipeClientFactory)
        {
            if (pipeClientFactory == null)
            {
                throw new ArgumentNullException("pipeClientFactory");
            }
            this.pipeClientFactory = pipeClientFactory;
        }

        public bool TryCommitText(string text, int foregroundProcessId, int timeoutMilliseconds, out string error)
        {
            error = null;
            if (string.IsNullOrEmpty(text))
            {
                error = "TSF bridge text is empty";
                return false;
            }

            int timeout = TsfBridgeSettings.NormalizeTimeout(timeoutMilliseconds.ToString(CultureInfo.InvariantCulture), TsfBridgeConstants.DefaultTimeoutMilliseconds);
            string request = TsfBridgeProtocol.BuildCommitTextRequest(text);
            string[] candidates = BuildPipeCandidates(foregroundProcessId);
            string lastError = "TSF bridge pipe unavailable";

            for (int i = 0; i < candidates.Length; i++)
            {
                string pipeName = candidates[i];
                try
                {
                    using (ITsfBridgePipeClient client = pipeClientFactory.Create(pipeName))
                    {
                        client.Connect(timeout);
                        client.WriteRequest(request);
                        string response = client.ReadResponse();
                        if (TsfBridgeProtocol.IsOkResponse(response))
                        {
                            error = null;
                            return true;
                        }
                        lastError = "TSF bridge rejected request on " + pipeName + ": " + (response ?? "");
                        if (!IsPipeErrorResponse(response))
                        {
                            error = lastError;
                            return false;
                        }
                    }
                }
                catch (Exception ex)
                {
                    lastError = "TSF bridge pipe " + pipeName + " failed: " + ex.Message;
                }
            }

            error = lastError;
            return false;
        }

        private static bool IsPipeErrorResponse(string response)
        {
            return string.Equals(TsfBridgeProtocol.GetErrorCode(response), "PIPE_ERROR", StringComparison.OrdinalIgnoreCase);
        }

        public static string[] BuildPipeCandidates(int foregroundProcessId)
        {
            if (foregroundProcessId > 0)
            {
                return new string[]
                {
                    TsfBridgeConstants.PipeNamePrefix + foregroundProcessId.ToString(CultureInfo.InvariantCulture),
                    TsfBridgeConstants.PipeName
                };
            }
            return new string[] { TsfBridgeConstants.PipeName };
        }
    }

    internal sealed class NamedPipeTsfBridgeClientFactory : ITsfBridgePipeClientFactory
    {
        public ITsfBridgePipeClient Create(string pipeName)
        {
            return new NamedPipeTsfBridgeClient(pipeName);
        }
    }

    internal sealed class NamedPipeTsfBridgeClient : ITsfBridgePipeClient
    {
        private readonly NamedPipeClientStream stream;

        public NamedPipeTsfBridgeClient(string pipeName)
        {
            stream = new NamedPipeClientStream(".", pipeName, PipeDirection.InOut);
        }

        public void Connect(int timeoutMilliseconds)
        {
            stream.Connect(timeoutMilliseconds);
            try
            {
                stream.ReadTimeout = timeoutMilliseconds;
                stream.WriteTimeout = timeoutMilliseconds;
            }
            catch
            {
            }
        }

        public void WriteRequest(string request)
        {
            byte[] payload = Encoding.UTF8.GetBytes(request ?? "");
            stream.Write(payload, 0, payload.Length);
            stream.Flush();
        }

        public string ReadResponse()
        {
            byte[] buffer = new byte[8192];
            int read = stream.Read(buffer, 0, buffer.Length);
            if (read <= 0)
            {
                return "";
            }
            return Encoding.UTF8.GetString(buffer, 0, read);
        }

        public void Dispose()
        {
            stream.Dispose();
        }
    }
}
