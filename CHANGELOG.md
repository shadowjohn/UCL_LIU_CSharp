# CHANGELOG

C# 版肥米輸入法更新紀錄。Python 版完整歷史請看 [UCL_LIU CHANGELOG](https://github.com/shadowjohn/UCL_LIU/blob/master/CHANGELOG.md)。

---

## [Unreleased] - 2026-05-27

### Added

- 新增 GitHub Actions 自動測試、Release build、打包與 tag 發行流程；推送 `v*` tag 時會自動建立或更新 GitHub Release。
- 新增 `tools/package-release.ps1`，本機與 GitHub Actions 共用同一套打包邏輯，產出推薦下載包與單檔版 `uclliu.exe`。
- C# 字碼表自動轉換補上 Python 版常見外部來源：`wuxiami.txt`、`liur_trad.dict.yaml` / `liur_Trad.dict.yaml`、`terry_boshiamy.txt`、`fcitx_boshiamy.txt`、`uniliu.txt`。
- `LiuTableConverter` 新增 RIME YAML 與鬆散文字碼表匯入流程，會先產生 `liu.cin`，再沿用既有 `liu.cin -> liu.json` 路徑。
- 載入 `liu.json` 時會將字根 key 正規化為小寫並合併同碼候選，對齊 Python 版後期韓語/特殊碼表相容處理。

### Documentation

- README 補上 v0.12 與 v0.11 的 Microsoft WDSI 病毒碼掃描提交連結。
- README 補充短版 UI chrome 尺寸、啟動 packed layout 與 focus 視覺修正狀態。
- README 更新 C# / Python 成熟度評估與字碼表支援清單。
- README 補充 TSF Bridge 推薦下載 `uclliu.zip`、啟用流程與 Windows 鍵盤配置成功狀態說明。
- README 補充 GitHub Actions 自動發行與本機打包指令。

### Fixed

- 修正候選字存在時按 numpad 數字或特殊鍵可能在 `LowLevelKeyboardProc` 觸發 `System.FormatException`，導致瀏覽器輸入時卡頓後崩潰。
- 修正 solution 的 `Release|Any CPU` 未勾選專案 build，避免 CI 顯示成功但沒有產生 Release 版 exe。
- 移除 slow keyboard hook 的 `UCLLIU_performance.log` 檔案寫入；節流偵測保留，Debug 模式仍可看到 `PERF ...` 訊息。
- 修正 `,,,s` 短版模式切換後使用舊 label 重算寬度、每鍵重複 layout、候選/簡根欄展開過寬，導致 UI 顯示異常與打字卡頓。
- 修正短版模式出字後注音提示讀取舊候選 label，造成 `0覺,音:...` 類殘留；短版欄寬未變時不再觸發 TableLayout 重排。
- 修正短版模式仍保留零寬隱藏欄位的 TableLayout 格線，造成 `肥 半 ㄥ 0 X` 之間出現不自然間距。
- 修正短版模式候選欄只用字數估算寬度，導致 `0肥`、`0你 1條` 類候選被截斷；改用目前 WinForms 字型實測寬度並微調置中。
- 短版模式 `肥` / `英`、`半` / `全`、`╳` chrome 字級與長版一致，並固定 40x40 尺寸。
- 修正 `,,,l` 從短版切回長版時，先顯示「正常模式」按鈕再復原欄位，造成 `TableLayoutPanel` 在 `X` 跨欄狀態下拋出 `System.ArgumentException` 崩潰。
- 修正短版模式 `肥` / `半` / `╳` 按鈕文字視覺中心偏低，加入短版專用 bottom padding 微調，長版維持原本對齊。
- 修正短版模式 chrome 按鈕 focus 後殘留按下視覺狀態，並讓短版啟動時立即套用 packed layout，避免一開始留下長版空白欄位。

---

## [0.12] - 2026-05-27

### Added

- 新增 `TsfBridge.cs`，以外掛式方式移植 Python 版 TSF Bridge：
  - 定位 `tsf_bridge` assets 與架構對應 DLL
  - 檢查 CLSID 註冊狀態與目前權限
  - 透過 UAC 啟動註冊/解除註冊腳本
  - 以 named pipe 呼叫 `commit_text`，失敗時 fallback 回 Unicode `SendInput`
- 專案加入 Python 版 TSF Bridge DLL 與 `register_tsf_bridge.bat` / `unregister_tsf_bridge.bat` / `unlock_tsf_bridge.ps1`，build 時複製到輸出目錄。
- 右下角選單新增「TSF Bridge 管理」，支援狀態檢查、註冊、解除註冊、解除 DLL 封鎖與開啟 Windows 輸入法設定。
- 右下角出字模式新增「TSF出字模式」，此模式為手動選用，不會自動改變預設出字流程。
- 新增 `TypingSound.cs`，支援：
  - `wavs\*.wav` 音效掃描
  - Enter / Delete / Backspace / Space 特殊鍵音效
  - 一般按鍵隨機音效
  - 0-100 音量正規化與 PCM 16-bit wav 音量縮放
  - 長按同鍵不重複連發音效
- 右下角選單新增「打字音」子選單，可開關打字音並切換 10%-100% 音量。
- 右下角選單新增「啟動預設為肥模式」與「允許 Shift+Space 切換全形/半形」。
- 右下角選單新增「顯示提示注音」，可控制出字後是否顯示 `音:ㄈㄟˊ` 類提示。
- 新增新版 `pinyi.txt` 注音查詢表，支援 `';zo6` 查「ㄈㄟˊ」候選。
- 專案加入 Python 版後期 `wavs` 音效素材，build 時複製到輸出目錄。

### Changed

- `liu.json` / `custom.json` 解析改用 `JavaScriptSerializer` 風格的 `json_decode` 流程，核心字根表改為 `Dictionary<string, List<string>>`。
- 移除 `System.Json` NuGet 依賴與 ILRepack 合併項目。
- `UCLLIU.ini` 改用內建 `SimpleIni` 讀寫，移除 `ini-parser` / `INIFileParser` 與 `packages.config`。
- 出字模式選擇改為支援視窗標題規則，PTT/term.ptt.cc/ws.ptt.cc 與 Win11 Notepad 會自動改用剪貼簿貼上。
- process 相容清單會忽略大小寫與 `.exe` 副檔名，並補上 Windows Terminal / mintty / RimWorld / mstsc 等 Python 版後期清單。
- 版本資訊更新至 v0.12，exe 詳細資料欄位參考 Python 版 metadata，補上 3WA、作者註解與產品資訊。
- `KEYBOARD_VOLUME` 會限制在 0-100，避免錯誤設定造成音量邏輯異常。
- 啟動時會 best-effort 將行程與 UI thread 提升到 `AboveNormal`，降低 CPU high loading 時 keyboard hook 被延遲的機率。
- foreground process cache 從 120ms 拉長到 500ms，減少 hook 熱路徑反覆查 process。
- `ENABLE_HALF_FULL=0` 時，`Shift+Space` 不再觸發半全形切換。
- `STARTUP_DEFAULT_UCL=0` 時，啟動後會套用英模式。
- 移除 C# 版不需要的 `dist` 目錄，開發輸出改以 `bin\Debug` / `bin\Release` 為準。
- tray menu 改在 `ContextMenu.Popup` 前即時重建，避免右下角選單狀態慢半拍才更新。
- tray icon 左鍵也可打開同一份選單，右鍵維持既有托盤選單行為。
- Unicode `SendInput` 由整串一次送出改為逐字送出，貼近 Python 版 `SendKeysCtypes` 的出字節奏。
- 低依賴說明改為「主程式無 NuGet / 無 DLL 合併；TSF Bridge 為選配外掛 DLL」。

### Fixed

- 主浮動視窗改套用 `WS_EX_TOOLWINDOW` 並移除 `WS_EX_APPWINDOW`，避免按 Alt+Tab 時出現肥米輸入法視窗。
- low-level keyboard hook 的前段 App 判斷改走輕量 process name 查詢，不再每鍵讀完整視窗標題或 `Process.MainModule`，降低剛切入 Notepad++ 等 App 時短暫卡住的機率。
- 修正 `CTRL_SP=1` 時 Shift 放開不會清掉 `flag_is_shift_down`，造成後續輸入像 Shift 黏住。
- low-level keyboard hook 支援 `WM_SYSKEYDOWN` / `WM_SYSKEYUP`，避免 Alt/系統組合鍵流程漏掉 keyup。
- 一般出字改成在 low-level keyboard hook 回傳後才送出，降低 Notepad++ / Scintilla 編輯區因同步 `SendInput` 重入而卡頓的機率。
- 延後送字時改為先同步清候選與輸入狀態，只延後真正輸出，避免快速接第二字時被上一字送出流程清掉狀態。
- Unicode `SendInput` 加上肥米專用 `dwExtraInfo` 標記，hook 只放行肥米自己的 injected event；送字期間的真人按鍵不再穿透成英文。
- keyboard hook 超過 20ms 會節流寫入 `UCLLIU_performance.log`，方便追 CPU high loading 下的卡頓來源。
- 輸入中的 label repaint 與短模式欄寬調整改排到 UI queue，hook 內只保留候選狀態計算，降低 Notepad++ 高負載漏字根的機率。
- Shift 單鍵切換英肥加入時間保護，只有短時間乾淨 tap 才切換，避免高負載下 Shift 延遲誤判導致 Notepad++ 跳英。
- 打字音效的 wav 音量快取與檔案準備移到背景執行緒，降低第一次按鍵或切換時卡 UI 的機會。
- 打字音效改為預載 wav 到記憶體並重用播放 handle，熱路徑不再每鍵建立 `SoundPlayer`、`PlaySync()` 或查檔案 timestamp。
- 打字音效播放改用 `winmm.dll` 的 `waveOut` one-shot，每次按鍵使用獨立播放 handle，避免連打時前一個聲音被截斷。
- 程式自己送字時會先跳過 hook 處理，避免不必要的 foreground process 查詢。
- 修正 `,,,x` / `,,,z` 框選轉換流程，收斂為 Python 版同樣的「清剪貼簿 -> `Ctrl+C` -> retry 讀剪貼簿」路徑，並延後到 keyboard hook 回傳後執行，避免讀到舊剪貼簿或尾鍵繼續流入目標視窗。
- 補回 Python 版後期 `uclcode_rr` 字根反查 hash，`,,,x` 解算數字候選碼時可直接 O(1) 查表。
- 修正新版 `pinyi.txt` 同音字候選會把注音碼或注音符號混到第 0 候選的問題，並保留舊版 `pinyi.txt` 相容。
- 補齊新版 `pinyi.txt` 的反向注音表，出字後可依設定顯示讀音。
- Notepad++ 不列入預設貼上模式，也不再預設走 `WM_CHAR`，先回到逐字 Unicode `SendInput` 驗證 Python-style 出字手感。
- 補上 Notepad++ 相容註記：自動完成 popup 可能攔截 Scintilla 按鍵/焦點流程，關閉自動完成後逐字 `SendInput` 可正常打字。
- 修正「顯示短根」只改記憶體狀態、不會寫回 `UCLLIU.ini` 的問題；啟動時也會正確套用 `SP` 設定。
- TSF Bridge pipe 會先嘗試 foreground PID 專用 pipe，再退回全域 pipe；預設 timeout 80ms，避免 TSF context 不存在時拖住一般出字。

### Verification

- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj`
- `MSBuild.exe uclliu.sln /t:Rebuild /p:Configuration=Debug /p:Platform="Any CPU"`

---

## 2026-05-25 - README/CHANGELOG 現代化

### Documentation

- 現代化 `README.md`，改成快速開始、目前狀態、出字模式、與 Python 版差異、開發驗證等章節。
- 新增本檔，將 C# 版近期追功能獨立整理，避免 README 繼續混入大量歷史流水帳。

---

## 2026-05-25 - Unicode SendInput 出字

### Added

- 新增 `TextOutput.cs`：
  - `UnicodeSendInputOutput`
  - `ClipboardPasteOutput`
  - `TextOutputRouter`
- 新增核心測試：
  - Unicode SendInput key down/up 事件建立
  - surrogate pair 保留
  - 剪貼簿貼上失敗後還原
  - 出字策略選擇

### Changed

- `DEFAULT` 正常出字模式改為優先使用 Unicode `SendInput`。
- 右下角選單標示為「正常出字模式（Unicode）」。
- 剪貼簿貼上流程集中封裝，加入 retry、短暫等待與 try/finally 還原。
- 出字失敗時保留 fallback，避免單一策略失敗造成無法出字。

### Verification

- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj`
- `dotnet msbuild uclliu.sln ...` 仍受限本機缺 .NET Framework 4.5.2 Developer Pack。

---

## 2026-05-25 - 自定詞庫

### Added

- 新增 `CustomDictionaryStore.cs`，支援 `custom.json` 載入、儲存、字根驗證與合併。
- 新增 `CustomDictionaryForm.cs`，提供自定詞庫視窗。
- 右下角選單加入「自定詞庫」。
- 新增 `,,,BOX` 開啟自定詞庫。
- 自定詞庫視窗改為單例，已開啟時聚焦。

### Changed

- `liu.json` 載入後會自動合併同目錄 `custom.json`。
- 儲存自定詞庫後可立即重載字根。
- 反查字根快取會在重載時清空並重建。

### Verification

- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj`
- `CustomDictionaryForm.cs` + `CustomDictionaryStore.cs` 以臨時 `net10.0-windows` WinForms 專案編譯通過。

---

## 2026-05-25 - 轉檔與 UI 熱路徑降載

### Added

- 新增 `GOALS.md`，將 C# 版追 Python 版功能拆成小輪次。
- 新增 `LiuTableConverter.cs`：
  - `liu.cin -> liu.json`
  - `liu-uni.tab -> liu.cin -> liu.json`
  - 啟動時缺 `liu.json` 可自動解算
- 新增 `UiLayoutCalculator.cs`。
- 新增 `tools/UclLiuCoreTests`，讓核心轉檔與 UI 計算可用 .NET SDK 驗證。

### Changed

- 預設 debug 關閉，降低 keyboard hook 熱路徑輸出負擔。
- 短版模式更新候選字時減少整個 UI 重算。
- `getForegroundWindowProcessInfo()` 加入短暫 cache，降低每次按鍵查 process 的成本。

### Verification

- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj`
- 完整 `uclliu.sln` build 受限本機缺 .NET Framework 4.5.2 Developer Pack。

---

## 2019-12-15 - v0.1

### Added

- 初版 C# / WinForms 肥米輸入法。
- 目標功能約對齊當時 Python 版 UCL_LIU 1.25。
