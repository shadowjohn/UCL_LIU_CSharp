# 開發對話紀錄

---

## 2026-05-26 - 顯示短根設定寫回 INI

### 任務目標

1. 修正 tray menu 點選「顯示短根」後沒有寫回 `UCLLIU.ini` 的問題。
2. 啟動時依照 INI 的 `SP` 值還原短根顯示狀態。

### 根因判斷

- `run_toggle_sp()` 只切換 `is_display_sp` 記憶體旗標，沒有同步更新 `config["DEFAULT"]["SP"]`，也沒有呼叫 `saveConfig()`。
- `loadConfig()` 只正規化 `SP` 字串，沒有把值套回 `is_display_sp`。

### 實作紀錄

- 新增 `ShortRootDisplaySetting`，集中處理 `SP` 正規化、讀取與切換儲存。
- `run_toggle_sp()` 改為同步更新 `SP=0/1` 並立即 `saveConfig()`。
- `loadConfig()` 改為正規化 `SP` 後套回 `is_display_sp`。

### 驗證紀錄

- 新增核心測試確認短根切換會更新 `SP` 並呼叫 save callback。
- 新增核心測試確認 `SP` 正規化與讀取規則。
- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj` 通過。

---

## 2026-05-26 - tray icon 左右鍵開啟選單

### 任務目標

1. 讓 tray icon 左鍵與右鍵都能開啟右下角選單。
2. 保留右鍵原本 `NotifyIcon.ContextMenu` 行為，避免改壞既有托盤選單關閉/失焦流程。

### 實作紀錄

- 新增 `TrayMenuClickPolicy.ShouldOpenMenu()`，明確定義左鍵與右鍵都屬於開啟選單操作。
- `NotifyIcon1_MouseClick` 對左鍵手動呼叫同一份 `ContextMenu`。
- 右鍵仍交給 WinForms `NotifyIcon.ContextMenu` 內建流程處理。

### 驗證紀錄

- 新增核心測試確認左鍵、右鍵會開選單，中鍵不開。
- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj` 通過。

---

## 2026-05-26 - Notepad++ 快速接打卡頓與跳英文修正

### 任務目標

1. 修正 Notepad++ 中第一字可出、第二字快速接打時偶發卡住或跳英文的問題。
2. 保持 Notepad++ 使用 Unicode `SendInput`，不回退到剪貼簿貼上。

### 根因判斷

- 前次為了避開 hook 內同步出字，將整個 `senddata()` 延後到 hook 回傳後執行；但 `senddata()` 也會清 `play_ucl_label` 與候選狀態，快速輸入下一字時可能被上一字的延後流程清掉。
- `is_send_ucl` 期間 hook 直接放行所有鍵盤事件，真人下一碼若剛好落在送字期間，可能穿透到目標 App 變成英文字。

### 實作紀錄

- 將 `senddata()` 拆成 `prepare_senddata_text()` 與 `send_prepared_output()`。
- `queue_senddata()` 先同步執行狀態清理與簡繁轉換，只把真正輸出排到 hook 回傳後。
- Unicode `SendInput` 的 key event 加入固定 `dwExtraInfo` 標記。
- low-level hook 改為只放行肥米自己標記的 injected event；剪貼簿/舊 SendKeys 模式仍可在 `is_send_ucl` 期間放行 injected event，但不再放行真人按鍵。

### 驗證紀錄

- 先新增測試確認 `DeferredTextOutputDispatcher` 會先 prepare 再 post。
- 先新增測試確認 Unicode `SendInput` 事件帶有肥米標記，且 hook 判斷不會把未標記 injected event 或真人按鍵視為肥米輸出。
- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj` 通過。

---

## 2026-05-26 - tray menu 狀態即時更新

### 任務目標

1. 修正右下角 tray 選單狀態慢半拍才更新的問題。
2. 避免點選項目後手動清空 menu 造成下一次開啟時短暫空白或舊狀態。

### 實作紀錄

- 將 `NotifyIcon.ContextMenu` 在表單建構時就指向同一份 `ContextMenu`。
- 選單內容改在 `ContextMenu.Popup` 事件觸發時即時重建，確保 Windows 顯示前已是最新狀態。
- 移除各 menu handler 裡的 `cMenu.MenuItems.Clear()`，統一只在 `rebuild_tray_menu()` 裡清空重建。
- 新增 `TrayMenuText`，集中處理 `●` / 全形空白標記與常用選單文字。

### 驗證紀錄

- 先新增 tray menu 標記測試，確認缺少 `TrayMenuText` 時測試紅燈。
- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj` 通過。
- `MSBuild.exe uclliu.csproj /t:Rebuild /p:Configuration=Debug /p:Platform=AnyCPU` 通過。

---

## 2026-05-26 - Notepad++ 打字手感追查與 hook 出字延後

### 任務目標

1. 不使用剪貼簿的前提下，改善 Notepad++ / Scintilla 編輯區忽然變難打的情況。
2. 降低 low-level keyboard hook 內同步送字造成的重入與卡頓風險。

### 實作紀錄

- 對照 Python 版後確認正常模式同樣是 `KEYEVENTF_UNICODE` SendInput，不應把 Notepad++ 改成貼上模式。
- 新增 `DeferredTextOutputDispatcher`，把一般出字排到 hook 回傳後再執行。
- `Form1.LowLevelKeyboardProc` 中所有直接 `ucl.senddata(...)` 改為 `ucl.queue_senddata(...)`。
- 需要出字後顯示簡根/注音的候選輸出改為 `queue_senddata_with_labels(...)`，確保 `senddata()` 清輸入區後才更新提示。

### 驗證紀錄

- 先新增 deferred output 測試，確認缺少 dispatcher 時測試紅燈。
- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj` 通過。
- `rg -n "ucl\.senddata" Form1.cs` 確認 hook 內已無直接送字。

---

## 2026-05-26 - 移除 C# 版 dist 目錄

### 任務目標

1. C# 版產物回歸 Visual Studio/MSBuild 的 `bin\Debug` / `bin\Release`。
2. 移除容易與 Python 版打包習慣混淆的 `dist` 目錄。

### 實作紀錄

- 刪除 tracked `dist/uclliu.exe` 與 `dist/pinyi.txt`。
- README 發行檔清單移除 `dist` 項目，改註明自行編譯輸出位置。
- `.gitignore` 改為忽略整個 `dist/`，避免之後手動測試產物又被簽入。
- CHANGELOG 補上 C# 版不維護 `dist` 的說明。

### 驗證紀錄

- `rg -n "\bdist\b" README.md CHANGELOG.md history.md .gitignore`

---

## 2026-05-26 - Notepad++ 貼上模式回退

### 任務目標

1. 依使用體感回退 Notepad++ 預設 `Ctrl+V` 貼上出字。
2. 保留出字相容清單集中管理，但不讓 Notepad++ 預設碰剪貼簿。

### 實作紀錄

- 將 Notepad++ 從 `TextOutputCompatibilityDefaults.PasteCtrlVApps` 移除。
- 核心測試改為要求 Notepad++ 預設維持 Unicode `SendInput`。
- README/CHANGELOG 同步移除 Notepad++ 貼上模式描述。

### 驗證紀錄

- 先修改測試期待 Notepad++ 不走貼上，確認舊行為測試失敗。
- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj` 通過。

---

## 2026-05-26 - Notepad++ 出字相容修正

### 任務目標

1. 修正 Notepad++ 中肥米候選可運作但送字吃不到的情況。
2. 避免同類相容清單散在 `uclliu.cs` 與文件中。

### 實作紀錄

- 判斷根因偏向 Notepad++/Scintilla 不穩定接收 Unicode `SendInput`，Codex/Electron 可出字代表 hook 本身仍活著。
- 新增 `TextOutputCompatibilityDefaults` 集中管理內建出字相容清單。
- 將 Notepad++ 加入 `Ctrl+V` 貼上出字清單，避開 `SendInput` 對 Scintilla 的相容問題。
- `uclliu.cs` 改從同一份預設相容清單初始化，減少文件、測試、實作分叉。

### 驗證紀錄

- 先加入 Notepad++ 預設貼上模式測試，確認缺少預設清單時測試紅燈。
- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj` 通過。

---

## 2026-05-25 - C# 版第一輪追功能：轉檔與 UI 熱路徑降載

### 任務目標

1. 參考 Python 版 UCL_LIU 後續功能，建立 C# 版分輪追趕清單。
2. 補 `liu-uni.tab -> liu.cin -> liu.json` 與 `liu.cin -> liu.json` 自動解算流程。
3. 先修 WinForms key hook 熱路徑中最容易造成卡頓的同步負擔。

### 實作紀錄

- 新增 `GOALS.md` 與 `docs/superpowers/plans/2026-05-25-ucl-liu-csharp-first-slice.md`，後續以每輪 2~3 項方式推進。
- 新增 `LiuTableConverter.cs`，提供 CIN 轉 JSON、UNITAB 轉 CIN、啟動時自動補 `liu.json`。
- 新增 `UiLayoutCalculator.cs` 與 `tools/UclLiuCoreTests`，讓核心轉檔與短版 UI 寬度計算可以用 .NET SDK 驗證。
- 將 `uclliu.cs` 啟動流程接上自動轉檔：若缺 `liu.json`，優先使用同目錄 `liu.cin`，再使用同目錄或系統安裝路徑的 `liu-uni.tab`。
- 將預設 debug 關閉，避免低階鍵盤 hook 每次按鍵都同步輸出大量除錯文字。
- 短版模式候選字更新時不再整個 `update_UI()` 重跑，改成只更新必要欄寬。
- `getForegroundWindowProcessInfo()` 加入短暫 cache，降低每次 key hook 都查 `Process.MainModule` 的成本。

### 驗證紀錄

- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj` 通過。
- `dotnet msbuild uclliu.sln /p:Configuration=Debug /p:Platform="Any CPU" /v:minimal` 目前受限於本機缺 .NET Framework 4.5.2 Developer Pack，尚無法完整編譯舊 WinForms 專案。

### 下一輪候選

- 自定詞庫：`custom.json` 合併載入、`,,,BOX`、單例視窗。
- Win11/Chrome/PTT/Notepad 出字相容與剪貼簿復原策略。
- README/CHANGELOG 對齊 Python 版新結構。

---

## 2026-05-25 - C# 版第二輪追功能：自定詞庫

### 任務目標

1. 對齊 Python 版 v1.63~v1.65 的 `custom.json` 自定字詞。
2. 加入 `,,,BOX` 快捷命令與右下角選單入口。
3. 自定詞庫視窗最多只允許開啟一個，已存在時改為聚焦。

### 實作紀錄

- 新增 `CustomDictionaryStore.cs`，負責 `custom.json` 載入、儲存、字根驗證與合併到 `liu.json` 的 `chardefs`。
- 新增核心測試：字根規則、大小寫轉小寫、合併順序、重複值處理、儲存再載入。
- `uclliu.cs` 載入 `liu.json` 後會自動合併同目錄 `custom.json`，並新增 `reload_word_root()` 供編輯器儲存後立即重載。
- 新增 `CustomDictionaryForm.cs`，提供新增/更新/刪除/上下移排序，並在關閉或修改後寫回 `custom.json`。
- `Form1.cs` 增加單例視窗管理；右下角選單加入「自定詞庫」。
- `run_extra()` 增加 `,,,BOX`，可直接開啟自定詞庫。

### 驗證紀錄

- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj` 通過。
- `CustomDictionaryForm.cs` + `CustomDictionaryStore.cs` 以臨時 `net10.0-windows` WinForms 專案編譯通過。
- 完整舊專案仍受限於本機缺 .NET Framework 4.5.2 Developer Pack，無法跑正式 `uclliu.sln` build。

### 下一輪候選

- Win11/Chrome/PTT/Notepad 出字相容與剪貼簿復原策略。
- README/CHANGELOG 對齊 Python 版新結構。
- TSF Bridge 移植評估。

---

## 2026-05-25 - C# 版第三輪追功能：Unicode SendInput 出字

### 任務目標

1. 研究 TSF 以外，比舊 `SendKeys.Send(data)` 更穩的出字方式。
2. 在 C# 版加入 Unicode `SendInput` 預設出字，保留剪貼簿與舊 `SendKeys` fallback。
3. 將剪貼簿貼上集中封裝，避免貼上失敗時直接中斷或永久覆蓋文字剪貼簿。

### 實作紀錄

- 新增 `TextOutput.cs`，包含 `UnicodeSendInputOutput`、`ClipboardPasteOutput` 與 `TextOutputRouter`。
- `UnicodeSendInputOutput` 以 `KEYEVENTF_UNICODE` 建立每個 UTF-16 code unit 的 key down/up 事件，避免 `SendKeys` 特殊字元語法干擾。
- `ClipboardPasteOutput` 加入剪貼簿 retry、送鍵後短暫等待、try/finally 還原原剪貼簿資料。
- `TextOutputRouter` 讓 `DEFAULT` 預設走 Unicode SendInput；特殊 App 與 `PASTE` / `BIG5` 模式仍走剪貼簿貼上。
- `uclliu.senddata()` 改為先走選定輸出策略，失敗時 fallback 到 Unicode SendInput 或舊 `SendKeys`，避免單點失敗造成出字流程中斷。
- 右下角出字模式選單將「正常出字模式」標示為「正常出字模式（Unicode）」。

### 驗證紀錄

- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj` 通過。
- 新增核心測試：Unicode SendInput 事件建立、代理對 surrogate pair 的 UTF-16 保留、剪貼簿貼上失敗後還原、輸出策略選擇。

---

## 2026-05-25 - C# 版第四輪追功能：README/CHANGELOG 現代化

### 任務目標

1. 將 C# 版 README 從舊 HTML 筆記整理成目前可維護的專案說明。
2. 清楚寫出 C# 版目前狀態與 Python 版 v1.67 的差異。
3. 新增 C# 版 CHANGELOG，讓後續每輪追功能有固定紀錄位置。

### 實作紀錄

- 重寫 `README.md`，改成快速開始、目前狀態、常用命令、出字模式、自定詞庫、設定檔、與 Python 版差異、開發與驗證等章節。
- 新增 `CHANGELOG.md`，整理 2019 初版與 2026-05-25 三輪追功能。
- 更新 `GOALS.md`，將 README/CHANGELOG 現代化標記完成。

### 驗證紀錄

- README / CHANGELOG 關鍵字與舊版 emoji 檢查無殘留項目。
- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj` 通過。
- `dotnet msbuild uclliu.sln /p:Configuration=Debug /p:Platform="Any CPU" /v:minimal` 仍受限於本機缺 .NET Framework 4.5.2 Developer Pack。

---

## 2026-05-25 - C# 版第五輪追功能：打字音效與 UX 設定

### 任務目標

1. 補 Python 版後期打字音效、音量與特殊鍵音效。
2. 將托盤選單補上打字音、啟動預設肥/英、允許半全形切換。
3. 讓 `ENABLE_HALF_FULL` 與 `STARTUP_DEFAULT_UCL` 設定真正影響 C# 版行為。

### 實作紀錄

- 新增 `TypingSound.cs`，負責 wav 掃描、特殊鍵分類、音量正規化、PCM 16-bit 音量縮放、背景播放與同鍵長按抑制。
- 複製 Python 版 `wavs\*.wav` 音效素材，並在 `uclliu.csproj` 設定輸出時複製。
- `uclliu.cs` 將 `KEYBOARD_VOLUME` 限制在 0-100，並提供打字音播放、預覽、重載方法。
- `Form1.cs` 在 keyboard hook 熱路徑只做短判斷，實際音效播放走 ThreadPool 與最多 3 個播放工作。
- `Shift+Space` 現在會尊重 `ENABLE_HALF_FULL=0`。
- 啟動後若 `STARTUP_DEFAULT_UCL=0`，會套用英模式。
- 右下角選單新增打字音開關、音量 10%-100%、啟動預設肥模式、允許半全形切換。

### 驗證紀錄

- 先新增核心測試並確認紅燈：缺少 `TypingSoundVolume`、`TypingSoundKeyState`、`TypingSoundCatalog`、`WavPcmVolumeScaler`。
- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj` 通過。
- `dotnet msbuild uclliu.sln ...` 因本機缺完整 .NET Framework 4.5.2 reference assemblies 失敗於 MSB3644。
- `MSBuild.exe uclliu.csproj /p:TargetFrameworkVersion=v4.8 /p:PostBuildEvent=` 臨時覆寫編譯通過，確認 WinForms 整合與 wav copy 無編譯錯。

---

## 2026-05-26 - Shift 黏住與 hook 卡頓修正

### 問題觀察

- 使用者回報切換輸入法或按 Shift 後 UI 偶爾卡頓，並且有時 Shift 狀態像是黏住。
- 追查 `Form1.LowLevelKeyboardProc` 後確認：`Shift keyup` 只有在 `CTRL_SP=0` 時才會清掉 `flag_is_shift_down`。
- low-level hook 只認 `WM_KEYDOWN/WM_KEYUP`，沒有涵蓋 `WM_SYSKEYDOWN/WM_SYSKEYUP`。
- 打字音效第一次播放特定音量時，wav 音量縮放與快取檔準備仍在 hook 執行緒上進行。

### 實作紀錄

- 新增 `KeyboardHookState.cs`，將 hook message 與 Shift release 規則抽成可測邏輯。
- `Shift keyup` 現在無論 `CTRL_SP` 設定都會清掉 Shift 狀態；只有 `CTRL_SP=0` 且沒有按其他鍵時才切換肥/英。
- hook 判斷支援 `WM_SYSKEYDOWN/WM_SYSKEYUP`，避免 Alt/系統組合鍵漏 keyup。
- `is_send_ucl` 檢查提前到 foreground process 查詢之前，減少程式送字造成的 hook 負擔。
- `TypingSoundPlayer` 將 `PrepareSoundPath()` 移入背景 ThreadPool，hook 不再同步做 wav 檔案 I/O。

### 驗證紀錄

- 先新增核心測試並確認紅燈：缺少 `KeyboardHookMessage`、`ShiftKeyReleaseDecision`、`KeyboardHookStateRules`。
- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj` 通過。
- `MSBuild.exe uclliu.sln /t:Rebuild /p:Configuration=Debug /p:Platform="Any CPU"` 通過，ILRepack post-build 也成功。

---

## 2026-05-26 - C# 打字音延遲修正

### 問題觀察

- 使用者回報 C# 版打字音比 Python 版 lag，打起來不爽。
- 對照 Python 版後確認：Python 會把 wav 讀成 `o_song[s]["data"]` 並重用 PyAudio stream。
- C# 版原本每次按鍵都進 ThreadPool，可能查檔案 timestamp、準備縮放檔、建立 `SoundPlayer`，再用 `PlaySync()` 播放，聲音自然會晚一拍。

### 實作紀錄

- `TypingSoundPlayer` 改為預載 wav bytes，建立並快取 `ITypingSoundHandle`。
- 熱路徑只挑音效與呼叫 `Play()`，缺快取時背景補載，不等待音效準備。
- `Form1_Load`、開啟打字音、切換音量時會預熱目前音量的音效。
- cache key 使用掃描 wav 時記下的檔案時間，不再每次按鍵查檔案 metadata。

### 驗證紀錄

- 先新增核心測試並確認紅燈：缺少 `ITypingSoundPlaybackEngine` / `ITypingSoundHandle`。
- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj` 通過。
- `MSBuild.exe uclliu.sln /t:Rebuild /p:Configuration=Debug /p:Platform="Any CPU"` 通過。

---

## 2026-05-26 - C# 打字音斷聲修正

### 問題觀察

- 使用者回報打字音雖有改善，但連打時仍有斷聲感。
- 追查後判斷瓶頸不只是 thread 數量，而是 `SoundPlayer.Play()` 同一播放模型不適合高速重疊短音效，容易讓前一段聲音被後一段切掉。
- 專案音效素材皆為 PCM 16-bit wav，可直接用 Windows 內建 `winmm.dll` 播放，不需要額外 DLL 或 NuGet。

### 實作紀錄

- 新增 `Pcm16WavData` 解析 PCM 16-bit wav 的 format/data chunk。
- 打字音播放引擎改為 `WaveOutPlaybackEngine`，每次按鍵建立獨立 `waveOut` one-shot playback handle。
- 每個音效同時最多保留 8 個播放工作，避免連打時無限制累積背景工作。
- 播放前提高 ThreadPool 最小 worker 數到同時出聲上限，降低短音效排隊造成的延遲感。
- `waveOutPrepareHeader` 後統一在 `finally` unprepare/close/free，避免播放失敗時殘留 native 資源。

### 驗證紀錄

- 先新增核心測試並確認紅燈：缺少 `Pcm16WavData`。
- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj` 通過。

---

## 2026-05-26 - 新版 pinyi.txt 同音字候選修正

### 問題觀察

- 使用者回報同音字第 0 字會出現注音，推測與新舊 `pinyi.txt` 格式差異有關。
- 新版 `pinyi.txt` 以 `VERSION_0.01` 開頭，前 3 行是檔頭，每筆資料第 0 欄是注音碼，例如 `u 一 壹 ... ㄧ`。
- C# 版原本將整行空白切開後全部當候選，導致注音碼或純注音符號可能混入候選字。
- Python 版 v0.01 會跳過每筆資料第 0 欄，只用後面的同音字資料。

### 實作紀錄

- 新增 `PinyiCandidateSelector.cs`，集中處理同音字候選解析。
- 新版 `VERSION_0.01` 格式會跳過前 3 行檔頭與每行第 0 欄注音碼。
- 同音字候選會過濾純注音符號 token，避免 `ㄧ`、`ㄢ` 等出現在候選第 0 字。
- 舊版無版本檔頭格式維持整行候選邏輯，保留相容。
- `uclliu.use_pinyi()` 改用 selector 做查詢與分頁。

### 驗證紀錄

- 先新增核心測試並確認紅燈：缺少 `PinyiCandidateSelector`。
- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj` 通過。
- `MSBuild.exe uclliu.sln /t:Rebuild /p:Configuration=Debug /p:Platform="Any CPU"` 通過，ILRepack post-build 成功。

---

## 2026-05-26 - `,,,x` / `,,,z` 框選轉換修正

### 問題觀察

- 使用者回報 `,,,x` 與 `,,,z` 目前沒成功。
- 對照 Python 版後確認：Python 版會先清空剪貼簿，再同步送 `Ctrl+C` 取得目前框選文字，避免複製失敗時讀到舊剪貼簿。
- 進一步評估後，`Ctrl+C` 應作為最後 fallback；標準文字控制項可先嘗試 UI Automation，傳統 Win32 控制項可再嘗試 `WM_COPY`。
- C# 版仍使用舊式 `SendKeys.Send("^C")` 搭配固定 `Sleep(500)`，沒有先清剪貼簿；`,,,x/z` 分支也沒有清掉 `last_key` 或回傳 `true` 吃掉尾鍵。
- Notepad++ 的編輯區是 Scintilla，實測仍不穩，推測原因是 C# 版在 low-level keyboard hook callback 內同步做取字、複製與出字，目標 App 還沒機會處理 copy 訊息。

### 實作紀錄

- 新增 `SelectedTextTransformCommand`，集中處理框選文字讀取、剪貼簿備份/還原與 retry。
- 框選文字來源順序改為 UI Automation `TextPattern.GetSelection()`、`WM_COPY`、安全版 `Ctrl+C`。
- `,,,z` 改為「框選文字 -> 簡轉繁 -> 最簡字根 -> 出字」。
- `,,,x` 改為「框選字根 -> 文字 -> 出字」。
- 兩個指令執行後會清掉 `last_key` 並回傳 `true`，避免尾鍵繼續進入後續 hook 流程。
- `,,,x/z` 只在 hook 內排程工作，真正取字、轉換與出字改由 WinForms `BeginInvoke` 延後到 hook 回傳後執行。
- 取字與出字期間保留 `is_send_ucl` 防護，避免自己的快捷鍵與 `SendInput` 被 hook 當成使用者輸入。

### 驗證紀錄

- 先新增核心測試並確認紅燈：缺少 `SelectedTextTransformCommand` / `ISelectedTextSource` / hook 外排程 dispatcher。
- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj` 通過。
- `MSBuild.exe uclliu.sln /t:Rebuild ...` 的正式輸出被正在執行的 `uclliu.exe` 鎖住；改用臨時 `OutDir` build 通過。

---

## 2026-05-26 - `,,,x` / `,,,z` Scintilla 取字與反查加速

### 問題觀察

- 使用者回報必須先手動複製或剪下框選文字，之後按 `,,,x` / `,,,z` 才會生效，代表 C# 版還是沒有穩定取得目前框選字。
- Notepad++ 編輯區是 Scintilla；直接跨 process 讀 Scintilla buffer 需要傳 pointer，風險高且不適合在這裡做。
- Python 版後期有 `uclcode_rr` 字根對字的 hash；C# 版只保留 `uclcode_r`，解算與候選碼還原都不完整。

### 實作紀錄

- 新增 `ScintillaCopySelectedTextSource`，偵測 focused control class name 包含 `Scintilla` 時送 `SCI_COPY`，排在 UI Automation 後、一般 `WM_COPY` 前。
- 將 focused window / class name / `SendMessageTimeout` 抽成 `IFocusedWindowGateway`，保留 250ms timeout，避免目標視窗卡住拖慢 UI。
- 新增 `LiuReverseLookupTable`，建立 `word -> code` 與 `code -> word` 兩張表；同根第 2 候選起會依 Python 版輸出 `root1`、`root2`。
- `uclcode_to_chinese()` 先查 `uclcode_rr`，再 fallback 到 `v/r/s/f/w` 候選尾碼與原本字根表。

### 驗證紀錄

- 先新增核心測試並確認紅燈：缺少 `IFocusedWindowGateway` / `ScintillaCopySelectedTextSource`。
- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj` 通過。
- `MSBuild.exe uclliu.sln /t:Rebuild /p:Configuration=Debug /p:Platform="Any CPU"` 搭配臨時 `OutDir` 通過，僅保留既有 `Form1.lParam` 未使用警告。

---

## 2026-05-26 - 主視窗不出現在 Alt+Tab

### 問題觀察

- 使用者回報 Python 版按 Alt+Tab 不會看到肥米浮動視窗，但 C# 版會出現在切換器。
- `Form1.Designer.cs` 已有 `ShowInTaskbar=false`，因此問題不是 taskbar button，而是 top-level window 沒有 tool window extended style。

### 實作紀錄

- 新增 `AltTabWindowStyle`，集中處理 `WS_EX_TOOLWINDOW` / `WS_EX_APPWINDOW` bit。
- `Form1.CreateParams` 套用 `HideFromSwitcher()`，讓主浮動視窗保留顯示但不被 Alt+Tab 收進 App 清單。

### 驗證紀錄

- 先新增核心測試並確認紅燈：缺少 `AltTabWindowStyle`。
- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj` 通過。
- `MSBuild.exe uclliu.sln /t:Rebuild /p:Configuration=Debug /p:Platform="Any CPU"` 搭配臨時 `OutDir` 通過，僅保留既有 `Form1.lParam` 未使用警告。

---

## 2026-05-26 - `,,,x` / `,,,z` 取字流程收斂

### 問題觀察

- 使用者回饋 Scintilla / Win32 gateway 分支過度複雜，先回到原本靠剪貼簿取框選文字的做法。
- 重新對照 Python 版後確認原版使用 `Ctrl+C`，不是 `Ctrl+X`；用 `Ctrl+X` 會先剪掉目標文字，失敗時風險較高。

### 實作紀錄

- 移除 `ISelectedTextSource`、UI Automation、`WM_COPY`、Scintilla `SCI_COPY` 與 focused window gateway。
- `SelectedTextTransformCommand` 收斂為單一路徑：備份剪貼簿、清空剪貼簿、送 `Ctrl+C`、retry 讀取 Unicode 文字、轉換後送出、最後還原剪貼簿。
- 移除不再使用的 UI Automation 參考與測試專案 WPF 設定。
- 保留 `LiuReverseLookupTable` / `uclcode_rr` 反查 hash 加速。

### 驗證紀錄

- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj` 通過。
- `MSBuild.exe uclliu.sln /t:Rebuild /p:Configuration=Debug /p:Platform="Any CPU"` 搭配臨時 `OutDir` 通過，僅保留既有 `Form1.lParam` 未使用警告。

---

## 2026-05-26 - Notepad++ 切入時 hook 熱路徑降載

### 問題觀察

- 使用者回報剛進 Notepad++ 時有時像 crash，短時間無法切換，過一會又恢復。
- 追查 `LowLevelKeyboardProc` 後發現每個 key event 一開始都呼叫 `getForegroundWindowProcessInfo()`。
- 該流程會同步讀 foreground title、PID，且原本會碰 `Process.MainModule.FileName`；在 low-level keyboard hook 內做這些跨 process 查詢，切換到剛載入的 App 時容易造成短暫卡住。

### 實作紀錄

- 新增 `ForegroundProcessSnapshot`，集中 normalize process name 與 snapshot dictionary。
- hook 前段 no-ucl app 判斷改呼叫 `getForegroundWindowProcessName()`，只查 foreground PID 與 `Process.ProcessName`，不讀視窗標題、不碰 `MainModule`。
- `getForegroundWindowProcessInfo()` 仍保留完整 title 給出字路由使用，但 process name 也改用 `ProcessName`，避免 `MainModule` 在特殊 App 上卡住或丟權限例外。

### 驗證紀錄

- 先新增核心測試並確認紅燈：缺少 `ForegroundProcessSnapshot`。
- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj` 通過。
