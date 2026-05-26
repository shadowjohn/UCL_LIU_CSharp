# 開發對話紀錄

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
