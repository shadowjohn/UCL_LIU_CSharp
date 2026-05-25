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
