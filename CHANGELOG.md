# CHANGELOG

C# 版肥米輸入法更新紀錄。Python 版完整歷史請看 [UCL_LIU CHANGELOG](https://github.com/shadowjohn/UCL_LIU/blob/master/CHANGELOG.md)。

---

## [Unreleased] - 2026-05-25

### Documentation

- 現代化 `README.md`，改成快速開始、目前狀態、出字模式、與 Python 版差異、開發驗證等章節。
- 新增本檔，將 C# 版近期追功能獨立整理，避免 README 繼續混入大量歷史流水帳。

### Notes

- 這次只更新文件，不改執行邏輯。

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
