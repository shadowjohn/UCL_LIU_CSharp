# C# 版 Unicode SendInput 出字 Slice

## 範圍

- 將 `DEFAULT` 出字模式從舊 `SendKeys.Send(data)` 改為 Unicode `SendInput`。
- 將剪貼簿貼上封裝成可 retry、可還原、可 fallback 的輸出策略。
- 保留 `PASTE`、`BIG5` 與特殊 App 清單，不在本輪移植 TSF bridge。

## 實作步驟

1. 先補核心測試：
   - Unicode `SendInput` 每個 UTF-16 code unit 產生 key down/up。
   - surrogate pair 不被拆成錯誤 code point。
   - 剪貼簿貼上送鍵失敗時還原原文字。
   - `DEFAULT` 優先 Unicode，特殊 App 仍走 paste/Big5。
2. 新增 `TextOutput.cs`：
   - `UnicodeSendInputOutput`
   - `ClipboardPasteOutput`
   - `TextOutputRouter`
3. 改 `uclliu.senddata()`：
   - 先清狀態與簡繁轉換。
   - 根據模式與前景 process 選策略。
   - 策略失敗時 fallback，並寫 debug log。
4. 更新 `GOALS.md`、`history.md`。

## 驗證

- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj`
- 舊 WinForms 專案完整 build 仍需本機安裝 .NET Framework 4.5.2 Developer Pack。
