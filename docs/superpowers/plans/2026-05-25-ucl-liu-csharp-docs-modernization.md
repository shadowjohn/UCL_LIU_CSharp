# C# 版 README/CHANGELOG 現代化 Slice

## 範圍

- 重整 `README.md`，讓新讀者能快速理解 C# 版現在可用什麼、缺什麼。
- 新增 `CHANGELOG.md`，將近期追 Python 版功能集中記錄。
- 更新 `GOALS.md` 與 `history.md`。

## 實作步驟

1. 對照 C# 版 `history.md`、`GOALS.md` 與 Python 版 `README.md` / `CHANGELOG.md`。
2. 將 C# README 改為 Markdown 結構：
   - 目前狀態
   - 快速開始
   - 常用命令
   - 出字模式
   - 自定詞庫
   - 與 Python 版主要差異
   - 開發與驗證
3. 新增 C# 版 CHANGELOG：
   - Unreleased 文件更新
   - Unicode SendInput 出字
   - 自定詞庫
   - 轉檔與 UI 熱路徑降載
   - 2019 v0.1 初版
4. 跑核心測試與基本文字檢查。

## 驗證

- `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj`
- 檢查 README / CHANGELOG 沒有殘留 placeholder 或舊版省略字樣。
