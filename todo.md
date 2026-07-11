# TODO

## v0.17 智慧候選字

設計文件：[`docs/superpowers/specs/2026-07-11-smart-candidate-design.md`](docs/superpowers/specs/2026-07-11-smart-candidate-design.md)

### 候選資料

- [x] 確認 `libchewing-data` 詞庫資料的逐檔授權與再散布條件。
- [x] 建立 `tsi.csv` 等來源詞庫轉換成 UTF-8 TSV `candidate.txt` 的工具。
- [x] 產生小型測試候選表，正式候選表確認授權後再放入 Git。
- [x] 讓完整 tag 發行包包含 `candidate.txt`，單檔版不包含。

### 核心功能

- [x] 實作候選表記憶體載入、錯誤行略過與穩定排序。
- [x] 實作送字後的單字／詞組預測，每頁最多 5 筆。
- [x] 實作 `Shift+1`～`Shift+5` 選字與 `Shift+Space` 優先翻頁。
- [x] 實作 Esc、新字根、英／肥切換、句尾與 Enter 關閉候選。
- [x] 保留逗號上下文；句尾、Enter 與模式切換會結束上下文。
- [x] v0.17 僅使用靜態候選表與上游詞頻固定排序，候選最多 3 個 Unicode scalar。
- [ ] 未來版本重新評估可明確開啟、可預期且不自我延伸的個人學習。

### 個人記憶

- [x] v0.17 不載入、不更新、不保存或清除 `candidate_memory.json`；既有檔案原樣保留。

### 選單與 UI

- [x] 新增 `12. 候選字相關`，將離開移到 `13. 離開(Quit)`。
- [x] 缺少候選表時只顯示 `請先下載候選字`，開啟專案 GitHub。
- [x] 候選表存在時只顯示總開關與連續出字。
- [x] 長版 UI 依候選內容自適展開，但不得超出螢幕工作區。
- [x] 短版 UI 保持目前緊湊布局。

### 驗證與文件

- [x] 補候選解析、靜態排序、max 3、設定 migration、按鍵優先權與 UI 寬度核心測試。
- [x] 確認原本字根、注音、同音、半／全形與遊戲模式沒有退步。
- [x] 更新 README、CHANGELOG、設定欄位與候選表來源／授權說明。
- [ ] 使用 `bin\Debug\uclliu.exe` 完成靜態候選鍵盤／UI smoke：候選載入、選單 12/13、總開關預設關閉／連續出字預設開啟且僅顯示這兩項、候選最多 3 個 Unicode scalar、`Shift+1`～`Shift+5`、`Shift+Space` 分頁、Esc／新字根／英肥切換取消、句尾／Enter 邊界，並確認 `candidate_memory.json` 不因等待或離開而更新。
