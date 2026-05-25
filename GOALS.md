# UCL_LIU_CSharp Goals

本檔追蹤 C# 版追上 Python 版 UCL_LIU 的開發節奏。每輪以 2~3 個項目為單位，完成後 build/test、檢查 diff、commit。

## 第一輪：基礎穩定與轉檔

- [x] 降低 WinForms key hook 熱路徑負擔，改善短版 UI 打快時卡頓或當機風險。
- [x] 補 `liu-uni.tab -> liu.cin -> liu.json` 與 `liu.cin -> liu.json` 自動解算流程。
- [x] 補可由 .NET SDK 執行的核心測試，避免舊 .NET Framework target pack 缺失時完全無法驗證。

## 第二輪候選

- [x] 對齊 Python 版 v1.63~v1.65 自定詞庫：`custom.json` 合併載入、`,,,BOX`、單例視窗。
- [x] 補 Unicode SendInput 預設出字，並將剪貼簿貼上集中成 retry/restore fallback。
- [x] 補 Win11/Chrome/PTT/Notepad 出字模式相容清單與標題規則。
- [x] README/CHANGELOG 現代化，將 C# 版目前狀態與 Python 版差異寫清楚。

## 第三輪候選

- [ ] 評估 TSF Bridge 移植方式，先做外掛式 fallback，不直接打進核心出字流程。
- [ ] 補管理員權限導引與 TSF 註冊/解除註冊選單。
- [x] 補打字音效、音量、半全形切換設定等 Python 版後期 UX 功能。

## 第四輪候選

- [x] 補 Win11/Chrome/PTT/Notepad 出字模式相容清單與標題規則。
- [ ] 評估 OpenCC / 特殊字修正移植範圍。

## 持續原則

- 小步 commit，每次只混 2~3 個相關項目。
- 先保舊流程，再補新能力。
- UI hook 路徑避免長時間 I/O、剪貼簿等待、Process 查詢與 layout 重算。
