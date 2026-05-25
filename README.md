# UCL_LIU_CSharp

C# / WinForms 版肥米輸入法，目前版本 v0.11。這個分支的目標是保留 2019 年 C# 版輕量、單檔、低依賴的優點，同時逐步追上 Python 版 UCL_LIU 後來累積的穩定性與工具鏈。

目前 C# 版仍不是 Python 版 v1.67 的完整替代品。它已補上字碼表自動轉換、自定詞庫、Unicode SendInput 出字、打字音效 UX 與核心測試；TSF Bridge 與更完整的 Windows 11 應用相容清單仍在後續項目。

![UCL_LIU C# screenshot](screenshot/ucl_1.png)

## 目前狀態

| 項目 | C# 版目前狀態 | Python 版對照 |
| --- | --- | --- |
| 核心輸入 | WinForms + low-level keyboard hook | PyHook / PyWin32 |
| 預設出字 | Unicode `SendInput`，失敗 fallback 舊 `SendKeys` | TSF 可選，失敗 fallback `SendKeysCtypes` / paste |
| 貼上模式 | 已封裝 retry、timeout、try/finally 還原剪貼簿 | 已有大量 App 特例 |
| 字碼表 | 支援 `liu.json`、`liu.cin`、`liu-uni.tab` 自動轉換 | 支援來源較多，含 RIME/fcitx/小小輸入法等 |
| 自定詞庫 | 支援 `custom.json`、右下角選單、`,,,BOX`、單例視窗 | v1.63-v1.65 已完整支援 |
| UI 效能 | 已降低短版模式與 foreground process 查詢熱路徑負擔 | Python 版後期另有多輪 Win11/位置修正 |
| TSF Bridge | 尚未移植 | v1.67 實驗性支援 |
| 打字音效 | 支援 `wavs\*.wav`、開關、10%-100% 音量、特殊鍵音效、防長按連發 | 已支援音效與音量設定 |
| 同音/注音 | 支援新版 `pinyi.txt` 同音字、`';` 注音查詢、出字後提示注音 | 已支援 |
| 啟動與半全形 UX | 支援啟動預設肥/英、允許停用 `Shift+Space` 半全形切換 | 已支援 |
| 測試 | `tools/UclLiuCoreTests` 可用 .NET SDK 跑核心測試 | Python 版以實機與歷史回報為主 |

## 快速開始

1. 下載或編譯 `uclliu.exe`。
2. 將字碼表放在 `uclliu.exe` 同一個目錄，三選一即可：
   - `liu.json`
   - `liu.cin`
   - `liu-uni.tab`
3. 可選：放入 `pinyi.txt` 啟用同音字與注音查詢。
4. 執行 `uclliu.exe`。

因字碼表版權問題，本專案不提供 `liu.json` / `liu.cin` / `liu-uni.tab`。若使用 `liu.cin` 或 `liu-uni.tab`，C# 版啟動時會自動產生 `liu.json`。

既有發行檔位置：

| 檔案 | 說明 |
| --- | --- |
| [dist/uclliu.exe](dist/uclliu.exe) | 倉庫內既有 C# 版執行檔 |
| [dist/pinyi.txt](dist/pinyi.txt) | 同音字庫，放在 exe 旁 |

目前倉庫內 `dist/uclliu.exe` 不一定包含最新開發分支變更；正式重打包需可完整編譯 .NET Framework 4.5.2 WinForms 專案。

## 常用命令

| 命令 | 功能 |
| --- | --- |
| `,,,unlock` | 正常模式 |
| `,,,lock` | 遊戲模式 |
| `,,,version` | 查看版本 |
| `,,,c` / `,,,t` | 簡體 / 繁體切換 |
| `,,,s` / `,,,l` | UI 縮窄 / 拉寬 |
| `,,,+` / `,,,-` | UI 放大 / 縮小 |
| `,,,z` | 將反白文字轉成字根 |
| `,,,x` | 將反白字根轉回文字 |
| `,,,BOX` | 開啟自定詞庫 |
| `';zo6` | 注音查詢「ㄈㄟˊ」，例如候選會含「肥」 |

## 出字模式

C# 版目前右下角選單提供三種出字模式：

| 模式 | 目前行為 |
| --- | --- |
| 正常出字模式（Unicode） | 預設使用 Unicode `SendInput`，避免舊 `SendKeys` 對 `+ ^ % { } ( )` 等字元的語法干擾 |
| BIG5 模式 | 以 Big5 文字貼上，保留給舊程式與特定環境 |
| 複製貼上模式 | 使用剪貼簿貼上，適合 PuTTY、PCMan、部分遊戲或特殊文字框 |

貼上流程已改為集中封裝：先備份剪貼簿、設定輸出文字、送出 `Ctrl+V` 或 `Shift+Insert`、最後盡量還原原本剪貼簿。

### 內建相容規則

| 環境 | 判斷方式 | 出字模式 |
| --- | --- | --- |
| Chrome / Edge / Brave / Firefox / Opera 開 PTT | 視窗標題包含 `批踢踢實業坊`、`term.ptt.cc` 或 `ws.ptt.cc` | `Ctrl+V` 貼上 |
| Win11 Notepad | Windows build >= 22000 且 process 為 `notepad` / `notepad.exe` | `Ctrl+V` 貼上 |
| PuTTY / PCMan / Pietty / Windows Terminal / mintty / RimWorld 等 | process 相容清單 | `Shift+Insert` 貼上 |
| Oxygen Not Included / PhotoImpact `iedit_` | process 相容清單 | `Ctrl+V` 貼上 |
| zip32w / DaqKing / EWinner | process 相容清單 | Big5 `Ctrl+V` 貼上 |

process 規則會自動忽略大小寫與 `.exe` 副檔名，`notepad` 與 `notepad.exe` 視為同一個程式。

## 自定詞庫

自定詞庫檔案為 `custom.json`，放在 `uclliu.exe` 同目錄。啟動時會自動合併到 `liu.json` 的 `chardefs`。

支援方式：

- 右下角選單開啟「自定詞庫」
- 輸入 `,,,BOX`
- 編輯器儲存後立即重載字根
- 同一時間只開一個自定詞庫視窗，已開啟時會聚焦

字根規則目前與 Python 版後期一致：允許 `a-z,.]['`，最長 5 碼，會統一轉小寫。

## 設定檔

設定檔位置仍沿用 `UCLLIU.ini`。常用欄位：

```ini
[DEFAULT]
short_mode = 0
zoom = 0.90
send_kind_1_paste =
send_kind_2_big5 =
alpha = 1
x = 1239
y = 950
ctrl_sp = 1
play_sound_enable = 0
keyboard_volume = 30
show_phone_code = 0
startup_default_ucl = 1
enable_half_full = 1
```

`send_kind_1_paste`、`send_kind_2_big5`、`send_kind_3_noucl` 可填入額外 process 名稱，用逗號分隔。
`play_sound_enable` 控制打字音，`keyboard_volume` 會限制在 0-100；右下角選單提供 10%-100% 快速切換。`show_phone_code=1` 時出字後會提示注音讀音。`startup_default_ucl=0` 時啟動預設為英模式，`enable_half_full=0` 時停用 `Shift+Space` 半全形切換。

## 與 Python 版主要差異

Python 版 UCL_LIU 已演進到 v1.67，功能較完整；C# 版目前是追功能中的現代化版本。

C# 版已追上的重點：

- `liu-uni.tab -> liu.cin -> liu.json`
- `liu.cin -> liu.json`
- `custom.json` 自定詞庫
- `,,,BOX`
- 自定詞庫單例視窗
- 預設 Unicode `SendInput` 出字
- 剪貼簿 paste retry / restore fallback
- 打字音效、音量設定、特殊鍵音效
- 啟動預設肥/英與 `Shift+Space` 半全形切換設定
- 短版 UI 熱路徑降載
- foreground process 查詢短暫 cache

Python 版仍領先的重點：

- TSF Bridge 出字模式與註冊/解除註冊管理
- 更完整的 Chrome / Edge / Brave / Opera / PTT / Notepad / VBA / Neovim 等相容規則
- OpenCC 簡繁轉換與更多特殊字修正
- 更多字碼表來源轉換
- 管理員權限導引與重啟流程
- 多年累積的問題回報修正

後續追蹤請看 [GOALS.md](GOALS.md) 與 [history.md](history.md)。

## 開發與驗證

主專案：

- Visual Studio 2019 / 2022
- .NET Framework 4.5.2 Developer Pack
- WinForms
- 外部 NuGet：無

核心測試：

```powershell
dotnet run --project .\tools\UclLiuCoreTests\UclLiuCoreTests.csproj
```

完整舊專案 build：

```powershell
dotnet msbuild .\uclliu.sln /p:Configuration=Debug /p:Platform="Any CPU" /v:minimal
```

目前開發機若缺 `.NETFramework,Version=v4.5.2` reference assemblies，完整 build 會在環境檢查階段失敗；核心測試仍可用 .NET SDK 驗證純邏輯。

## 專案檔案

| 檔案 | 說明 |
| --- | --- |
| `uclliu.cs` | 核心輸入法邏輯、字根載入、命令處理 |
| `Form1.cs` | WinForms UI、tray menu、keyboard hook |
| `TextOutput.cs` | Unicode SendInput、剪貼簿貼上、出字策略選擇 |
| `TypingSound.cs` | 打字音效、音量縮放、wav 分類、防長按連發 |
| `LiuTableConverter.cs` | `liu-uni.tab` / `liu.cin` / `liu.json` 轉換 |
| `CustomDictionaryStore.cs` | `custom.json` 載入、儲存、合併 |
| `CustomDictionaryForm.cs` | 自定詞庫編輯器 |
| `UiLayoutCalculator.cs` | 可測試的 UI 寬度計算 |
| `tools/UclLiuCoreTests` | 核心測試 harness |
| `wavs` | Python 版後期打字音效 wav 素材 |
| `CHANGELOG.md` | C# 版近期變更 |
| `GOALS.md` | 追 Python 版功能清單 |
| `history.md` | 開發對話與驗證紀錄 |

## 版權與作者

- 作者：羽山秋人 ([3wa.tw](https://3wa.tw))
- 信箱：uclliu.3wa@gmail.com
- 授權：MIT License
