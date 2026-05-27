# UCL_LIU_CSharp

C# / WinForms 版肥米輸入法，目前版本 v0.12。這個分支的目標是保留 2019 年 C# 版輕量、低依賴、好攜帶的優點，同時逐步追上 Python 版 UCL_LIU 後來累積的穩定性與工具鏈。

目前 C# 版已進入可日用測試的現代化復刻狀態：已補上字碼表自動轉換、自定詞庫、Unicode SendInput 出字、剪貼簿 fallback、打字音效、同音/注音查詢、Win11/Chrome/PTT/Notepad 相容規則、TSF Bridge 外掛 fallback 與核心測試。它仍不是 Python 版 v1.67 的完整替代品；OpenCC/特殊字修正、更多長尾 App 相容規則與 TSF 實機調校仍在後續項目。

![UCL_LIU C# screenshot](screenshot/ucl_1.png)

## 成熟度評估

| 版本 | 實用成熟度 | 工程可維護性 | 日用信心 | 定位 |
| --- | ---: | ---: | ---: | --- |
| Python 版 v1.67 | 8.5 / 10 | 6.5 / 10 | 8.5 / 10 | 功能最完整的正史版，多年實戰相容性最足 |
| C# 版 v0.12 | 7.8 / 10 | 8.1 / 10 | 7.8 / 10 | TSF 出字已可日用測試、低依賴、可維護的現代化復刻版 |

## 目前狀態

| 項目 | C# 版目前狀態 | Python 版對照 |
| --- | --- | --- |
| 核心輸入 | WinForms + low-level keyboard hook | PyHook / PyWin32 |
| 預設出字 | Python-style 逐字 Unicode `SendInput`，失敗 fallback 舊 `SendKeys` | TSF 可選，失敗 fallback `SendKeysCtypes` / paste |
| 貼上模式 | 已封裝 retry、timeout、try/finally 還原剪貼簿 | 已有大量 App 特例 |
| 字碼表 | 支援 `liu.json`、`liu.cin`、`liu-uni.tab` 自動轉換 | 支援來源較多，含 RIME/fcitx/小小輸入法等 |
| 自定詞庫 | 支援 `custom.json`、右下角選單、`,,,BOX`、單例視窗 | v1.63-v1.65 已完整支援 |
| UI 效能 | 已降低短版模式與 foreground process 查詢熱路徑負擔；短版啟動即套用 packed layout，`肥` / `半` / `╳` chrome 尺寸與長版一致，並移除按鈕 focus 後的按下視覺殘留 | Python 版後期另有多輪 Win11/位置修正 |
| TSF Bridge | 已移植為手動 `TSF出字模式`，透過 named pipe 呼叫外掛 DLL，失敗 fallback Unicode `SendInput`；右下角選單可檢查/註冊/解除註冊 | v1.67 實驗性支援 |
| 打字音效 | 支援 `wavs\*.wav`、開關、10%-100% 音量、特殊鍵音效、防長按連發 | 已支援音效與音量設定 |
| 同音/注音 | 支援新版 `pinyi.txt` 同音字、`';` 注音查詢、出字後提示注音 | 已支援 |
| 啟動與半全形 UX | 支援啟動預設肥/英、允許停用 `Shift+Space` 半全形切換 | 已支援 |
| 測試 | `tools/UclLiuCoreTests` 可用 .NET SDK 跑核心測試 | Python 版以實機與歷史回報為主 |

## 低依賴狀態

C# 版 v0.12 目前已整理成「無外部 NuGet、無 DLL 合併」的乾淨 WinForms 專案。主程式只使用 .NET Framework 4.5.2 內建 reference，功能邏輯盡量收在專案原始碼裡，方便直接用 Visual Studio 或 MSBuild 重建。

TSF Bridge 是例外：它是可選外掛，不合併進主程式、不列入預設出字流程；只有手動切到 `TSF出字模式` 時才會透過 named pipe 呼叫 `tsf_bridge\UclTsfBridge.dll`。

| 類別 | 目前狀態 |
| --- | --- |
| NuGet 套件 | 無 |
| 外部 DLL | 主程式無；選配 TSF Bridge 會隨附 `tsf_bridge\UclTsfBridge.dll` |
| JSON | 使用 .NET 內建 `System.Web.Script.Serialization.JavaScriptSerializer`，不再依賴 `System.Json` |
| INI | 使用專案內 `SimpleIni.cs`，不再依賴 `ini-parser` / `INIFileParser` |
| 打包合併 | 不再使用 ILRepack，不需要把套件 DLL 合併進 exe |
| 開發輸出 | 直接使用 `bin\Debug` / `bin\Release`，不再維護 Python 版式的 `dist` 目錄 |
| 可選資料檔 | `liu.json` / `liu.cin` / `liu-uni.tab`、`pinyi.txt`、`wavs\*.wav`、`tsf_bridge\*` |

目前 `uclliu.csproj` 的 reference 只保留：

- `System`
- `System.Core`
- `System.Data`
- `System.Drawing`
- `System.Web.Extensions`
- `System.Windows.Forms`

## 快速開始

1. 下載或編譯 `uclliu.exe`。
2. 將字碼表放在 `uclliu.exe` 同一個目錄，三選一即可：
   - `liu.json`
   - `liu.cin`
   - `liu-uni.tab`
3. 可選：放入 `pinyi.txt` 啟用同音字與注音查詢。
4. 可選：保留 `tsf_bridge` 目錄，右下角選單可註冊 TSF Bridge 並手動切到 `TSF出字模式`。
5. 執行 `uclliu.exe`。

因字碼表版權問題，本專案不提供 `liu.json` / `liu.cin` / `liu-uni.tab`。若使用 `liu.cin` 或 `liu-uni.tab`，C# 版啟動時會自動產生 `liu.json`。

發行檔位置：

| 檔案 | 說明 |
| --- | --- |
| [RELEASE/0.12/uclliu.exe](RELEASE/0.12/uclliu.exe) | v0.12 開發分支打包版 |
| [RELEASE/0.12/uclliu.zip](RELEASE/0.12/uclliu.zip) | v0.12 壓縮版，含選配 TSF Bridge 外掛 |
| [RELEASE/0.11/uclliu.exe](RELEASE/0.11/uclliu.exe) | v0.11 開發分支打包版 |
| [RELEASE/0.11/uclliu.zip](RELEASE/0.11/uclliu.zip) | v0.11 壓縮版 |
| [RELEASE/0.1/uclliu.exe](RELEASE/0.1/uclliu.exe) | 2019 初版備份 |

v0.12 發行日期：2026-05-27。v0.12 病毒碼掃描已提交 Microsoft WDSI：[submission a42546c1-4432-40f2-8cc6-6e226617cf19](https://www.microsoft.com/en-us/wdsi/submission/a42546c1-4432-40f2-8cc6-6e226617cf19)。

v0.11 病毒碼掃描紀錄：[submission 2a365b04-dea0-496f-937f-9051b163a968](https://www.microsoft.com/en-us/wdsi/submission/2a365b04-dea0-496f-937f-9051b163a968)。

若自行編譯，請使用 .NET Framework 4.5.2 WinForms 專案重建。C# 版開發輸出以 `bin\Debug\uclliu.exe` 或 `bin\Release\uclliu.exe` 為準，不另外維護 `dist` 目錄；請確認 `pinyi.txt`、`wavs`、`tsf_bridge`、`liu.json` 或可轉換字碼表與 exe 放在同一目錄。

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

C# 版目前右下角選單提供四種出字模式：

| 模式 | 目前行為 |
| --- | --- |
| 正常出字模式（Unicode） | 預設使用逐字 Unicode `SendInput`，貼近 Python 版 `SendKeysCtypes` 每字送出的節奏，並避免舊 `SendKeys` 對 `+ ^ % { } ( )` 等字元的語法干擾 |
| BIG5 模式 | 以 Big5 文字貼上，保留給舊程式與特定環境 |
| 複製貼上模式 | 使用剪貼簿貼上，適合 PuTTY、PCMan、部分遊戲或特殊文字框 |
| TSF出字模式 | 透過外掛 `UclTsfBridge.dll` named pipe 出字；若 pipe、TSF context 或 DLL 狀態失敗，會 fallback 回 Unicode `SendInput`。實機測試 Notepad++ 開啟自動完成時也可正常出字 |

貼上流程已改為集中封裝：先備份剪貼簿、設定輸出文字、送出 `Ctrl+V` 或 `Shift+Insert`、最後盡量還原原本剪貼簿。

### TSF Bridge 管理

TSF Bridge 不會自動啟用，也不會自動註冊。右下角選單提供「TSF Bridge 管理」：

- 檢查 TSF Bridge 狀態：顯示 DLL、註冊狀態、目前權限與註冊位置。
- 註冊 TSF Bridge：透過 UAC 執行 `tsf_bridge\register_tsf_bridge.bat`。
- 解除註冊 TSF Bridge：透過 UAC 執行 `tsf_bridge\unregister_tsf_bridge.bat`。
- 解除 DLL 封鎖：執行 `tsf_bridge\unlock_tsf_bridge.ps1`。
- 開啟 Windows 輸入法設定：協助加入或切換 `UCLLIU TSF Bridge`。

切到 `TSF出字模式` 後，C# 版會先嘗試連線 `uclliu_tsf_bridge_<foreground pid>`，再退回全域 `uclliu_tsf_bridge`。預設 timeout 為 80ms，失敗時直接回到 Unicode `SendInput`，避免 TSF 狀態不穩時卡住主出字流程。

### 內建相容規則

| 環境 | 判斷方式 | 出字模式 |
| --- | --- | --- |
| Chrome / Edge / Brave / Firefox / Opera 開 PTT | 視窗標題包含 `批踢踢實業坊`、`term.ptt.cc` 或 `ws.ptt.cc` | `Ctrl+V` 貼上 |
| Win11 Notepad | Windows build >= 22000 且 process 為 `notepad` / `notepad.exe` | `Ctrl+V` 貼上 |
| Notepad++ | process 為 `notepad++` / `notepad++.exe`，焦點控制項通常為 Scintilla | 逐字 Unicode `SendInput`，不使用剪貼簿；建議關閉 Notepad++ 自動完成 |
| PuTTY / PCMan / Pietty / Windows Terminal / mintty / RimWorld 等 | process 相容清單 | `Shift+Insert` 貼上 |
| Oxygen Not Included / PhotoImpact `iedit_` | process 相容清單 | `Ctrl+V` 貼上 |
| zip32w / DaqKing / EWinner | process 相容清單 | Big5 `Ctrl+V` 貼上 |

process 規則會自動忽略大小寫與 `.exe` 副檔名，`notepad` 與 `notepad.exe` 視為同一個程式。

Notepad++ 若啟用自動完成，彈出的候選視窗可能攔截 Scintilla 的按鍵與焦點流程，造成肥米字根漏進編輯區或出字失敗。實機測試關閉 Notepad++ 自動完成後，逐字 Unicode `SendInput` 可正常打字。

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
tsf_bridge_timeout_ms = 80
```

`send_kind_1_paste`、`send_kind_2_big5`、`send_kind_3_noucl` 可填入額外 process 名稱，用逗號分隔。
`play_sound_enable` 控制打字音，`keyboard_volume` 會限制在 0-100；右下角選單提供 10%-100% 快速切換。`show_phone_code=1` 時出字後會提示注音讀音。`startup_default_ucl=0` 時啟動預設為英模式，`enable_half_full=0` 時停用 `Shift+Space` 半全形切換。`tsf_bridge_timeout_ms` 控制 TSF Bridge pipe 等待時間，會限制在 10-1000ms，預設 80ms。

## 與 Python 版主要差異

Python 版 UCL_LIU 已演進到 v1.67，功能較完整；C# 版目前是追功能中的現代化版本。

C# 版已追上的重點：

- 無外部 NuGet 依賴，`System.Json`、`ini-parser` / `INIFileParser`、ILRepack 與 `packages.config` 已移除
- `liu-uni.tab -> liu.cin -> liu.json`
- `liu.cin -> liu.json`
- `custom.json` 自定詞庫
- `,,,BOX`
- 自定詞庫單例視窗
- 預設逐字 Unicode `SendInput` 出字
- 剪貼簿 paste retry / restore fallback
- Win11 Notepad、Chrome/Edge/Brave/PTT 標題規則與常見終端相容清單
- 打字音效、音量設定、特殊鍵音效
- 新版 `pinyi.txt` 同音字、`';` 注音查詢、出字後提示注音
- 啟動預設肥/英與 `Shift+Space` 半全形切換設定
- TSF Bridge 外掛式手動出字模式、狀態檢查、UAC 註冊/解除註冊導引
- 短版 UI 熱路徑降載、啟動 packed layout、chrome button focus 視覺修正
- foreground process 查詢短暫 cache
- 可用 .NET SDK 執行的核心測試

Python 版仍領先的重點：

- TSF Bridge 多環境實機調校與更完整的啟用提示
- 更多長尾 App 實機相容規則，例如 VBA、Neovim、特殊遊戲與遠端桌面情境
- OpenCC 簡繁轉換與更多特殊字修正
- 更多字碼表來源轉換
- 管理員權限導引與重啟流程細節
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
& 'C:\Program Files\Microsoft Visual Studio\18\Community\MSBuild\Current\Bin\MSBuild.exe' .\uclliu.sln /t:Rebuild /p:Configuration=Debug /p:Platform="Any CPU"
```

若開發機缺 `.NETFramework,Version=v4.5.2` reference assemblies，完整 build 會在環境檢查階段失敗；核心測試仍可用 .NET SDK 驗證純邏輯。

## 專案檔案

| 檔案 | 說明 |
| --- | --- |
| `uclliu.cs` | 核心輸入法邏輯、字根載入、命令處理 |
| `Form1.cs` | WinForms UI、tray menu、keyboard hook |
| `TextOutput.cs` | Unicode SendInput、剪貼簿貼上、出字策略選擇 |
| `TsfBridge.cs` | TSF Bridge assets 定位、註冊狀態、named pipe 出字 fallback |
| `TypingSound.cs` | 打字音效、音量縮放、wav 分類、防長按連發 |
| `PhoneCodeTable.cs` | 新版 `pinyi.txt` 注音查詢與反向讀音表 |
| `LiuTableConverter.cs` | `liu-uni.tab` / `liu.cin` / `liu.json` 轉換 |
| `CustomDictionaryStore.cs` | `custom.json` 載入、儲存、合併 |
| `CustomDictionaryForm.cs` | 自定詞庫編輯器 |
| `SimpleIni.cs` | 內建 INI 讀寫，取代外部 ini-parser |
| `UclLiuAppInfo.cs` | 版本、作者、exe 詳細資料欄位 |
| `UiLayoutCalculator.cs` | 可測試的 UI 寬度計算 |
| `tools/UclLiuCoreTests` | 核心測試 harness |
| `wavs` | Python 版後期打字音效 wav 素材 |
| `tsf_bridge` | Python 版 TSF Bridge DLL 與註冊/解除註冊腳本 |
| `CHANGELOG.md` | C# 版近期變更 |
| `GOALS.md` | 追 Python 版功能清單 |
| `history.md` | 開發對話與驗證紀錄 |

## 版權與作者

- 作者：羽山秋人 ([3wa.tw](https://3wa.tw))
- 信箱：uclliu.3wa@gmail.com
- 授權：MIT License
