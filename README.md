# UCL_LIU_CSharp

C# / WinForms 版肥米輸入法，目前版本 v0.14。這個分支的目標是保留 2019 年 C# 版輕量、低依賴、好攜帶的優點，同時逐步追上 Python 版 UCL_LIU 後來累積的穩定性與工具鏈。

目前 C# 版已進入可日用測試的現代化復刻狀態：已補上多來源字碼表自動轉換、自定詞庫、Unicode SendInput 出字、剪貼簿 fallback、打字音效、同音/注音查詢、Win11/Chrome/PTT/Notepad 相容規則、TSF Bridge 外掛 fallback、管理員重啟導引與核心測試。它仍不是 Python 版 v1.67 的完整替代品；OpenCC/特殊字修正、更多長尾 App 相容規則與 TSF 實機調校仍在後續項目。

![UCL_LIU C# screenshot](screenshot/ucl_1.png)

## 成熟度評估

| 版本 | 實用成熟度 | 工程可維護性 | 日用信心 | 定位 |
| --- | ---: | ---: | ---: | --- |
| Python 版 v1.67 | 8.5 / 10 | 6.5 / 10 | 8.5 / 10 | 功能最完整的正史版，多年實戰相容性最足 |
| C# 版 v0.14 | 8.2 / 10 | 8.2 / 10 | 8.1 / 10 | 同音/注音與 TSF 管理員導引更完整、低依賴、可維護的現代化復刻版 |

## 目前狀態

| 項目 | C# 版目前狀態 | Python 版對照 |
| --- | --- | --- |
| 核心輸入 | WinForms + low-level keyboard hook | PyHook / PyWin32 |
| 預設出字 | Python-style 逐字 Unicode `SendInput`，失敗 fallback 舊 `SendKeys` | TSF 可選，失敗 fallback `SendKeysCtypes` / paste |
| 貼上模式 | 已封裝 retry、timeout、try/finally 還原剪貼簿 | 已有大量 App 特例 |
| 字碼表 | 支援 `liu.json`、`liu.cin`、`liu-uni.tab`、RIME/fcitx/小小輸入法等來源自動轉換 | 支援來源較多，另含部分需手動整理的長尾碼表 |
| 自定詞庫 | 支援 `custom.json`、右下角選單、`,,,BOX`、單例視窗 | v1.63-v1.65 已完整支援 |
| UI 效能 | 已降低短版模式與 foreground process 查詢熱路徑負擔；短版啟動即套用 packed layout，`肥` / `半` / `╳` chrome 尺寸與長版一致，並移除按鈕 focus 後的按下視覺殘留 | Python 版後期另有多輪 Win11/位置修正 |
| TSF Bridge | 已移植為手動 `TSF出字模式`，透過 named pipe 呼叫外掛 DLL，失敗 fallback Unicode `SendInput`；右下角選單可檢查/註冊/解除註冊，非管理員時可導引重新以系統管理員啟動 | v1.67 實驗性支援 |
| 打字音效 | 支援自備 `wavs\*.wav`、開關、10%-100% 音量、特殊鍵音效、防長按連發；官方發行檔不內含授權不明音效素材 | 已支援音效與音量設定 |
| 同音/注音 | 支援新版 `pinyi.txt` 同音字、`';` 注音查詢、注音候選空白出字、`Shift+Space` 換頁與出字後提示注音 | 已支援 |
| 啟動與半全形 UX | 支援啟動預設肥/英、允許停用 `Shift+Space` 半全形切換 | 已支援 |
| 測試 | `tools/UclLiuCoreTests` 可用 .NET SDK 跑核心測試 | Python 版以實機與歷史回報為主 |

## 低依賴狀態

C# 版 v0.14 目前已整理成「無外部 NuGet、無 DLL 合併」的乾淨 WinForms 專案。主程式只使用 .NET Framework 4.5.2 內建 reference，功能邏輯盡量收在專案原始碼裡，方便直接用 Visual Studio 或 MSBuild 重建。

TSF Bridge 是例外：它是可選外掛，不合併進主程式、不列入預設出字流程；只有手動切到 `TSF出字模式` 時才會透過 named pipe 呼叫 `tsf_bridge\UclTsfBridge.dll`。

| 類別 | 目前狀態 |
| --- | --- |
| NuGet 套件 | 無 |
| 外部 DLL | 主程式無；選配 TSF Bridge 會隨附 `tsf_bridge\UclTsfBridge.dll` |
| JSON | 使用 .NET 內建 `System.Web.Script.Serialization.JavaScriptSerializer`，不再依賴 `System.Json` |
| INI | 使用專案內 `SimpleIni.cs`，不再依賴 `ini-parser` / `INIFileParser` |
| 打包合併 | 不再使用 ILRepack，不需要把套件 DLL 合併進 exe |
| 開發輸出 | 直接使用 `bin\Debug` / `bin\Release`，不再維護 Python 版式的 `dist` 目錄 |
| 可選資料檔 | `liu.json` / `liu.cin` / `liu-uni.tab`、`wuxiami.txt`、`liur_trad.dict.yaml`、`terry_boshiamy.txt`、`fcitx_boshiamy.txt`、`uniliu.txt`、`pinyi.txt`、自備 `wavs\*.wav`、`tsf_bridge\*` |

目前 `uclliu.csproj` 的 reference 只保留：

- `System`
- `System.Core`
- `System.Data`
- `System.Drawing`
- `System.Web.Extensions`
- `System.Windows.Forms`

## 快速開始

1. 下載或編譯 `uclliu.exe`。
2. 將字碼表放在 `uclliu.exe` 同一個目錄，擇一即可：
   - `liu.json`
   - `liu.cin`
   - `liu-uni.tab`
   - `wuxiami.txt`
   - `liur_trad.dict.yaml` / `liur_Trad.dict.yaml`
   - `terry_boshiamy.txt`
   - `fcitx_boshiamy.txt`
   - `uniliu.txt`
3. 可選：放入 `pinyi.txt` 啟用同音字與注音查詢。
4. 可選：放入自有或合法授權的 `wavs\*.wav` 啟用打字音效。
5. 可選：保留 `tsf_bridge` 目錄，右下角選單可註冊 TSF Bridge 並手動切到 `TSF出字模式`。
6. 執行 `uclliu.exe`。

因字碼表版權問題，本專案不提供上述字碼表。若使用 `liu.cin`、`liu-uni.tab` 或外部文字碼表，C# 版啟動時會自動產生 `liu.json`。

發行檔位置：

| 檔案 | 說明 |
| --- | --- |
| [uclliu-v0.14.zip](https://github.com/shadowjohn/UCL_LIU_CSharp/releases/download/v0.14/uclliu-v0.14.zip) | v0.14 推薦下載包，含 `uclliu.exe`、`pinyi.txt`、`tsf_bridge`、README 與 LICENSE |
| [uclliu.exe](https://github.com/shadowjohn/UCL_LIU_CSharp/releases/download/v0.14/uclliu.exe) | v0.14 單檔版，不含 TSF Bridge、同音/注音資料與音效素材 |
| [RELEASE/0.12/uclliu.zip](RELEASE/0.12/uclliu.zip) | v0.12 推薦下載包，含 `uclliu.exe`、`tsf_bridge` 外掛、註冊/解除註冊腳本 |
| [RELEASE/0.12/uclliu.exe](RELEASE/0.12/uclliu.exe) | v0.12 單檔版，不含 TSF Bridge；只適合不需要 TSF 的使用者 |
| [RELEASE/0.11/uclliu.exe](RELEASE/0.11/uclliu.exe) | v0.11 開發分支打包版 |
| [RELEASE/0.11/uclliu.zip](RELEASE/0.11/uclliu.zip) | v0.11 壓縮版 |
| [RELEASE/0.1/uclliu.exe](RELEASE/0.1/uclliu.exe) | 2019 初版備份 |

v0.14 發行日期：2026-05-28，發行檔由 GitHub Actions 自動測試、編譯與打包。v0.12 病毒碼掃描已提交 Microsoft WDSI：[submission a42546c1-4432-40f2-8cc6-6e226617cf19](https://www.microsoft.com/en-us/wdsi/submission/a42546c1-4432-40f2-8cc6-6e226617cf19)。

v0.11 病毒碼掃描紀錄：[submission 2a365b04-dea0-496f-937f-9051b163a968](https://www.microsoft.com/en-us/wdsi/submission/2a365b04-dea0-496f-937f-9051b163a968)。

v0.14 推薦下載包 `uclliu-v0.14.zip` 內含：

| 檔案 / 目錄 | 用途 |
| --- | --- |
| `uclliu.exe` | 主程式 |
| `pinyi.txt` | 同音字、`';` 注音查詢與出字後注音提示資料 |
| `tsf_bridge\*` | TSF Bridge DLL、註冊/解除註冊與解除封鎖腳本 |
| `README.md` / `LICENSE` | 使用說明與授權 |

官方發行檔不內含 wav 音效素材。若要使用打字音，請在 `uclliu.exe` 同一層建立 `wavs` 目錄，放入自有、CC0 或其他具備可再散布授權的 `.wav` 檔。

若自行編譯，請使用 .NET Framework 4.5.2 WinForms 專案重建。C# 版開發輸出以 `bin\Debug\uclliu.exe` 或 `bin\Release\uclliu.exe` 為準，不另外維護 `dist` 目錄；請確認 `pinyi.txt`、自備 `wavs`、`tsf_bridge`、`liu.json` 或可轉換字碼表與 exe 放在同一目錄。

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
| `';a` + 空白 | 注音查詢「ㄇ」，例如候選會含「ㄇ」 |

## 同音字與注音查詢

`pinyi.txt` 放在 `uclliu.exe` 同一目錄後會啟用同音字與注音功能。`'ucl` 可用最後一個已出字的字查同音候選；`';zo6` 可用注音鍵盤碼查 `ㄈㄟˊ`，候選會包含「肥」；`';a` 後按空白可查 `ㄇ` 這類單獨注音符號候選。注音候選顯示後，可按數字鍵選字，按空白送出第一候選；候選有下一頁時，`Shift+Space` 會先換頁。右下角選單開啟「顯示提示注音」後，出字後會顯示 `音:ㄈㄟˊ` 類提示。

v0.14 推薦下載包已內含 `pinyi.txt`；若只下載單檔 `uclliu.exe`，需要自行補上 `pinyi.txt` 才會有同音字與注音查詢。

## 打字音效

C# 版會掃描 `uclliu.exe` 同一層的 `wavs\*.wav`。`enter.wav`、`delete.wav`、`backspace.wav`、`space.wav` 會對應特殊鍵，其餘 wav 會作為一般按鍵音效隨機播放；右下角選單可開關打字音並調整 10%-100% 音量。

因舊 ICQ 等第三方音效授權未確認，官方 release 不隨附 wav 檔。要公開散布自訂封包時，請只放入自製、CC0 或其他明確允許再散布的音效。

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

TSF Bridge 不會自動啟用，也不會自動註冊。要使用 TSF，請下載 `uclliu-v0.14.zip` 並完整解壓縮，讓 `tsf_bridge` 目錄與 `uclliu.exe` 放在同一層。只下載單檔 `uclliu.exe` 時不會包含 TSF Bridge。

右下角選單提供「TSF Bridge 管理」：

- 檢查 TSF Bridge 狀態：顯示 DLL、註冊狀態、目前權限與註冊位置。
- 註冊 TSF Bridge：透過 UAC 執行 `tsf_bridge\register_tsf_bridge.bat`。
- 解除註冊 TSF Bridge：透過 UAC 執行 `tsf_bridge\unregister_tsf_bridge.bat`。
- 解除 DLL 封鎖：執行 `tsf_bridge\unlock_tsf_bridge.ps1`。
- 開啟 Windows 輸入法設定：協助加入或切換 `UCLLIU TSF Bridge`。
- 若目前不是系統管理員，右下角主選單會顯示「★以系統管理員身分重新啟動肥米」。

建議啟用流程：

1. 下載並解壓縮 [uclliu-v0.14.zip](https://github.com/shadowjohn/UCL_LIU_CSharp/releases/download/v0.14/uclliu-v0.14.zip)。
2. 執行 `uclliu.exe`。
3. 若右下角肥米選單出現「★以系統管理員身分重新啟動肥米」，先點選並允許 UAC。
4. 從右下角肥米選單進入 `4.TSF Bridge 管理`。
5. 若檔案是從網路下載，先點 `解除 DLL 封鎖`。
6. 點 `註冊 TSF Bridge`，允許 UAC 後等待註冊完成。
7. 點 `開啟 Windows 輸入法設定`，將 `UCLLIU TSF Bridge` 加入繁體中文鍵盤。
8. 用工作列語言選單或 `Win+Space` 切到 `繁體中文（台灣） / UCLLIU TSF Bridge`。
9. 回肥米右下角選單，選 `TSF出字模式`。若 DLL、註冊或權限尚未就緒，肥米會提示下一步。

看到以下狀態代表設定完成：

- Windows 11 鍵盤配置清單會出現 `繁體中文（台灣）`，下方顯示 `UCLLIU TSF Bridge`；選取後工作列語言圖示會維持在繁體中文輸入環境。
- 肥米右下角 `4.TSF Bridge 管理` 子選單第一列會顯示 `TSF Bridge 已註冊`。

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
- Python 版常見外部字碼表來源轉換：`wuxiami.txt`、`liur_trad.dict.yaml` / `liur_Trad.dict.yaml`、`terry_boshiamy.txt`、`fcitx_boshiamy.txt`、`uniliu.txt`
- `liu.json` 字根 key 載入時統一轉小寫，可吃到大寫 root 的特殊碼表
- `custom.json` 自定詞庫
- `,,,BOX`
- 自定詞庫單例視窗
- 預設逐字 Unicode `SendInput` 出字
- 剪貼簿 paste retry / restore fallback
- Win11 Notepad、Chrome/Edge/Brave/PTT 標題規則與常見終端相容清單
- 打字音效、音量設定、特殊鍵音效
- 新版 `pinyi.txt` 同音字、`';` 注音查詢、注音候選空白出字、`Shift+Space` 換頁、出字後提示注音
- 啟動預設肥/英與 `Shift+Space` 半全形切換設定
- TSF Bridge 外掛式手動出字模式、狀態檢查、UAC 註冊/解除註冊與管理員重啟導引
- 短版 UI 熱路徑降載、啟動 packed layout、chrome button focus 視覺修正
- foreground process 查詢短暫 cache
- 可用 .NET SDK 執行的核心測試

Python 版仍領先的重點：

- TSF Bridge 多環境實機調校
- 更多長尾 App 實機相容規則，例如 VBA、Neovim、特殊遊戲與遠端桌面情境
- OpenCC 簡繁轉換與更多特殊字修正
- 韓文字根等原本需人工整理的長尾碼表匯入流程
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
& 'C:\Program Files\Microsoft Visual Studio\18\Community\MSBuild\Current\Bin\MSBuild.exe' .\uclliu.csproj /t:Rebuild /p:Configuration=Debug /p:Platform=AnyCPU
```

若開發機缺 `.NETFramework,Version=v4.5.2` reference assemblies，完整 build 會在環境檢查階段失敗；核心測試仍可用 .NET SDK 驗證純邏輯。

發行打包：

```powershell
& .\tools\package-release.ps1 -Version v0.14 -Configuration Release -OutputDirectory .\artifacts
```

官方打包預設不包含 `wavs`。若是私用或已確認音效可再散布，可加上 `-IncludeWavs` 封入本機 `wavs` 目錄。

GitHub Actions 會在 `master` push / PR 時自動跑核心測試與 Release build，並上傳 artifact。推送 `v*` tag 時會自動建立或更新 GitHub Release：

```powershell
git tag v0.14
git push origin v0.14
```

自動 Release 會上傳兩個檔案：`uclliu-v0.14.zip` 是推薦下載包，含 TSF Bridge 與同音/注音資料；`uclliu.exe` 是單檔版，不含 TSF Bridge。

## 專案檔案

| 檔案 | 說明 |
| --- | --- |
| `uclliu.cs` | 核心輸入法邏輯、字根載入、命令處理 |
| `Form1.cs` | WinForms UI、tray menu、keyboard hook |
| `TextOutput.cs` | Unicode SendInput、剪貼簿貼上、出字策略選擇 |
| `TsfBridge.cs` | TSF Bridge assets 定位、註冊狀態、啟用決策與 named pipe 出字 fallback |
| `ElevatedRestart.cs` | 重新以系統管理員身分啟動肥米的參數組裝 |
| `TypingSound.cs` | 打字音效、音量縮放、wav 分類、防長按連發 |
| `PhoneCodeTable.cs` | 新版 `pinyi.txt` 注音查詢與反向讀音表 |
| `LiuTableConverter.cs` | `liu-uni.tab` / `liu.cin` / `liu.json` 與外部字碼表轉換 |
| `CustomDictionaryStore.cs` | `custom.json` 載入、儲存、合併 |
| `CustomDictionaryForm.cs` | 自定詞庫編輯器 |
| `SimpleIni.cs` | 內建 INI 讀寫，取代外部 ini-parser |
| `UclLiuAppInfo.cs` | 版本、作者、exe 詳細資料欄位 |
| `UiLayoutCalculator.cs` | 可測試的 UI 寬度計算 |
| `tools/UclLiuCoreTests` | 核心測試 harness |
| `tools/package-release.ps1` | 本機與 GitHub Actions 共用的發行打包腳本 |
| `.github/workflows/build-and-release.yml` | GitHub Actions 測試、編譯、打包與 tag Release |
| `wavs` | 選配本機打字音效目錄；官方 repo / release 不隨附 wav，請自行放入合法授權音效 |
| `tsf_bridge` | Python 版 TSF Bridge DLL 與註冊/解除註冊腳本 |
| `CHANGELOG.md` | C# 版近期變更 |
| `GOALS.md` | 追 Python 版功能清單 |
| `history.md` | 開發對話與驗證紀錄 |

## 版權與作者

- 作者：羽山秋人 ([3wa.tw](https://3wa.tw))
- 信箱：uclliu.3wa@gmail.com
- 授權：MIT License
