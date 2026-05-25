# C# 版打字音效與 UX 設定

## 目標

- 對齊 Python 版後期打字音、音量、特殊鍵音效。
- 補上托盤選單中的打字音、啟動預設肥模式、允許半全形切換設定。
- 避免 keyboard hook 因音效 I/O 或同步播放造成卡頓。

## 實作項目

1. 新增可測核心邏輯：
   - 音量限制 0-100。
   - wav 檔名分類：`enter` / `return`、`delete` / `del`、`backspace` / `bs`、`space` / `sp`。
   - 同鍵長按不重複播放。
   - PCM 16-bit wav 音量縮放。
2. 新增播放器：
   - 掃描 exe 同目錄與 `wavs` 子目錄。
   - 背景 ThreadPool 播放，最多 3 個播放工作。
   - 音效失敗不得影響輸入法主流程。
3. 接入 UI 與設定：
   - `PLAY_SOUND_ENABLE` 控制 hook 是否播放音效。
   - `KEYBOARD_VOLUME` 由托盤選單切換並播放預覽。
   - `STARTUP_DEFAULT_UCL=0` 時啟動為英模式。
   - `ENABLE_HALF_FULL=0` 時停用 `Shift+Space` 半全形切換。

## 驗證

- 核心測試採 TDD：先確認缺少 TypingSound 類別而紅燈，再補實作。
- 跑 `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj`。
- 用本機可用的 .NET Framework reference assemblies 臨時覆寫到 `v4.8` 編譯，確認 WinForms 整合。
