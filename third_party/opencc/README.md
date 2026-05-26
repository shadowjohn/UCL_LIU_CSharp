# OpenCC Lite for UCL_LIU_CSharp

本目錄保存 C# 版肥米輸入法使用的 OpenCC `s2t` 最小資料集。

## 使用方式

- `OpenCcLite` 只支援 `s2t`：Simplified Chinese to Traditional Chinese。
- 目前用在 `,,,z`「反白文字轉嘸蝦米字根」前的簡轉繁流程。
- 字典檔以 Embedded Resource 包進 `uclliu.exe`，執行時不需要 NuGet，也不需要額外 DLL。
- `LICENSE.txt`、`NOTICE.txt` 會複製到 build output；若重新打包 zip，請一起放入發行檔。

## 來源

- `config/s2t.json`
- `dictionary/STPhrases.txt`
- `dictionary/STCharacters.txt`

以上資料取自 OpenCC / opencc-python-reimplemented 使用的 OpenCC 詞庫。

## 授權

OpenCC 詞庫與相關資料依 Apache License 2.0 使用。保留本目錄的 `LICENSE.txt` 與 `NOTICE.txt`。

UCL_LIU_CSharp 自身仍依專案根目錄 `LICENSE` 授權。
