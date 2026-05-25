# UCL_LIU_CSharp Custom Dictionary Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [x]`) syntax for tracking.

**Goal:** Add C# version custom dictionary support aligned with Python UCL_LIU v1.63~v1.65.

**Architecture:** Keep dictionary persistence in a pure `CustomDictionaryStore` class with tests, merge custom entries into `uclcode` after base `liu.json` loads, and add a small WinForms editor opened from tray menu or `,,,BOX`.

**Tech Stack:** C# / WinForms / JavaScriptSerializer / .NET SDK console harness for core tests.

---

### Task 1: Core Custom Dictionary Store

**Files:**
- Create: `CustomDictionaryStore.cs`
- Modify: `tools/UclLiuCoreTests/Program.cs`
- Modify: `tools/UclLiuCoreTests/UclLiuCoreTests.csproj`

- [x] Write red tests for root validation, custom merge order, and deterministic save/load.
- [x] Implement `NormalizeRootKey`, `IsValidRootKey`, `Load`, `Save`, and `MergeInto`.
- [x] Verify with `dotnet run --project tools\UclLiuCoreTests\UclLiuCoreTests.csproj`.

### Task 2: App Integration

**Files:**
- Modify: `uclliu.cs`
- Modify: `uclliu.csproj`

- [x] Add `CUSTOM_JSON_FILE` path beside `uclliu.exe`.
- [x] Merge `custom.json` after base `liu.json` parse.
- [x] Add `reload_word_root()` and clear reverse lookup before rebuilding.

### Task 3: WinForms Editor And Single Instance

**Files:**
- Create: `CustomDictionaryForm.cs`
- Modify: `Form1.cs`
- Modify: `uclliu.cs`
- Modify: `uclliu.csproj`

- [x] Add custom dictionary editor for add/update/delete/reorder.
- [x] Add tray menu item `自定詞庫`.
- [x] Add `,,,BOX` command.
- [x] Keep only one custom dictionary window open; focus existing window if already opened.

### Task 4: Verification And Commit

- [x] Run core tests.
- [x] Compile-check new WinForms editor in a temporary `net10.0-windows` project.
- [x] Record legacy build blocker: missing .NET Framework 4.5.2 Developer Pack.
- [x] Commit second slice.
