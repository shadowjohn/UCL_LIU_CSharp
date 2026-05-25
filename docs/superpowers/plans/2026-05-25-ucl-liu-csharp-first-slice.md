# UCL_LIU_CSharp First Slice Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Improve C# version stability and add the first table conversion path from `liu-uni.tab` or `liu.cin` to `liu.json`.

**Architecture:** Keep the WinForms app shape intact. Add pure C# core helpers for table conversion and UI sizing so they can be tested outside the legacy .NET Framework app, then call those helpers from the existing startup and layout paths.

**Tech Stack:** C# / WinForms / .NET Framework 4.5.2 for the app, .NET SDK console harness for core tests.

---

### Task 1: Core Test Harness

**Files:**
- Create: `tools/UclLiuCoreTests/UclLiuCoreTests.csproj`
- Create: `tools/UclLiuCoreTests/Program.cs`

- [x] **Step 1: Write failing tests**

Add console tests that call `LiuTableConverter` and `UiLayoutCalculator` before those classes exist.

- [x] **Step 2: Verify red**

Run: `dotnet run --project tools/UclLiuCoreTests/UclLiuCoreTests.csproj`

Expected: build fails because `LiuTableConverter.cs` and `UiLayoutCalculator.cs` are not implemented yet.

### Task 2: Table Conversion Core

**Files:**
- Create: `LiuTableConverter.cs`
- Modify: `uclliu.csproj`
- Modify: `uclliu.cs`

- [x] **Step 1: Implement CIN to JSON conversion**

Parse `%chardef begin` / `%chardef end`, group each root key into `chardefs`, and write deterministic UTF-8 JSON.

- [x] **Step 2: Implement `liu-uni.tab` to CIN conversion**

Port the Python `liu_unitab2cin.py` binary decoding logic into a pure C# helper.

- [x] **Step 3: Wire startup auto-conversion**

Before showing missing `liu.json`, try current folder `liu.cin`, current folder `liu-uni.tab`, and known Boshiamy install paths.

- [x] **Step 4: Verify green**

Run: `dotnet run --project tools/UclLiuCoreTests/UclLiuCoreTests.csproj`

Expected: all converter tests pass.

### Task 3: UI Hot Path Load Reduction

**Files:**
- Create: `UiLayoutCalculator.cs`
- Modify: `uclliu.csproj`
- Modify: `uclliu.cs`

- [x] **Step 1: Move short-mode width math into a pure helper**

Use fixed bounds and minimum zero width so empty labels do not cause repeated layout churn.

- [x] **Step 2: Avoid full UI rebuild on every candidate update**

Replace per-keystroke `update_UI()` in short mode with a narrow column-width update.

- [x] **Step 3: Cache foreground process info briefly**

Avoid hitting `Process.MainModule` for every low-level key event.

- [x] **Step 4: Verify**

Run core tests and attempt legacy build. If legacy build is blocked by missing .NET Framework 4.5.2 Developer Pack, record the blocker.

### Task 4: Commit

**Files:**
- Modify: all first-slice files

- [x] **Step 1: Review diff**

Run: `git diff --check` and `git diff --stat`.

- [ ] **Step 2: Commit**

Run:

```powershell
git add GOALS.md docs/superpowers/plans/2026-05-25-ucl-liu-csharp-first-slice.md tools/UclLiuCoreTests LiuTableConverter.cs UiLayoutCalculator.cs uclliu.cs uclliu.csproj
git commit -m "feat: add first csharp parity slice"
```
