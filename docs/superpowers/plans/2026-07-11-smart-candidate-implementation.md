# Smart Candidate Learning Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add local next-word/phrase prediction and adaptive root-candidate ranking without new DLLs or NuGet packages.

**Architecture:** Load a tab-separated `candidate.txt` into immutable in-memory lookup tables, merge it with a small local `candidate_memory.json`, and expose all runtime behavior through one `SmartCandidateSession`. Existing `uclliu` output and `Form1` keyboard-hook paths only notify or query that session; disk I/O remains outside the hook and uses an idle WinForms timer.

**Tech Stack:** C# 7.3, .NET Framework 4.5.2, WinForms, `System.Web.Script.Serialization.JavaScriptSerializer`, existing console test harness, PowerShell release tooling.

---

## File map

- Create `SmartCandidateTable.cs`: parse UTF-8 TSV and provide stable prefix candidates.
- Create `SmartCandidateMemory.cs`: learn phrases/root choices, rank candidates, and atomically persist JSON.
- Create `SmartCandidateSession.cs`: current context, visible page, selection, cancellation, and dirty/idle state.
- Create `tools/convert-chewing-candidates.ps1`: convert a licensed `tsi.csv` into deterministic `candidate.txt`.
- Modify `uclliu.cs`: initialize the feature, observe output, expose menu actions, and refresh existing candidate labels.
- Modify `Form1.cs`: keyboard priority, idle timer, menu items, GitHub link, and close-time flush.
- Modify `UiLayoutCalculator.cs`: bounded long-mode candidate width calculation.
- Modify `TrayMenuText.cs`: stable candidate-menu labels.
- Modify both project files so new pure helpers are built and tested.
- Modify `tools/package-release.ps1`: include `candidate.txt` only in the full zip.
- Modify `tools/UclLiuCoreTests/Program.cs`: one runnable check per non-trivial rule.
- Modify `README.md`, `CHANGELOG.md`, `todo.md`, and `history.md` after implementation.

### Task 1: Candidate table parser and stable lookup

**Files:**
- Create: `SmartCandidateTable.cs`
- Modify: `uclliu.csproj`
- Modify: `tools/UclLiuCoreTests/UclLiuCoreTests.csproj`
- Modify: `tools/UclLiuCoreTests/Program.cs`

- [ ] **Step 1: Register failing parser tests**

Add these calls beside the existing candidate tests:

```csharp
failed += Run("smart candidate table parses tsv and skips invalid rows", TestSmartCandidateTableParsesTsv);
failed += Run("smart candidate table keeps stable unique order", TestSmartCandidateTableKeepsStableOrder);
```

Add the tests:

```csharp
private static void TestSmartCandidateTableParsesTsv()
{
    SmartCandidateTable table = SmartCandidateTable.Parse(new[] {
        "王\t小明\t先生\t小姐",
        "bad",
        "你\t好\t們"
    });
    AssertSequence(new[] { "小明", "先生", "小姐" }, table.Find("王"));
    AssertEqual(1, table.InvalidLineCount);
}

private static void TestSmartCandidateTableKeepsStableOrder()
{
    SmartCandidateTable table = SmartCandidateTable.Parse(new[] {
        "王\t小明\t先生\t小明",
        "王\t小姐\t先生"
    });
    AssertSequence(new[] { "小明", "先生", "小姐" }, table.Find("王"));
}
```

Add this test helper near the other assertions:

```csharp
private static void AssertSequence(IEnumerable<string> expected, IEnumerable<string> actual)
{
    AssertEqual(String.Join("|", expected), String.Join("|", actual));
}
```

- [ ] **Step 2: Run tests and verify red**

Run:

```powershell
dotnet run --project .\tools\UclLiuCoreTests\UclLiuCoreTests.csproj
```

Expected: compile failure because `SmartCandidateTable` does not exist.

- [ ] **Step 3: Implement the minimal table**

Create `SmartCandidateTable.cs`:

```csharp
using System;
using System.Collections.Generic;
using System.IO;

namespace uclliu
{
    public sealed class SmartCandidateTable
    {
        private readonly Dictionary<string, List<string>> rows;
        public int InvalidLineCount { get; private set; }
        public bool IsAvailable { get { return rows.Count > 0; } }

        private SmartCandidateTable(Dictionary<string, List<string>> rows, int invalidLineCount)
        {
            this.rows = rows;
            InvalidLineCount = invalidLineCount;
        }

        public static SmartCandidateTable Empty()
        {
            return new SmartCandidateTable(new Dictionary<string, List<string>>(StringComparer.Ordinal), 0);
        }

        public static SmartCandidateTable Load(string path)
        {
            return File.Exists(path) ? Parse(File.ReadLines(path)) : Empty();
        }

        public static SmartCandidateTable Parse(IEnumerable<string> lines)
        {
            Dictionary<string, List<string>> result = new Dictionary<string, List<string>>(StringComparer.Ordinal);
            int invalid = 0;
            foreach (string raw in lines)
            {
                if (String.IsNullOrWhiteSpace(raw) || raw.StartsWith("#", StringComparison.Ordinal)) continue;
                string[] parts = raw.Split('\t');
                if (parts.Length < 2 || String.IsNullOrWhiteSpace(parts[0])) { invalid++; continue; }
                string key = parts[0].Trim();
                List<string> values;
                if (!result.TryGetValue(key, out values)) { values = new List<string>(); result[key] = values; }
                for (int i = 1; i < parts.Length; i++)
                {
                    string value = parts[i].Trim();
                    if (value.Length > 0 && !values.Contains(value)) values.Add(value);
                }
                if (values.Count == 0) invalid++;
            }
            return new SmartCandidateTable(result, invalid);
        }

        public IList<string> Find(string context)
        {
            List<string> values;
            return context != null && rows.TryGetValue(context, out values)
                ? new List<string>(values)
                : new List<string>();
        }
    }
}
```

Add `SmartCandidateTable.cs` as `<Compile Include>` to both project files.

- [ ] **Step 4: Verify green and commit**

Run the core tests and `git diff --check`; expect all tests to pass.

```powershell
git add SmartCandidateTable.cs uclliu.csproj tools\UclLiuCoreTests\UclLiuCoreTests.csproj tools\UclLiuCoreTests\Program.cs
git commit -s -m "feat: load smart candidate table"
```

### Task 2: Personal learning, ranking, and atomic JSON storage

**Files:**
- Create: `SmartCandidateMemory.cs`
- Modify: `uclliu.csproj`
- Modify: `tools/UclLiuCoreTests/UclLiuCoreTests.csproj`
- Modify: `tools/UclLiuCoreTests/Program.cs`

- [ ] **Step 1: Write failing learning and persistence tests**

Register and add:

```csharp
failed += Run("smart memory learns 王 to 小明 from normal output", TestSmartMemoryLearnsPhrase);
failed += Run("explicit candidate choice ranks above observed output", TestSmartMemoryExplicitChoiceWins);
failed += Run("smart memory saves and reloads json", TestSmartMemoryRoundTrips);

private static void TestSmartMemoryLearnsPhrase()
{
    SmartCandidateMemory memory = new SmartCandidateMemory();
    memory.ObserveSequence("王小明");
    AssertEqual("小明", memory.GetPredictions("王")[0]);
}

private static void TestSmartMemoryExplicitChoiceWins()
{
    SmartCandidateMemory memory = new SmartCandidateMemory();
    memory.ObserveSequence("王先生");
    memory.RecordPredictionChoice("王", "小明");
    AssertEqual("小明", memory.GetPredictions("王")[0]);
}

private static void TestSmartMemoryRoundTrips()
{
    string dir = CreateTempDirectory();
    try {
        string path = Path.Combine(dir, "candidate_memory.json");
        SmartCandidateMemory memory = new SmartCandidateMemory();
        memory.ObserveSequence("王小明");
        memory.RecordRootChoice("abc", "王");
        SmartCandidateMemoryStore.SaveAtomic(path, memory);
        SmartCandidateMemory loaded = SmartCandidateMemoryStore.Load(path);
        AssertEqual("小明", loaded.GetPredictions("王")[0]);
        AssertEqual("王", loaded.RankRootCandidates("abc", new[] { "汪", "王" })[0]);
    } finally { Directory.Delete(dir, true); }
}
```

- [ ] **Step 2: Run tests and verify red**

Expected: compile failure for `SmartCandidateMemory`.

- [ ] **Step 3: Implement weighted learning and storage**

Create `SmartCandidateMemory.cs` with serializable dictionaries. Use score `+1` for observed sequences and `+5` for explicit prediction/root choices. `ObserveSequence` must generate suffixes from a maximum three-character context:

```csharp
for (int start = 0; start < text.Length - 1; start++)
{
    int maxContext = Math.Min(3, text.Length - start - 1);
    for (int contextLength = 1; contextLength <= maxContext; contextLength++)
    {
        string context = text.Substring(start, contextLength);
        string continuation = text.Substring(start + contextLength);
        Increment(Predictions, context, continuation, 1);
    }
}
```

Define these public methods exactly:

```csharp
public void ObserveSequence(string text);
public void RecordPredictionChoice(string context, string candidate);
public void RecordRootChoice(string root, string candidate);
public IList<string> GetPredictions(string context);
public IList<string> RankRootCandidates(string root, IEnumerable<string> original);
public bool IsDirty { get; }
public void MarkSaved();
```

Sort by descending score, then by first-seen sequence number. Merge root candidates without adding words absent from the original list.

Implement `SmartCandidateMemoryStore` in the same file using the existing `JavaScriptSerializer`. Save to `path + ".tmp"`, then use `File.Replace` when the target exists or `File.Move` otherwise. On deserialize failure, move the bad file to `path + ".broken"` (replace an older `.broken`) and return a new memory object.

Add the new file to both project files.

- [ ] **Step 4: Run tests and commit**

```powershell
dotnet run --project .\tools\UclLiuCoreTests\UclLiuCoreTests.csproj
git diff --check
git add SmartCandidateMemory.cs uclliu.csproj tools\UclLiuCoreTests\UclLiuCoreTests.csproj tools\UclLiuCoreTests\Program.cs
git commit -s -m "feat: learn local candidate preferences"
```

### Task 3: Candidate session, paging, cancellation, and boundaries

**Files:**
- Create: `SmartCandidateSession.cs`
- Modify: `KeyboardHookState.cs`
- Modify: both project files
- Modify: `tools/UclLiuCoreTests/Program.cs`

- [ ] **Step 1: Write failing state tests**

Cover: 1-based five-item pages, selection, next page, comma retention, sentence reset, mode reset, and Shift+Space priority.

```csharp
SmartCandidateTable table = SmartCandidateTable.Parse(new[] {
    "王\t小明\t先生\t小姐\t同學\t老師\t主任"
});
SmartCandidateMemory memory = new SmartCandidateMemory();
SmartCandidateSession session = new SmartCandidateSession(table, memory, 5);
session.ObserveCommittedText("王");
AssertEqual("小明", session.VisibleCandidates[0]);
AssertEqual("小明", session.Select(1));
AssertTrue(SmartCandidateKeyRules.ShouldPageOnShiftSpace(true, true), "prediction page wins");
AssertTrue(!SmartCandidateKeyRules.ShouldPageOnShiftSpace(true, false), "last page falls through");
session.ObserveCommittedText("，");
AssertTrue(session.Context.Length > 0, "comma keeps context");
session.ObserveCommittedText("。");
AssertEqual("", session.Context);
```

Also assert `Cancel()` clears visible candidates, and `ShouldSelect(49, true)` maps virtual key 49 to candidate number 1 while unshifted digits return 0.

- [ ] **Step 2: Run tests and verify red**

Expected: missing `SmartCandidateSession` and `SmartCandidateKeyRules`.

- [ ] **Step 3: Implement the session**

`SmartCandidateSession` owns context, merged candidates, page offset, and last activity time. Use the longest available suffix among 3, 2, then 1 characters. Merge memory candidates before table candidates with stable de-duplication.

Required API:

```csharp
public bool Enabled { get; set; }
public bool ContinuousEnabled { get; set; }
public string Context { get; }
public IList<string> VisibleCandidates { get; }
public bool HasNextPage { get; }
public DateTime LastActivityUtc { get; }
public void ObserveCommittedText(string text);
public string Select(int oneBasedIndex);
public bool NextPage();
public void Cancel();
public void EndContext();
public bool ShouldFlush(DateTime utcNow, TimeSpan idle);
```

`Select` records an explicit prediction choice, appends the selected phrase to context, and refreshes candidates. `ObserveCommittedText` learns completed Chinese runs; `。！？；\r\n` end the run and context, while `，` preserves context. Non-Chinese output cancels the visible page without teaching garbage.

Add `SmartCandidateKeyRules` to `KeyboardHookState.cs`:

```csharp
public static int SelectionNumber(int virtualKey, bool shiftDown)
{
    return shiftDown && virtualKey >= 49 && virtualKey <= 53 ? virtualKey - 48 : 0;
}

public static bool ShouldPageOnShiftSpace(bool visible, bool hasNextPage)
{
    return visible && hasNextPage;
}
```

- [ ] **Step 4: Verify and commit**

```powershell
dotnet run --project .\tools\UclLiuCoreTests\UclLiuCoreTests.csproj
git add SmartCandidateSession.cs KeyboardHookState.cs uclliu.csproj tools\UclLiuCoreTests\UclLiuCoreTests.csproj tools\UclLiuCoreTests\Program.cs
git commit -s -m "feat: add smart candidate session"
```

### Task 4: Integrate output, root ranking, keys, and idle save

**Files:**
- Modify: `uclliu.cs`
- Modify: `Form1.cs`
- Modify: `tools/UclLiuCoreTests/Program.cs`

- [ ] **Step 1: Add failing integration-policy tests**

Add pure policy tests proving prediction selection precedes normal shifted digit handling, prediction paging precedes half/full, and mode changes request cancellation. Keep these tests against `SmartCandidateKeyRules`; do not instantiate `Form1`.

- [ ] **Step 2: Add runtime initialization in `uclliu`**

Add constants and fields:

```csharp
public const string CANDIDATE_FILE = "candidate.txt";
public const string CANDIDATE_MEMORY_FILE = "candidate_memory.json";
public SmartCandidateMemory smartCandidateMemory;
public SmartCandidateSession smartCandidates;
```

After `loadConfig()` resolves `my.pwd()`, default these INI keys to `1` when absent:

```text
SMART_CANDIDATE_ENABLE
SMART_CANDIDATE_CONTINUOUS
SMART_ROOT_ENABLE
```

Load table and memory once, create the session, and log invalid TSV line count. Add methods:

```csharp
public bool has_smart_candidate_table();
public bool has_visible_smart_candidates();
public bool try_select_smart_candidate(int number);
public bool try_page_smart_candidates();
public void cancel_smart_candidates();
public void flush_smart_candidate_memory(bool force);
public void clear_smart_candidate_memory();
```

- [ ] **Step 3: Integrate output and existing candidate labels**

After successful preparation in the central output path, call `smartCandidates.ObserveCommittedText(data)` and refresh `word_label` only when continuous prediction is enabled. Do not observe output produced by `Select` twice: `try_select_smart_candidate` records selection, sends the returned phrase through a private output method with `observeSmartCandidate=false`, then refreshes.

Before displaying normal root candidates, call:

```csharp
if (config["DEFAULT"]["SMART_ROOT_ENABLE"] == "1")
    candidates = smartCandidateMemory.RankRootCandidates(play_ucl_label, candidates).ToList();
```

When a normal root candidate is selected, record its root and word once.

Render prediction candidates as `1候選 2候選 ...`; retain the existing 0-based numbering for legacy root/phone candidate flows.

- [ ] **Step 4: Add hook priority and cancellation**

In `LowLevelKeyboardProc`, before half/full handling and normal shifted character output:

```csharp
int smartNumber = SmartCandidateKeyRules.SelectionNumber(ea, ucl.flag_is_shift_down);
if (keydown && smartNumber > 0 && ucl.try_select_smart_candidate(smartNumber)) return NO;

if (keydown && ea == 32 && ucl.flag_is_shift_down
    && SmartCandidateKeyRules.ShouldPageOnShiftSpace(
        ucl.has_visible_smart_candidates(), ucl.smartCandidates.HasNextPage)
    && ucl.try_page_smart_candidates()) return NO;
```

Call `cancel_smart_candidates()` on Esc, the first new root key, and every英／肥 toggle path.

- [ ] **Step 5: Add a 30-second UI timer and close flush**

Use one WinForms `Timer` owned by `Form1`, interval `30000`. On Tick call `flush_smart_candidate_memory(false)`; that method saves only when dirty and idle for at least three minutes. In `Form1_FormClosed`, stop the timer and call `flush_smart_candidate_memory(true)` before disposing.

- [ ] **Step 6: Verify and commit**

Run core tests and a temporary-output Debug build:

```powershell
$out = Join-Path $env:TEMP 'uclliu-smart-candidate-verify\'
& 'C:\Program Files\Microsoft Visual Studio\18\Community\MSBuild\Current\Bin\MSBuild.exe' .\uclliu.csproj /t:Build /p:Configuration=Debug /p:Platform=AnyCPU "/p:OutDir=$out"
git diff --check
```

Expected: all tests pass and build has zero errors.

```powershell
git add uclliu.cs Form1.cs tools\UclLiuCoreTests\Program.cs
git commit -s -m "feat: integrate smart candidate input"
```

### Task 5: Tray menu and GitHub download entry

**Files:**
- Modify: `Form1.cs`
- Modify: `TrayMenuText.cs`
- Modify: `tools/UclLiuCoreTests/Program.cs`

- [ ] **Step 1: Write failing menu-text tests**

Assert the exact missing-table label, enabled marks, `12.` submenu title, and `13. 離開(Quit)`.

- [ ] **Step 2: Implement menu text helpers**

Add constants/helpers to `TrayMenuText` so click handlers never branch on localized text:

```csharp
public const string CandidateDownload = "請先下載候選字";
public const string CandidateMenu = "12. 候選字相關";
public const string Exit = "13. 離開(Quit)";
```

- [ ] **Step 3: Build the submenu and handlers**

When the table is missing, add only `CandidateDownload`; its handler calls:

```csharp
Process.Start(new ProcessStartInfo {
    FileName = "https://github.com/shadowjohn/UCL_LIU_CSharp",
    UseShellExecute = true
});
```

When present, add toggles for total enable, continuous prediction, and smart root, plus clear memory. Each toggle updates its INI key and calls `saveConfig()`. Clear memory asks once with `MessageBoxButtons.YesNo`, then calls `clear_smart_candidate_memory()`.

- [ ] **Step 4: Verify and commit**

```powershell
dotnet run --project .\tools\UclLiuCoreTests\UclLiuCoreTests.csproj
git add Form1.cs TrayMenuText.cs tools\UclLiuCoreTests\Program.cs
git commit -s -m "feat: add smart candidate menu"
```

### Task 6: Bounded long-mode candidate width

**Files:**
- Modify: `UiLayoutCalculator.cs`
- Modify: `uclliu.cs`
- Modify: `tools/UclLiuCoreTests/Program.cs`

- [ ] **Step 1: Write failing width tests**

```csharp
AssertEqual(500, UiLayoutCalculator.BoundCandidateWidth(500, 320, 1200));
AssertEqual(320, UiLayoutCalculator.BoundCandidateWidth(200, 320, 1200));
AssertEqual(1200, UiLayoutCalculator.BoundCandidateWidth(1600, 320, 1200));
```

- [ ] **Step 2: Implement the clamp**

```csharp
public static int BoundCandidateWidth(int measuredWidth, int minimumWidth, int screenWorkWidth)
{
    return Math.Max(minimumWidth, Math.Min(measuredWidth, screenWorkWidth));
}
```

In long mode only, measure the rendered candidate label plus existing chrome widths and resize the form using this clamp. Do not alter short-mode packing.

- [ ] **Step 3: Verify and commit**

Run tests, build to temporary `OutDir`, then commit the three files with message `feat: resize long candidate view`.

### Task 7: Licensed table converter and release packaging

**Files:**
- Create: `tools/convert-chewing-candidates.ps1`
- Create after license verification: `THIRD_PARTY_CANDIDATE_DATA.md`
- Modify: `tools/package-release.ps1`
- Modify: `.gitignore`
- Test: manual deterministic converter check

- [ ] **Step 1: Verify the source-data license before copying data**

Record the exact upstream repository, commit, source file path, copyright, license, redistribution requirements, and transformation in `THIRD_PARTY_CANDIDATE_DATA.md`. If the data file has no explicit reusable license, stop this task and use only the small test table; do not copy or commit `tsi.csv` or generated production data.

- [ ] **Step 2: Implement deterministic conversion**

The PowerShell script accepts `-InputPath` and `-OutputPath`, reads CSV with `Import-Csv -Header Phrase,Frequency,Phone`, keeps phrases of length 2 or more, creates every 1～3-character prefix mapping to the remaining suffix, sorts by numeric frequency descending then ordinal phrase, removes duplicates, and writes UTF-8 without BOM TSV. Use this complete core:

```powershell
[CmdletBinding()]
param([Parameter(Mandatory)][string]$InputPath, [Parameter(Mandatory)][string]$OutputPath)
$ErrorActionPreference = 'Stop'
$rows = @{}
Import-Csv -LiteralPath $InputPath -Header Phrase,Frequency,Phone | ForEach-Object {
    $phrase = [string]$_.Phrase
    $frequency = 0L
    [void][long]::TryParse([string]$_.Frequency, [ref]$frequency)
    if ($phrase.Length -lt 2) { return }
    $maxPrefix = [Math]::Min(3, $phrase.Length - 1)
    for ($length = 1; $length -le $maxPrefix; $length++) {
        $key = $phrase.Substring(0, $length)
        if (-not $rows.ContainsKey($key)) { $rows[$key] = @{} }
        $suffix = $phrase.Substring($length)
        if (-not $rows[$key].ContainsKey($suffix) -or $rows[$key][$suffix] -lt $frequency) {
            $rows[$key][$suffix] = $frequency
        }
    }
}
$lines = foreach ($key in @($rows.Keys | Sort-Object)) {
    $candidates = @($rows[$key].GetEnumerator() |
        Sort-Object @{ Expression = 'Value'; Descending = $true }, @{ Expression = 'Key'; Descending = $false } |
        ForEach-Object { $_.Key })
    $key + "`t" + ($candidates -join "`t")
}
[System.IO.File]::WriteAllLines($OutputPath, $lines, [System.Text.UTF8Encoding]::new($false))
```

- [ ] **Step 3: Run a small deterministic check**

Create a temporary CSV containing `王小明`, `王先生`, and duplicate `王小明` rows. Run the converter twice and assert both SHA256 hashes match and the first line begins `王<TAB>小明` when its frequency is higher.

- [ ] **Step 4: Package only the full zip**

Add `candidate.txt` to `$optionalFiles` in `tools/package-release.ps1`. Keep the single `uclliu.exe` unchanged. Update release-note text so the zip lists `candidate.txt` and the single exe states that the candidate table must be downloaded manually.

Ignore upstream source downloads and local `candidate_memory.json`, `.tmp`, and `.broken`; do not ignore the licensed generated `candidate.txt` once approved for Git.

- [ ] **Step 5: Commit**

```powershell
git add tools\convert-chewing-candidates.ps1 tools\package-release.ps1 .gitignore
if (Test-Path -LiteralPath .\THIRD_PARTY_CANDIDATE_DATA.md) { git add THIRD_PARTY_CANDIDATE_DATA.md }
git commit -s -m "build: prepare smart candidate data"
```

Include `candidate.txt` in this commit only after the Step 1 license gate passes.

### Task 8: Documentation, TODO closure, and full verification

**Files:**
- Modify: `README.md`
- Modify: `CHANGELOG.md`
- Modify: `todo.md`
- Modify: `history.md`

- [ ] **Step 1: Update user documentation**

Document file placement, manual GitHub download, four menu items, `Shift+1～5`, `Shift+Space`, cancellation rules, three INI keys, local-only memory, and release zip behavior. State explicitly that there is no automatic download and no new DLL/NuGet.

- [ ] **Step 2: Update project records**

Add the feature under `CHANGELOG.md` Unreleased. Add a dated `history.md` entry with implementation and verification results. Check completed `todo.md` items only when their corresponding command or manual check has passed; leave the licensed production-table items unchecked if Task 7 stopped at its license gate.

- [ ] **Step 3: Run final automated verification**

```powershell
dotnet run --project .\tools\UclLiuCoreTests\UclLiuCoreTests.csproj
$out = Join-Path $env:TEMP 'uclliu-smart-candidate-final\'
& 'C:\Program Files\Microsoft Visual Studio\18\Community\MSBuild\Current\Bin\MSBuild.exe' .\uclliu.csproj /t:Build /p:Configuration=Debug /p:Platform=AnyCPU "/p:OutDir=$out"
git diff --check
git status --short
```

Expected: core tests pass, Debug build succeeds, diff check is clean, and status lists only intended documentation changes.

- [ ] **Step 4: Run manual smoke test from `bin\Debug`**

Close the running app, build to the normal Debug output, and start `bin\Debug\uclliu.exe`. Verify:

1. Missing table shows only the GitHub entry and normal typing still works.
2. Test table shows `王 -> 小明` and `Shift+1` sends `小明`.
3. More than five candidates page with `Shift+Space`; the final page falls through to half/full.
4. Esc, new root input, and 英／肥 switch clear prediction.
5. Repeated ordinary `王小明` input promotes `小明` after restart.
6. Smart root selection promotes a frequently chosen existing root candidate.
7. Long mode expands within the active screen; short mode remains compact.

- [ ] **Step 5: Commit documentation**

```powershell
git add README.md CHANGELOG.md todo.md history.md
git commit -s -m "docs: document smart candidates"
```
