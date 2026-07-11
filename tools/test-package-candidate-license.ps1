[CmdletBinding()]
param()

$ErrorActionPreference = "Stop"
$projectRoot = Join-Path ([System.IO.Path]::GetTempPath()) ("uclliu-package-license-test-" + [Guid]::NewGuid().ToString("N"))
$repoRoot = (Resolve-Path -LiteralPath (Join-Path $PSScriptRoot "..")).Path
$packageScript = Join-Path $PSScriptRoot "package-release.ps1"

function Assert-PackageFails {
    param([string]$Name, [string]$ExpectedMessage)

    $output = Join-Path $projectRoot $Name
    $zipPath = Join-Path $output ("uclliu-v" + $Name + ".zip")
    $zipExisted = Test-Path -LiteralPath $zipPath
    $failed = $false
    try {
        & $packageScript -ProjectRoot $projectRoot -OutputDirectory $output -Version $Name *> $null
    } catch {
        $failed = $true
        if ($_.Exception.Message -notlike "*$ExpectedMessage*") {
            throw "錯誤訊息未指出缺檔：$($_.Exception.Message)"
        }
    }
    if (-not $failed) { throw "candidate 缺少授權檔時打包未失敗。" }
    if (-not $zipExisted -and (Test-Path -LiteralPath $zipPath)) {
        throw "授權檔缺失時不應產生 zip。"
    }
}

function Write-CandidateNotice {
    param([string]$Hash)

    [System.IO.File]::WriteAllText(
        (Join-Path $projectRoot "THIRD_PARTY_CANDIDATE_DATA.md"),
        "# Test notice`n`n- Candidate SHA-256: ``$Hash```n",
        [System.Text.UTF8Encoding]::new($false))
}

try {
    $build = Join-Path $projectRoot "bin\Release"
    New-Item -ItemType Directory -Path $build | Out-Null
    [System.IO.File]::WriteAllBytes((Join-Path $build "uclliu.exe"), [byte[]](1, 2, 3, 4))
    [System.IO.File]::WriteAllText((Join-Path $projectRoot "candidate.txt"), "王`t小明", [System.Text.UTF8Encoding]::new($false))

    Assert-PackageFails -Name "missing-notice" -ExpectedMessage "THIRD_PARTY_CANDIDATE_DATA.md"

    $candidateHash = (Get-FileHash -LiteralPath (Join-Path $projectRoot "candidate.txt") -Algorithm SHA256).Hash
    Write-CandidateNotice -Hash $candidateHash
    Assert-PackageFails -Name "missing-license" -ExpectedMessage "LICENSES\LGPL-2.1-or-later.txt"

    $licenses = Join-Path $projectRoot "LICENSES"
    New-Item -ItemType Directory -Path $licenses | Out-Null
    Copy-Item -LiteralPath (Join-Path $repoRoot "LICENSES\LGPL-2.1-or-later.txt") -Destination $licenses

    $positiveOutput = Join-Path $projectRoot "atomic"
    New-Item -ItemType Directory -Path $positiveOutput | Out-Null
    $oldZip = Join-Path $positiveOutput "uclliu-vatomic.zip"
    $oldExe = Join-Path $positiveOutput "uclliu.exe"
    $oldNotes = Join-Path $positiveOutput "release-notes.md"
    [System.IO.File]::WriteAllBytes($oldZip, [byte[]](9, 8, 7))
    [System.IO.File]::WriteAllBytes($oldExe, [byte[]](6, 5, 4))
    [System.IO.File]::WriteAllText($oldNotes, "old notes")
    $oldHashes = @{}
    foreach ($path in @($oldZip, $oldExe, $oldNotes)) {
        $oldHashes[$path] = (Get-FileHash -LiteralPath $path).Hash
    }

    Copy-Item -LiteralPath (Join-Path $repoRoot "THIRD_PARTY_CANDIDATE_DATA.md") -Destination $projectRoot -Force
    Assert-PackageFails -Name "atomic" -ExpectedMessage "SHA-256"
    foreach ($path in @($oldZip, $oldExe, $oldNotes)) {
        if ($oldHashes[$path] -ne (Get-FileHash -LiteralPath $path).Hash) {
            throw "失敗打包改動了既有發行檔：$path"
        }
    }

    Write-CandidateNotice -Hash $candidateHash
    & $packageScript -ProjectRoot $projectRoot -OutputDirectory $positiveOutput -Version "atomic" *> $null

    $extract = Join-Path $projectRoot "extract"
    Expand-Archive -LiteralPath (Join-Path $positiveOutput "uclliu-vatomic.zip") -DestinationPath $extract
    foreach ($required in @("candidate.txt", "THIRD_PARTY_CANDIDATE_DATA.md", "LICENSES\LGPL-2.1-or-later.txt")) {
        if (-not (Test-Path -LiteralPath (Join-Path $extract $required))) { throw "zip 缺少 $required" }
    }
    if ((Get-FileHash -LiteralPath (Join-Path $build "uclliu.exe")).Hash -ne
        (Get-FileHash -LiteralPath (Join-Path $positiveOutput "uclliu.exe")).Hash) {
        throw "單檔 exe 不應被修改。"
    }
    if (Get-ChildItem -LiteralPath $positiveOutput -Filter ".package-stage-*" -Force) {
        throw "打包完成後不應殘留暫存目錄。"
    }

    Write-Host "PASS candidate package license gate"
} finally {
    if (Test-Path -LiteralPath $projectRoot) {
        Remove-Item -LiteralPath $projectRoot -Recurse -Force
    }
}
