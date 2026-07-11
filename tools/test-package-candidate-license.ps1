[CmdletBinding()]
param()

$ErrorActionPreference = "Stop"
$projectRoot = Join-Path ([System.IO.Path]::GetTempPath()) ("uclliu-package-license-test-" + [Guid]::NewGuid().ToString("N"))
$repoRoot = (Resolve-Path -LiteralPath (Join-Path $PSScriptRoot "..")).Path
$packageScript = Join-Path $PSScriptRoot "package-release.ps1"

function Assert-PackageFails {
    param([string]$Name, [string]$ExpectedMessage)

    $output = Join-Path $projectRoot $Name
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
    if (Test-Path -LiteralPath (Join-Path $output ("uclliu-v" + $Name + ".zip"))) {
        throw "授權檔缺失時不應產生 zip。"
    }
}

try {
    $build = Join-Path $projectRoot "bin\Release"
    New-Item -ItemType Directory -Path $build | Out-Null
    [System.IO.File]::WriteAllBytes((Join-Path $build "uclliu.exe"), [byte[]](1, 2, 3, 4))
    [System.IO.File]::WriteAllText((Join-Path $projectRoot "candidate.txt"), "王`t小明", [System.Text.UTF8Encoding]::new($false))

    Assert-PackageFails -Name "missing-notice" -ExpectedMessage "THIRD_PARTY_CANDIDATE_DATA.md"

    Copy-Item -LiteralPath (Join-Path $repoRoot "THIRD_PARTY_CANDIDATE_DATA.md") -Destination $projectRoot
    Assert-PackageFails -Name "missing-license" -ExpectedMessage "LICENSES\LGPL-2.1-or-later.txt"

    $licenses = Join-Path $projectRoot "LICENSES"
    New-Item -ItemType Directory -Path $licenses | Out-Null
    Copy-Item -LiteralPath (Join-Path $repoRoot "LICENSES\LGPL-2.1-or-later.txt") -Destination $licenses
    $positiveOutput = Join-Path $projectRoot "positive"
    & $packageScript -ProjectRoot $projectRoot -OutputDirectory $positiveOutput -Version "positive" *> $null

    $extract = Join-Path $projectRoot "extract"
    Expand-Archive -LiteralPath (Join-Path $positiveOutput "uclliu-vpositive.zip") -DestinationPath $extract
    foreach ($required in @("candidate.txt", "THIRD_PARTY_CANDIDATE_DATA.md", "LICENSES\LGPL-2.1-or-later.txt")) {
        if (-not (Test-Path -LiteralPath (Join-Path $extract $required))) { throw "zip 缺少 $required" }
    }
    if ((Get-FileHash -LiteralPath (Join-Path $build "uclliu.exe")).Hash -ne
        (Get-FileHash -LiteralPath (Join-Path $positiveOutput "uclliu.exe")).Hash) {
        throw "單檔 exe 不應被修改。"
    }

    Write-Host "PASS candidate package license gate"
} finally {
    if (Test-Path -LiteralPath $projectRoot) {
        Remove-Item -LiteralPath $projectRoot -Recurse -Force
    }
}
