[CmdletBinding()]
param(
    [string]$Version = "",
    [string]$Configuration = "Release",
    [string]$ProjectRoot = "",
    [string]$OutputDirectory = "",
    [switch]$IncludeWavs
)

$ErrorActionPreference = "Stop"

if ([string]::IsNullOrWhiteSpace($ProjectRoot)) {
    $resolvedProjectRoot = Resolve-Path -LiteralPath (Join-Path $PSScriptRoot "..")
} else {
    $resolvedProjectRoot = Resolve-Path -LiteralPath $ProjectRoot
}
$ProjectRoot = $resolvedProjectRoot.ProviderPath

if ([string]::IsNullOrWhiteSpace($OutputDirectory)) {
    $OutputDirectory = Join-Path $ProjectRoot "artifacts"
}

$buildDirectory = Join-Path $ProjectRoot ("bin\" + $Configuration)
$exePath = Join-Path $buildDirectory "uclliu.exe"
if (-not (Test-Path -LiteralPath $exePath)) {
    throw "找不到建置產物：$exePath"
}

if (-not (Test-Path -LiteralPath $OutputDirectory)) {
    New-Item -ItemType Directory -Path $OutputDirectory | Out-Null
}
$OutputDirectory = (Resolve-Path -LiteralPath $OutputDirectory).Path

$packageRoot = Join-Path $OutputDirectory "package"
if (Test-Path -LiteralPath $packageRoot) {
    Remove-Item -LiteralPath $packageRoot -Recurse -Force
}
New-Item -ItemType Directory -Path $packageRoot | Out-Null

Copy-Item -LiteralPath $exePath -Destination (Join-Path $packageRoot "uclliu.exe") -Force

$optionalFiles = @("pinyi.txt", "candidate.txt", "THIRD_PARTY_CANDIDATE_DATA.md", "README.md", "LICENSE")
foreach ($fileName in $optionalFiles) {
    $sourcePath = Join-Path $ProjectRoot $fileName
    if (Test-Path -LiteralPath $sourcePath) {
        Copy-Item -LiteralPath $sourcePath -Destination (Join-Path $packageRoot $fileName) -Force
    }
}

$lgplLicensePath = Join-Path $ProjectRoot "LICENSES\LGPL-2.1-or-later.txt"
if (Test-Path -LiteralPath $lgplLicensePath) {
    $licensesDestination = Join-Path $packageRoot "LICENSES"
    if (-not (Test-Path -LiteralPath $licensesDestination)) {
        New-Item -ItemType Directory -Path $licensesDestination | Out-Null
    }
    Copy-Item -LiteralPath $lgplLicensePath -Destination (Join-Path $licensesDestination "LGPL-2.1-or-later.txt") -Force
}

function Copy-TsfBridgeRuntime {
    param(
        [string]$SourceRoot,
        [string]$DestinationRoot
    )

    if (-not (Test-Path -LiteralPath $SourceRoot)) {
        return
    }

    $destination = Join-Path $DestinationRoot "tsf_bridge"
    if (-not (Test-Path -LiteralPath $destination)) {
        New-Item -ItemType Directory -Path $destination | Out-Null
    }

    $rootFiles = @(
        "UclTsfBridge.dll",
        "register_tsf_bridge.bat",
        "unregister_tsf_bridge.bat",
        "unlock_tsf_bridge.ps1",
        "README.md"
    )
    foreach ($fileName in $rootFiles) {
        $sourcePath = Join-Path $SourceRoot $fileName
        if (Test-Path -LiteralPath $sourcePath) {
            Copy-Item -LiteralPath $sourcePath -Destination (Join-Path $destination $fileName) -Force
        }
    }

    foreach ($arch in @("x64", "x86")) {
        $sourceDll = Join-Path $SourceRoot (Join-Path $arch "UclTsfBridge.dll")
        if (Test-Path -LiteralPath $sourceDll) {
            $archDestination = Join-Path $destination $arch
            if (-not (Test-Path -LiteralPath $archDestination)) {
                New-Item -ItemType Directory -Path $archDestination | Out-Null
            }
            Copy-Item -LiteralPath $sourceDll -Destination (Join-Path $archDestination "UclTsfBridge.dll") -Force
        }
    }
}

$tsfSourcePath = Join-Path $buildDirectory "tsf_bridge"
if (-not (Test-Path -LiteralPath $tsfSourcePath)) {
    $tsfSourcePath = Join-Path $ProjectRoot "tsf_bridge"
}
Copy-TsfBridgeRuntime -SourceRoot $tsfSourcePath -DestinationRoot $packageRoot

if ($IncludeWavs) {
    $sourcePath = Join-Path $buildDirectory "wavs"
    if (-not (Test-Path -LiteralPath $sourcePath)) {
        $sourcePath = Join-Path $ProjectRoot "wavs"
    }

    if (Test-Path -LiteralPath $sourcePath) {
        Copy-Item -LiteralPath $sourcePath -Destination (Join-Path $packageRoot "wavs") -Recurse -Force
    }
}

$versionSuffix = $Version.Trim()
if ([string]::IsNullOrWhiteSpace($versionSuffix)) {
    $versionSuffix = "local"
}
if (-not $versionSuffix.StartsWith("v", [System.StringComparison]::OrdinalIgnoreCase)) {
    $versionSuffix = "v" + $versionSuffix
}

$zipPath = Join-Path $OutputDirectory ("uclliu-" + $versionSuffix + ".zip")
if (Test-Path -LiteralPath $zipPath) {
    Remove-Item -LiteralPath $zipPath -Force
}
Compress-Archive -Path (Join-Path $packageRoot "*") -DestinationPath $zipPath -Force

$singleExePath = Join-Path $OutputDirectory "uclliu.exe"
Copy-Item -LiteralPath $exePath -Destination $singleExePath -Force

$notesPath = Join-Path $OutputDirectory "release-notes.md"
$zipContents = if ($IncludeWavs) {
    "uclliu.exe、pinyi.txt、candidate.txt、第三方候選資料聲明、LGPL 授權全文、wavs、tsf_bridge、README 與 LICENSE"
} else {
    "uclliu.exe、pinyi.txt、candidate.txt、第三方候選資料聲明、LGPL 授權全文、tsf_bridge、README 與 LICENSE"
}
$soundNote = if ($IncludeWavs) {
    "本次封包包含 wavs 音效目錄，請確認音效檔具備可再散布授權。"
} else {
    "官方發行檔不內含 wav 音效；若要啟用打字音，請自行放入自有或合法授權的 wavs\*.wav。"
}
@"
UCL_LIU_CSharp $versionSuffix

- uclliu-$versionSuffix.zip：推薦下載包，含 $zipContents。
- uclliu.exe：單檔版，不含 TSF Bridge、同音/注音資料、固定候選資料與音效素材。

$soundNote

字碼表因版權因素不包含在發行檔內，請自行放入 liu.json、liu.cin、liu-uni.tab 或其他可轉換字碼表。candidate.txt 採 LGPL-2.1-or-later，來源與再散布聲明見 THIRD_PARTY_CANDIDATE_DATA.md，授權全文見 LICENSES/LGPL-2.1-or-later.txt。
"@ | Set-Content -LiteralPath $notesPath -Encoding UTF8

Write-Host "Package zip: $zipPath"
Write-Host "Single exe:  $singleExePath"
Write-Host "Notes:       $notesPath"
