[CmdletBinding()]
param(
    [string]$Version = "",
    [string]$Configuration = "Release",
    [string]$ProjectRoot = "",
    [string]$OutputDirectory = ""
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

$optionalFiles = @("pinyi.txt", "README.md", "LICENSE")
foreach ($fileName in $optionalFiles) {
    $sourcePath = Join-Path $ProjectRoot $fileName
    if (Test-Path -LiteralPath $sourcePath) {
        Copy-Item -LiteralPath $sourcePath -Destination (Join-Path $packageRoot $fileName) -Force
    }
}

$optionalDirectories = @("wavs", "tsf_bridge")
foreach ($directoryName in $optionalDirectories) {
    $sourcePath = Join-Path $buildDirectory $directoryName
    if (-not (Test-Path -LiteralPath $sourcePath)) {
        $sourcePath = Join-Path $ProjectRoot $directoryName
    }

    if (Test-Path -LiteralPath $sourcePath) {
        Copy-Item -LiteralPath $sourcePath -Destination (Join-Path $packageRoot $directoryName) -Recurse -Force
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
@"
UCL_LIU_CSharp $versionSuffix

- uclliu-$versionSuffix.zip：推薦下載包，含 uclliu.exe、pinyi.txt、wavs、tsf_bridge、README 與 LICENSE。
- uclliu.exe：單檔版，不含 TSF Bridge、同音/注音資料與音效素材。

字碼表因版權因素不包含在發行檔內，請自行放入 liu.json、liu.cin、liu-uni.tab 或其他可轉換字碼表。
"@ | Set-Content -LiteralPath $notesPath -Encoding UTF8

Write-Host "Package zip: $zipPath"
Write-Host "Single exe:  $singleExePath"
Write-Host "Notes:       $notesPath"
