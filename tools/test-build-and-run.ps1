$ErrorActionPreference = 'Stop'

$root = Split-Path -Parent $PSScriptRoot
$scriptPath = Join-Path $root 'build_and_run.bat'
if (-not (Test-Path -LiteralPath $scriptPath)) {
    throw 'build_and_run.bat is missing'
}

$source = Get-Content -LiteralPath $scriptPath -Raw
$required = @(
    'cd /d "%~dp0"',
    'set "TARGET=%ROOT%bin\Debug\uclliu.exe"',
    'Get-CimInstance Win32_Process',
    '[StringComparison]::OrdinalIgnoreCase',
    'C:\Program Files\Microsoft Visual Studio\18\Community\MSBuild\Current\Bin\MSBuild.exe',
    'vswhere.exe',
    'uclliu.sln" /t:Build /p:Configuration=Debug /p:Platform="Any CPU"',
    'if errorlevel 1',
    'Start-Process -FilePath $target -WorkingDirectory $workingDirectory -WindowStyle Hidden -PassThru'
)

foreach ($text in $required) {
    if (-not $source.Contains($text)) {
        throw "build_and_run.bat missing required text: $text"
    }
}

$stop = $source.IndexOf('Get-CimInstance Win32_Process', [StringComparison]::Ordinal)
$build = $source.IndexOf('uclliu.sln" /t:Build', [StringComparison]::Ordinal)
$gate = $source.IndexOf('if errorlevel 1', $build, [StringComparison]::Ordinal)
$start = $source.IndexOf('Start-Process -FilePath $target', [StringComparison]::Ordinal)
if (-not ($stop -lt $build -and $build -lt $gate -and $gate -lt $start)) {
    throw 'required stop -> build -> success gate -> start sequence is broken'
}

$projectPath = Join-Path $root 'uclliu.csproj'
$project = Get-Content -LiteralPath $projectPath -Raw
if ($project -notmatch '<Content Include="candidate\.txt">\s*<CopyToOutputDirectory>PreserveNewest</CopyToOutputDirectory>') {
    throw 'uclliu.csproj must copy candidate.txt to the build output'
}

Write-Host 'PASS: build_and_run source contract'
