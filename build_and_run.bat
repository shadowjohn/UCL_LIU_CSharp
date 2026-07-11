@echo off
setlocal EnableExtensions
cd /d "%~dp0"
set "ROOT=%~dp0"
set "TARGET=%ROOT%bin\Debug\uclliu.exe"

where pwsh.exe >nul 2>nul
if errorlevel 1 (
    echo ERROR: pwsh.exe is required.
    exit /b 2
)

echo [1/4] Stopping only the repo Debug uclliu.exe...
pwsh.exe -NoLogo -NoProfile -NonInteractive -Command "$ErrorActionPreference='Stop'; $target=[IO.Path]::GetFullPath($env:TARGET); $deadline=[DateTime]::UtcNow.AddSeconds(10); $find={ @(Get-CimInstance Win32_Process -Filter \"Name='uclliu.exe'\" | Where-Object { $_.ExecutablePath -and [string]::Equals([IO.Path]::GetFullPath($_.ExecutablePath), $target, [StringComparison]::OrdinalIgnoreCase) }) }; $running=@(& $find); foreach ($process in $running) { Write-Host ('Stopping PID {0}: {1}' -f $process.ProcessId, $target); $null=Invoke-CimMethod -InputObject $process -MethodName Terminate }; do { $remaining=@(& $find); if ($remaining.Count -eq 0) { break }; Start-Sleep -Milliseconds 250 } while ([DateTime]::UtcNow -lt $deadline); if ($remaining.Count -ne 0) { Write-Error ('Timed out stopping exact target: {0}' -f $target); exit 1 }; Write-Host ('Stopped {0} matching process(es).' -f $running.Count)"
if errorlevel 1 (
    echo ERROR: Could not stop the exact Debug executable.
    exit /b 10
)

echo [2/4] Locating MSBuild...
set "MSBUILD=C:\Program Files\Microsoft Visual Studio\18\Community\MSBuild\Current\Bin\MSBuild.exe"
if exist "%MSBUILD%" goto msbuild_found
set "MSBUILD="
set "VSWHERE=%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\vswhere.exe"
if exist "%VSWHERE%" goto vswhere_found
for /f "delims=" %%I in ('where vswhere.exe 2^>nul') do if not defined VSWHERE set "VSWHERE=%%I"
:vswhere_found
if not defined VSWHERE goto msbuild_missing
if not exist "%VSWHERE%" goto msbuild_missing
for /f "delims=" %%I in ('call "%VSWHERE%" -latest -products * -requires Microsoft.Component.MSBuild -find MSBuild\**\Bin\MSBuild.exe') do if not defined MSBUILD set "MSBUILD=%%I"
if not defined MSBUILD goto msbuild_missing
if not exist "%MSBUILD%" goto msbuild_missing
goto msbuild_found

:msbuild_missing
echo ERROR: MSBuild was not found in Visual Studio 18 Community or through vswhere.
exit /b 20

:msbuild_found
echo MSBuild: %MSBUILD%
echo [3/4] Building Debug Any CPU...
"%MSBUILD%" "%ROOT%uclliu.sln" /t:Build /p:Configuration=Debug /p:Platform="Any CPU" /nologo
if errorlevel 1 (
    echo ERROR: Build failed. uclliu.exe was not started.
    exit /b 30
)
if not exist "%TARGET%" (
    echo ERROR: Build succeeded but the expected executable is missing: %TARGET%
    exit /b 31
)

echo [4/4] Starting the exact Debug executable...
pwsh.exe -NoLogo -NoProfile -NonInteractive -Command "$ErrorActionPreference='Stop'; $target=[IO.Path]::GetFullPath($env:TARGET); $workingDirectory=Split-Path -Parent $target; $started=Start-Process -FilePath $target -WorkingDirectory $workingDirectory -WindowStyle Hidden -PassThru; Start-Sleep -Milliseconds 750; $process=Get-CimInstance Win32_Process -Filter ('ProcessId={0}' -f $started.Id); if (-not $process -or -not $process.ExecutablePath -or -not [string]::Equals([IO.Path]::GetFullPath($process.ExecutablePath), $target, [StringComparison]::OrdinalIgnoreCase)) { Write-Error ('Exact Debug executable did not remain running: {0}' -f $target); exit 1 }; $version=[Diagnostics.FileVersionInfo]::GetVersionInfo($target); Write-Host ('STARTED PID={0} FileVersion={1} ProductVersion={2} Path={3}' -f $process.ProcessId, $version.FileVersion, $version.ProductVersion, $target)"
if errorlevel 1 (
    echo ERROR: Build passed, but startup verification failed.
    exit /b 40
)

echo SUCCESS: Debug build is running.
exit /b 0
