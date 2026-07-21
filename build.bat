@echo off
setlocal EnableExtensions EnableDelayedExpansion

set "ROOT_DIR=%~dp0"
pushd "%ROOT_DIR%" >nul

set "CONFIGURATION=%~1"
if "%CONFIGURATION%"=="" set "CONFIGURATION=Debug"
set "OUT_DIR=%~2"
if "%OUT_DIR%"=="" set "OUT_DIR=artifacts\build-%CONFIGURATION%"
if not "%OUT_DIR%"=="" (
  set "LAST_OUT_DIR_CHAR=!OUT_DIR:~-1!"
  if not "!LAST_OUT_DIR_CHAR!"=="\" set "OUT_DIR=!OUT_DIR!\"
)
set "PROJECT=uclliu.csproj"
set "MSBUILD="

rem Keep this file ASCII-only because cmd.exe may break UTF-8 batch files on user machines.
if "%MSBUILD%"=="" call :UseMSBuild "%ProgramFiles%\Microsoft Visual Studio\18\Community\MSBuild\Current\Bin\amd64\MSBuild.exe"
if "%MSBUILD%"=="" call :UseMSBuild "%ProgramFiles%\Microsoft Visual Studio\18\BuildTools\MSBuild\Current\Bin\amd64\MSBuild.exe"
if "%MSBUILD%"=="" call :UseMSBuild "%ProgramFiles%\Microsoft Visual Studio\18\Professional\MSBuild\Current\Bin\amd64\MSBuild.exe"
if "%MSBUILD%"=="" call :UseMSBuild "%ProgramFiles%\Microsoft Visual Studio\18\Enterprise\MSBuild\Current\Bin\amd64\MSBuild.exe"
if "%MSBUILD%"=="" call :UseMSBuild "%ProgramFiles%\Microsoft Visual Studio\2026\Community\MSBuild\Current\Bin\amd64\MSBuild.exe"
if "%MSBUILD%"=="" call :UseMSBuild "%ProgramFiles%\Microsoft Visual Studio\2026\BuildTools\MSBuild\Current\Bin\amd64\MSBuild.exe"
if "%MSBUILD%"=="" call :UseMSBuild "%ProgramFiles%\Microsoft Visual Studio\2026\Professional\MSBuild\Current\Bin\amd64\MSBuild.exe"
if "%MSBUILD%"=="" call :UseMSBuild "%ProgramFiles%\Microsoft Visual Studio\2026\Enterprise\MSBuild\Current\Bin\amd64\MSBuild.exe"
if "%MSBUILD%"=="" call :UseMSBuild "%ProgramFiles%\Microsoft Visual Studio\2022\Community\MSBuild\Current\Bin\amd64\MSBuild.exe"
if "%MSBUILD%"=="" call :UseMSBuild "%ProgramFiles%\Microsoft Visual Studio\2022\BuildTools\MSBuild\Current\Bin\amd64\MSBuild.exe"
if "%MSBUILD%"=="" call :UseMSBuild "%ProgramFiles%\Microsoft Visual Studio\2022\Professional\MSBuild\Current\Bin\amd64\MSBuild.exe"
if "%MSBUILD%"=="" call :UseMSBuild "%ProgramFiles%\Microsoft Visual Studio\2022\Enterprise\MSBuild\Current\Bin\amd64\MSBuild.exe"
if "%MSBUILD%"=="" call :UseMSBuild "%ProgramFiles(x86)%\Microsoft Visual Studio\2019\BuildTools\MSBuild\Current\Bin\amd64\MSBuild.exe"
if "%MSBUILD%"=="" call :UseMSBuild "%ProgramFiles(x86)%\Microsoft Visual Studio\2019\Community\MSBuild\Current\Bin\amd64\MSBuild.exe"

if "%MSBUILD%"=="" if exist "%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\vswhere.exe" (
  for /f "usebackq tokens=*" %%i in (`"%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\vswhere.exe" -latest -products * -requires Microsoft.Component.MSBuild -find MSBuild\**\Bin\amd64\MSBuild.exe`) do set "MSBUILD=%%i"
)

if "%MSBUILD%"=="" (
  echo [ERROR] MSBuild.exe not found. Install Visual Studio Build Tools with .NET Framework 4.5.2 targeting pack.
  popd >nul
  exit /b 1
)

if not exist "%PROJECT%" (
  echo [ERROR] Project not found: %PROJECT%
  popd >nul
  exit /b 1
)

echo [INFO] MSBuild: %MSBUILD%
echo [INFO] Build %PROJECT% %CONFIGURATION% AnyCPU...
echo [INFO] Output: %OUT_DIR%
"%MSBUILD%" "%PROJECT%" /t:Rebuild /p:Configuration=%CONFIGURATION% /p:Platform=AnyCPU /p:OutDir=%OUT_DIR% /nr:false /m
if errorlevel 1 (
  echo [ERROR] Build failed.
  popd >nul
  exit /b 1
)

echo [OK] Built %OUT_DIR%uclliu.exe
popd >nul
exit /b 0

:UseMSBuild
if exist "%~1" set "MSBUILD=%~1"
exit /b 0
