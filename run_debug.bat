@echo off
pushd "%~dp0" >nul
start "" /wait ".\bin\Debug\uclliu.exe" --debug
popd >nul
