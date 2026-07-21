@echo off
pushd "%~dp0" >nul
start "" /wait ".\artifacts\build-Debug\uclliu.exe" --debug
popd >nul
