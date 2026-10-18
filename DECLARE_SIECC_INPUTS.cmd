@echo off
rem Double-click to write the input-identification declaration (author only; records names, sizes and SHA-256; never copies file contents).
rem Always Windows PowerShell 5.1 (never PowerShell 7), with the 5.1 default PSModulePath whatever shell this was started from.
setlocal
chcp 65001 >nul
set "PSModulePath=%USERPROFILE%\Documents\WindowsPowerShell\Modules;%ProgramFiles%\WindowsPowerShell\Modules;%SystemRoot%\System32\WindowsPowerShell\v1.0\Modules"
"%SystemRoot%\System32\WindowsPowerShell\v1.0\powershell.exe" -NoProfile -ExecutionPolicy Bypass -STA -File "%~dp0scripts\declare_project_inputs.ps1" %*
set RC=%ERRORLEVEL%
echo.
pause
exit /b %RC%
