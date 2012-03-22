@echo off
setlocal
set TESTDIR=%~dp0unittest
set XYZZYHOME=%TESTDIR%\..
"%XYZZYHOME%\xyzzy.exe" -l "%TESTDIR%\run-all.l"
