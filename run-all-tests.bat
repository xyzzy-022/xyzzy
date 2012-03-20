@echo off
setlocal
set TESTDIR=%~dp0
set XYZZYHOME=%TESTDIR%..
"%XYZZYHOME%\xyzzy.exe" -l "%TESTDIR%run-all.l"
