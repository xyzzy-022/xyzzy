@echo off
setlocal
cd /d %~dp0

rem Usage: run-tests.bat [TESTS...]
rem
rem Example:
rem  すべてのテストを実行
rem  run-tests.bat
rem
rem  lisp-tests.l だけを実行
rem  run-tests.bat unittest/lisp-tests.l
rem
rem  lisp-tests.l と editor-tests.l だけを実行
rem  run-tests.bat unittest/lisp-tests.l unittest/editor-tests.l

set TESTDIR=%~dp0unittest
set XYZZYHOME=%TESTDIR%\..
set XYZZYINIFILE=
set XYZZYCONFIGPATH=
"%XYZZYHOME%\xyzzy.exe" -q -trace -l "%TESTDIR%\run-tests-helper.l" %*
