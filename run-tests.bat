@echo off

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

setlocal
set TESTDIR=%~dp0unittest
set XYZZYHOME=%TESTDIR%\..
"%XYZZYHOME%\xyzzy.exe" -trace -l "%TESTDIR%\run-tests.l" %*
