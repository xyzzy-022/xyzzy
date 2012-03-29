@echo off

echo Byte compile...
rem startup.lc だけ「存在するファイルを作成することは出来ません」と言われて
rem たまにエラーになるので削除
del /S /Q lisp\startup.lc >nul 2>&1

del /S /Q xyzzy.wxp >nul 2>&1
set XYZZYHOME=%~dp0
xyzzy.exe -load misc/makelc.l -e "(makelc t)" -f kill-xyzzy
del /S /Q xyzzy.wxp 2> nul
