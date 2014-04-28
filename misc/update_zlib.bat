@echo off
setlocal
cd /d %~dp0

REM src/zlib を更新します
REM curl.exe, 7za.exe が必要です。

set ZLIBVER=zlib-1.2.8
set ZLIBZIP=zlib128.zip
set ZLIBDIST=%~dp0\..\src\zlib

curl http://zlib.net/%ZLIBZIP% -o %ZLIBZIP%
7za x -y %ZLIBZIP%

cd %ZLIBVER%

call "%VS120COMNTOOLS%\vsvars32.bat"

nmake -f win32/Makefile.msc LOC=-MT
copy zlib.lib %ZLIBDIST%\Release
copy zlib.pdb %ZLIBDIST%\Release

nmake -f win32/Makefile.msc clean

nmake -f win32/Makefile.msc LOC=-MTd
copy zlib.lib %ZLIBDIST%\Debug
copy zlib.pdb %ZLIBDIST%\Debug

copy zconf.h %ZLIBDIST%
copy zlib.h %ZLIBDIST%

cd ..

RD /S /Q %ZLIBVER%
DEL /Q %ZLIBZIP%
