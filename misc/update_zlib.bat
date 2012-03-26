@echo off
setlocal

REM src/zlib を更新します
REM curl.exe, 7za.exe が必要です。

set ZLIBVER=zlib-1.2.6
set ZLIBZIP=zlib126.zip
set ZLIBDIST=%~dp0\..\src\zlib

cd %~dp0
curl http://zlib.net/%ZLIBZIP% -o %ZLIBZIP%
7za x -y %ZLIBZIP%

cd %ZLIBVER%

call "%VS100COMNTOOLS%\vsvars32.bat"

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
