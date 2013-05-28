@echo off
rem
rem _dist/ に xyzzy のバイナリを集めて xyzzy.zip を作成する。
rem 事前に build.bat と bytecompile.bat を実行してビルドしておくこと。
rem

setlocal
cd /d %~dp0

if "%1"=="" (
  set VERSION=dev
) else (
  set VERSION=%1
)

set APPNAME=xyzzy
set BASEDIR=%~dp0
set DISTROOT=%BASEDIR%\_dist
set DISTDIR=%DISTROOT%\%APPNAME%
set DIST_ARCHIVE=%DISTROOT%\%APPNAME%-%VERSION%.zip

cd %BASEDIR%
rd /S /Q %DISTDIR% 2> nul
del /Q %DIST_ARCHIVE% 2> nul

mkdir %DISTROOT% 2> nul
mkdir %DISTDIR%
mkdir %DISTDIR%\lisp
mkdir %DISTDIR%\etc
mkdir %DISTDIR%\docs
mkdir %DISTDIR%\reference
mkdir %DISTDIR%\site-lisp

xcopy /F /G /H /R /K /Y *.exe %DISTDIR%
xcopy /F /G /H /R /K /Y LICENSE %DISTDIR%\docs\
xcopy /F /G /H /R /K /Y LEGAL.md %DISTDIR%\docs\
xcopy /F /G /H /R /K /Y /S /E lisp %DISTDIR%\lisp\
xcopy /F /G /H /R /K /Y /S /E etc %DISTDIR%\etc\
xcopy /F /G /H /R /K /Y /S /E docs %DISTDIR%\docs\
xcopy /F /G /H /R /K /Y /S /E reference %DISTDIR%\reference\

cd %DISTROOT%
7za a %DIST_ARCHIVE% %DISTDIR%
