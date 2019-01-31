@echo off
rem
rem Checkout xyzzy to _dist/ and build clean
rem Create xyzzy-(VERSION).tar.gz and xyzzy-src-(VERSION).tar.gz
rem

setlocal
cd /d %~dp0

if "%1"=="" goto usage

set VERSION=%1

set TAG=v%VERSION%
set APPNAME=xyzzy

set BASEDIR=%~dp0
set GIT_REPO=%BASEDIR%
set DISTROOT=%BASEDIR%\_dist
set DISTDIR=%BASEDIR%\_dist\%APPNAME%
set SRCVER=%APPNAME%-src-%VERSION%
set SRCDIR=%BASEDIR%\_dist\%SRCVER%
set DIST_ARCHIVE=%DISTROOT%\%APPNAME%-%VERSION%.tar.gz
set SRC_ARCHIVE=%DISTROOT%\%APPNAME%-src-%VERSION%.tar.gz

call git tag %TAG% -a -m "%APPNAME% %VERSION% released!" || exit /b 1
call git tag

cd %BASEDIR%
rd /S /Q %DISTROOT% 2> nul

mkdir %DISTROOT%
mkdir %SRCDIR%
mkdir %DISTDIR%
mkdir %DISTDIR%\lisp
mkdir %DISTDIR%\etc
mkdir %DISTDIR%\docs
mkdir %DISTDIR%\reference
mkdir %DISTDIR%\site-lisp

cd %SRCDIR%
call git clone %GIT_REPO% %SRCDIR% || exit /b 1
call git checkout %TAG% || git tag exit /b 1
rd /S /Q .git 2> nul

pushd .
cd %DISTROOT%
tar -czf %SRC_ARCHIVE% %SRCVER%
popd

call build.bat Release Build normal "/p:GenerateDebugInformation=false" || exit /b 1
call bytecompile.bat || exit /b 1

xcopy /F /G /H /R /K /Y *.exe %DISTDIR%
xcopy /F /G /H /R /K /Y LICENSE %DISTDIR%\docs\
xcopy /F /G /H /R /K /Y LEGAL.md %DISTDIR%\docs\
xcopy /F /G /H /R /K /Y /S /E lisp %DISTDIR%\lisp\
xcopy /F /G /H /R /K /Y /S /E etc %DISTDIR%\etc\
xcopy /F /G /H /R /K /Y /S /E docs %DISTDIR%\docs\
xcopy /F /G /H /R /K /Y /S /E reference %DISTDIR%\reference\

cd %DISTROOT%
tar -czf  %DIST_ARCHIVE% %APPNAME%
goto :eof

:usage
echo Usage: %0 VERSION
goto :eof