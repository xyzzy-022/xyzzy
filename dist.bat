@echo off
setlocal

if "%1"=="" goto usage

set VERSION=%1

set TAG=v%VERSION%
set APPNAME=xyzzy
set ARCHIVE=%APPNAME%-%VERSION%.zip

set BASEDIR=%~dp0
set GIT_REPO=%BASEDIR%
set DISTROOT=%BASEDIR%\_dist
set DISTDIR=%BASEDIR%\_dist\%APPNAME%
set BUILDDIR=%BASEDIR%\_dist\build

call git tag %TAG% -a -m "%APPNAME% %VERSION% released!" || exit /b 1
call git tag

cd %BASEDIR%
rd /S /Q %DISTROOT% 2> nul

mkdir %DISTROOT%
mkdir %BUILDDIR%
mkdir %DISTDIR%
mkdir %DISTDIR%\lisp
mkdir %DISTDIR%\etc
mkdir %DISTDIR%\docs
mkdir %DISTDIR%\reference
mkdir %DISTDIR%\site-lisp

cd %BUILDDIR%
call git clone %GIT_REPO% %BUILDDIR% || exit /b 1
call git checkout %TAG% || git tag; exit /b 1
call build.bat || exit /b 1
call bytecompile.bat || exit /b 1

xcopy /F /G /H /R /K /Y *.exe %DISTDIR%
xcopy /F /G /H /R /K /Y LICENSE %DISTDIR%\docs\
xcopy /F /G /H /R /K /Y LEGAL.md %DISTDIR%\docs\
xcopy /F /G /H /R /K /Y /S /E lisp %DISTDIR%\lisp\
xcopy /F /G /H /R /K /Y /S /E etc %DISTDIR%\etc\
xcopy /F /G /H /R /K /Y /S /E docs %DISTDIR%\docs\
xcopy /F /G /H /R /K /Y /S /E reference %DISTDIR%\reference\

cd %DISTROOT%
7za a %ARCHIVE% %DISTDIR%
goto :eof

:usage
echo Usage: %0 VERSION
goto :eof
