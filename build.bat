@echo off
setlocal
cd /d %~dp0



if "%1"=="" (set CONFIGURATION=Release;Platform="x64") else (set CONFIGURATION=%1)
if "%2"=="" (set TARGET=Build) else (set TARGET=%2)
if "%3"=="" (set VERBOSITY=normal) else (set VERBOSITY=%3)

echo Clean xyzzy...
msbuild xyzzy.sln /nologo /p:Configuration=%CONFIGURATION% /target:Clean /verbosity:%VERBOSITY% %4 %5 %6 %7 %8 %9 || exit /b 1
echo Build xyzzy...
msbuild xyzzy.sln /nologo /p:Configuration=%CONFIGURATION% /target:%TARGET% /verbosity:%VERBOSITY% %4 %5 %6 %7 %8 %9 || exit /b 1
