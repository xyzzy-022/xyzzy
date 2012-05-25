@echo off
setlocal
cd /d %~dp0\..

set VERSION_DESCRIBE_H=src\gen\version-describe.h
set VERSION_DESCRIBE_TMP=src\gen\version-describe.%RANDOM%.tmp

if exist .git goto gen_git_version
goto gen_release_version

:gen_git_version
rem git describe の書式
rem   (tag)-(tag 以降のコミット回数)-g(hash)
rem
rem git describe に --long を指定しないとコミット回数が 0 の場合に
rem タグしか表示されない

set GIT_DESCRIBE=git describe --tags --dirty
for /F "usebackq" %%i in (`%GIT_DESCRIBE% --long 2^> nul`) do (
  set DESCRIBE_LONG=%%i
)
for /F "usebackq" %%i in (`%GIT_DESCRIBE% 2^> nul`) do (
  set DESCRIBE=%%i
)

if "%DESCRIBE%"=="" goto gen_release_version
if not "%DESCRIBE%"=="%DESCRIBE_LONG%" goto gen_release_version
goto gen_develop_version

:gen_release_version
echo. > %VERSION_DESCRIBE_TMP%
goto :check_update

:gen_develop_version
echo #define PROGRAM_VERSION_DESCRIBE_STRING "%DESCRIBE%" > %VERSION_DESCRIBE_TMP%
goto :check_update

:check_update
if not exist %VERSION_DESCRIBE_H% goto update
fc %VERSION_DESCRIBE_H% %VERSION_DESCRIBE_TMP% > nul
if errorlevel 1 goto update
goto not_update

:update
if not "%DESCRIBE%"=="" echo %DESCRIBE%
copy /Y %VERSION_DESCRIBE_TMP% %VERSION_DESCRIBE_H% > nul
goto cleanup

:not_update
goto cleanup

:cleanup
DEL /Q /S %VERSION_DESCRIBE_TMP% > nul
exit /b 0
