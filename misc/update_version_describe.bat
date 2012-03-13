@echo off
setlocal
cd %~dp0\..

set GIT_DESCRIBE=git describe --tags --dirty
set VERSION_DESCRIBE_H=src\version-describe.gen.h
set VERSION_DESCRIBE_TMP=src\version-describe.%RANDOM%.tmp

rem git describe の書式
rem   (tag)-(tag 以降のコミット回数)-g(hash)
rem
rem git describe に --long を指定しないとコミット回数が 0 の場合に
rem タグしか表示されない

for /F "usebackq" %%i in (`%GIT_DESCRIBE% --long`) do (
  set DESCRIBE_LONG=%%i
)
for /F "usebackq" %%i in (`%GIT_DESCRIBE%`) do (
  set DESCRIBE=%%i
)

if not "%DESCRIBE%"=="%DESCRIBE_LONG%" (
  rem リリースバージョン
  echo. > %VERSION_DESCRIBE_TMP%
) else (
  rem 開発バージョン
  echo #define PROGRAM_VERSION_DESCRIBE_STRING "%DESCRIBE%" > %VERSION_DESCRIBE_TMP%
)

if not exist %VERSION_DESCRIBE_H% goto update
fc %VERSION_DESCRIBE_H% %VERSION_DESCRIBE_TMP% > nul
if errorlevel 1 goto update
goto not_update

:update
echo %DESCRIBE%
copy /Y %VERSION_DESCRIBE_TMP% %VERSION_DESCRIBE_H% > nul
goto cleanup

:not_update
goto cleanup

:cleanup
DEL /Q /S %VERSION_DESCRIBE_TMP% > nul
exit /b 0
