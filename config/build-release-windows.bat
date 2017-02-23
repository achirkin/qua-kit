@echo off

rem GO TO PROJECT ROOT FOLDER
pushd %~dp0
cd ..\

rem NOW DO THE MAIN SCRIPT IN THE ROOT PROJECT FOLDER
set TMP_DIST_DIR=
set BUILDING_PROGRAM=

echo Creating release folders in the project root (%CD%\release\windows)
if not exist "%CD%\release\" mkdir "%CD%\release"
if not exist "%CD%\release\windows\" mkdir "%CD%\release\windows"


rem BUILDING qua-server
echo Building qua-server...
for /f "delims=" %%a in ('stack --stack-yaml apps\hs\qua-server\stack.yaml path --dist-dir') do @set TMP_DIST_DIR=%%a
set BUILDING_PROGRAM=qua-server
call config\win-retry-build.bat stack build --install-ghc --stack-yaml apps\hs\qua-server\stack.yaml --flag qua-server:-postgresql
IF %ERRORLEVEL% NEQ 0 goto :FAILURE
if not exist "%CD%\release\windows\static" mkdir "%CD%\release\windows\static"
xcopy "%CD%\apps\hs\qua-server\static" "%CD%\release\windows\static" /Y /s /e /h /q
copy "%CD%\apps\hs\qua-server\%TMP_DIST_DIR%\build\qua-server\qua-server.exe" "%CD%\release\windows\qua-server.exe"


rem BUILDING helen
echo Building helen...
for /f "delims=" %%a in ('stack --stack-yaml apps\hs\helen\stack.yaml path --dist-dir') do @set TMP_DIST_DIR=%%a
set BUILDING_PROGRAM=helen
call config\win-retry-build.bat stack build --install-ghc --stack-yaml apps\hs\helen\stack.yaml
IF %ERRORLEVEL% NEQ 0 goto :FAILURE
copy "%CD%\apps\hs\helen\%TMP_DIST_DIR%\build\helen\helen.exe" "%CD%\release\windows\helen.exe"


rem BUILDING siren
echo Building siren...
for /f "delims=" %%a in ('stack --stack-yaml services\siren\stack.yaml path --dist-dir') do @set TMP_DIST_DIR=%%a
set BUILDING_PROGRAM=siren
call config\win-retry-build.bat stack build --install-ghc --stack-yaml services\siren\stack.yaml
IF %ERRORLEVEL% NEQ 0 goto :FAILURE
copy "%CD%\services\siren\%TMP_DIST_DIR%\build\siren\siren.exe" "%CD%\release\windows\siren.exe"

rem BUILDING example services
echo Building example services...
for /f "delims=" %%a in ('stack --stack-yaml services\examples-hs\stack.yaml path --dist-dir') do @set TMP_DIST_DIR=%%a
set BUILDING_PROGRAM=hs-example-service
call config\win-retry-build.bat stack build --install-ghc --stack-yaml services\examples-hs\stack.yaml
IF %ERRORLEVEL% NEQ 0 goto :FAILURE
copy "%CD%\services\examples-hs\%TMP_DIST_DIR%\build\hs-example-service\hs-example-service.exe" "%CD%\release\windows\hs-example-service.exe"


echo Finished successfully!
popd
pause
goto :EOF

:FAILURE
echo Failed to build %BUILDING_PROGRAM%!
pause

