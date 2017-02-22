@echo off

rem GO TO PROJECT ROOT FOLDER
pushd %~dp0
cd ..\

rem NOW DO THE MAIN SCRIPT IN THE ROOT PROJECT FOLDER
set TMP_DIST_DIR=

echo "Creating release folders in the project root (%CD%\release\windows)"
if not exist "%CD%\release\" mkdir "%CD%\release"
if not exist "%CD%\release\windows\" mkdir "%CD%\release\windows"


rem BUILDING qua-server
echo "Building qua-server..."
for /f "delims=" %%a in ('stack --stack-yaml apps\hs\qua-server\stack.yaml path --dist-dir') do @set TMP_DIST_DIR=%%a
stack build --install-ghc --stack-yaml apps\hs\qua-server\stack.yaml --flag qua-server:-postgresql
if not exist "%CD%\release\windows\static" mkdir "%CD%\release\windows\static"
xcopy "%CD%\apps\hs\qua-server\static" "%CD%\release\windows\static" /Y /s /e /h /q
copy "%CD%\apps\hs\qua-server\%TMP_DIST_DIR%\build\qua-server\qua-server.exe" "%CD%\release\windows\qua-server.exe"


rem BUILDING helen
echo "Building helen..."
for /f "delims=" %%a in ('stack --stack-yaml apps\hs\helen\stack.yaml path --dist-dir') do @set TMP_DIST_DIR=%%a
stack build --install-ghc --stack-yaml apps\hs\helen\stack.yaml
copy "%CD%\apps\hs\helen\%TMP_DIST_DIR%\build\helen\helen.exe" "%CD%\release\windows\helen.exe"


rem BUILDING siren
echo "Building siren..."
for /f "delims=" %%a in ('stack --stack-yaml services\siren\stack.yaml path --dist-dir') do @set TMP_DIST_DIR=%%a
stack build --install-ghc --stack-yaml services\siren\stack.yaml
copy "%CD%\services\siren\%TMP_DIST_DIR%\build\siren\siren.exe" "%CD%\release\windows\siren.exe"


echo "Finished successfully!"
popd
pause
