@echo off

rem GO TO PROJECT ROOT FOLDER
pushd %~dp0

START "siren job" /b "sirenDelayed.bat"
START "example service job" /b "sExampleDelayed.bat"
helen.exe

popd
pause