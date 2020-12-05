@echo off

rem Setup bin. dir for openda.
rem Script has to be run from the dir. containing the script.
set ODASYSTEM=win64_ifort
set OPENDA_BINDIR=%~dp0
for %%a in (%OPENDA_BINDIR:~0,-1%) do set "OPENDADIR=%%~dpa"
set ODALIBDIR="%OPENDADIR%\bin\%ODASYSTEM%"
echo.
echo OPENDADIR set to %OPENDADIR%
echo System set to %ODASYSTEM%

rem Try if setup_openda.bat can be found in the Windows PATH
rem If not OPENDA_BINDIR should be added to the PATH
rem Do not run WHERE from the current directory as it might contain setup_openda.bat
pushd .
CD /D c:\
where /Q setup_openda.bat
set NOTFOUND=%errorlevel%
popd
if %NOTFOUND%==0 goto :End

echo Include OpenDA bin\ dir to PATH
echo.
set PATH=%OPENDADIR%\bin;%PATH%
:End
