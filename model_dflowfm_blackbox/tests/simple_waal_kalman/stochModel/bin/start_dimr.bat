@echo off
rem set DFLOWFMDIR=
if "%DFLOWFMDIR%"=="" goto error_not_defined

set dimr="%DFLOWFMDIR%\plugins\DeltaShell.Dimr\run_dimr.bat"
rem 
rem check if dimr is available
rem
if not exist %dimr% goto error_dimr_not_found
rem
rem start DIMR for D-Flow FM 
rem 
%dimr% dimrConfig.xml
goto eof

rem
rem error messages
rem

:error_not_defined
echo No installation directory of D-Flow FM specified.
goto eof

:error_dimr_not_found
echo ERROR in ./stochModel/bin/start_dimr.bat:
echo File not found: %dimr%
goto eof

:eof
exit /B %ERRORLEVEL%
