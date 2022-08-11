@echo off
<<<<<<< HEAD
rem This script requires one input argument i.e. file name of the dimr_config.xml
rem To use this script, set DFLOWFMDIR to the installation directory of D-Flow FM.
set DFLOWFMDIR=
rem set DFLOWFMDIR=p:\h6\opt\delft3dfm\2.13.02.67836\
if "%DFLOWFMDIR%"=="" goto error_not_defined

set dimr="%DFLOWFMDIR%\x64\dimr\scripts\run_dimr.bat"
rem set dimr="%DFLOWFMDIR%\plugins\DeltaShell.Dimr\run_dimr.bat"
=======
rem This script requires one input argument: a file name (typically called dimr_config.xml)
rem To use this script, uncomment the next line and set DFLOWFMDIR to <Your installation base dir>
rem set DFLOWFMDIR=
rem or create an environment variable %DFLOWFMDIR%
if "%DFLOWFMDIR%"=="" goto error_not_defined

set dimr="%DFLOWFMDIR%\x64\dimr\scripts\run_dimr.bat"
>>>>>>> a42825562... Updated all examples to use run_dimr.{bat|sh} to start D-FLOW FM.
rem 
rem check if dimr is available
rem
if not exist %dimr% goto error_dimr_not_found
<<<<<<< HEAD

=======
>>>>>>> a42825562... Updated all examples to use run_dimr.{bat|sh} to start D-FLOW FM.
rem
rem start DIMR for D-Flow FM 
rem 
%dimr% %1
goto eof

rem
rem error messages
rem

:error_not_defined
echo ERROR in ./stochModel/bin/start_dimr.bat:
echo No installation directory of D-Flow FM specified.
goto eof

:error_dimr_not_found
echo ERROR in ./stochModel/bin/start_dimr.bat:
echo File not found: %dimr%
goto eof

:eof
exit /B %ERRORLEVEL%
