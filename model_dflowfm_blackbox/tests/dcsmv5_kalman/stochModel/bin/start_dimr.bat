@echo off
set executable="C:\Program Files (x86)\Deltares\Delft3D FM Suite 2017 HMWQ (1.2.2.36603)\plugins\DeltaShell.Dimr\run_dimr.bat"
rem 
rem check if executable is available on PATH
rem
rem where /Q %executable%
rem if errorlevel 1 goto error_not_found
rem if errorlevel 2 goto error_unknown
rem 
rem set work-dir and mdu-file  
rem
rem set mdufile=%1%
rem set work=%cd%
rem
rem copy ini files to work-directory
rem
rem for /f %%j in ("%executable%") do (
rem     set DFLOWFMDIR=%%~dp$PATH:j
rem )
rem copy %DFLOWFMDIR%unstruc.ini %work%
rem copy %DFLOWFMDIR%interact.ini %work%
rem
rem start D-Flow FM
rem 
%executable% dimrConfig.xml
goto eof

rem
rem error messages
rem

:error_not_found
echo ERROR in ./stochModel/bin/start_dimr.bat:
echo No installation directory of D-Flow FM found in PATH.
echo PATH = %PATH%
goto eof

:eof
exit /B %ERRORLEVEL%
