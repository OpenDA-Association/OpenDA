@echo off
set executable=dflowfm.exe
rem 
rem check if executable is available on PATH
rem
where /Q %executable%
if errorlevel 1 goto error_not_found
if errorlevel 2 goto error_unknown
rem 
rem set work-dir and mdu-file  
rem
set mdufile=%1%
set work=%cd%
rem
rem copy ini files to work-directory
rem
for /f %%j in ("%executable%") do (
    set DFLOWFMDIR=%%~dp$PATH:j
)
copy %DFLOWFMDIR%unstruc.ini %work%
copy %DFLOWFMDIR%interact.ini %work%
rem
rem start D-Flow FM
rem 
%executable% --autostartstop --nodisplay %mdufile%
goto eof

rem
rem error messages
rem

:error_not_found
echo ERROR in ./stochModel/bin/start_dflowfm.bat:
echo No installation directory of D-Flow FM found in PATH.
echo PATH = %PATH%
goto eof

:error_unknown
echo ERROR in ./stochModel/bin/start_dflowfm.bat:
echo problem in command "where %executable%"
goto eof

:eof
exit /B %ERRORLEVEL%