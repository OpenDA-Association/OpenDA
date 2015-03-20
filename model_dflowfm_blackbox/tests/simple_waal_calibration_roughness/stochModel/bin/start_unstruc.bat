@echo off
rem 
rem check if unstruc.exe is available on PATH
rem
where /Q unstruc.exe
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
for /f %%j in ("unstruc.exe") do (
    set DFLOWFMDIR=%%~dp$PATH:j
)
copy %DFLOWFMDIR%unstruc.ini %work%
copy %DFLOWFMDIR%interact.ini %work%
rem
rem start D-Flow FM
rem 
unstruc.exe %mdufile% --autostartstop --nodisplay
goto eof

rem
rem error messages
rem

:error_not_found
echo ERROR in ./stochModel/bin/start_unstruc.bat:
echo No installation directory of D-Flow FM found in PATH.
echo PATH = %PATH%
goto eof

:error_unknown
echo ERROR in ./stochModel/bin/start_unstruc.bat:
echo problem in command "where unstruc.exe"
goto eof

:eof
