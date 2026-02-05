@echo off
rem Usage: oda_run_batch.bat <odafile>
rem or:    oda_run_batch.bat <odafile> [-jre "<java path>"] [-Xmx value]

setlocal enabledelayedexpansion

call %~dp0\setup_openda.bat

rem Default XMX (in MB) and OPENDA_JRE
set XMX=1024
set OPENDA_JRE="%OPENDA_BINDIR%\..\jre"

rem ==== check command line arguments ====

if "%1"=="" goto Error2
set OPENDA_CONFIG=%1%
shift

rem ==== parse remaining arguments (any order) ====
:parseLoop
if "%~1"=="" goto afterParse

if /I "%~1"=="-jre" (
    if "%~2"=="" goto Error4
    set "OPENDA_JRE=%~2"
    shift
    shift
    goto parseLoop
)

if /I "%~1"=="-Xmx" (
    if "%~2"=="" goto Error5
    set "XMX=%~2"
    shift
    shift
    goto parseLoop
)
shift
goto parseLoop

:afterParse

echo XMX=%XMX%
echo OPENDA_JRE=%OPENDA_JRE%
echo OPENDA_CONFIG=%OPENDA_CONFIG%

if exist %OPENDA_JRE%\bin\java.exe goto JAVA_OK
goto Error4

if "%JAVA_HOME%" == "" goto Error0
set OPENDA_JRE=%JAVA_HOME%

:JAVA_OK
if not exist %OPENDA_BINDIR%\openda_core.jar goto Error1

echo =========================================================================
echo Starting "%OPENDA_JRE%\bin\java -Xmx%XMX%m org.openda.application.OpenDaApplication %OPENDA_CONFIG% > openda_logfile.txt"

"%OPENDA_JRE%\bin\java" -Xms128m -Xmx%XMX%m -classpath %OPENDA_BINDIR%\* org.openda.application.OpenDaApplication %OPENDA_CONFIG% > openda_logfile.txt

if errorlevel 1 goto Error3

endlocal
echo Run finished
echo =========================================================================
goto End

rem ==== show errors ===
:Error0
echo No JAVA runtime found - please check this
pause
goto End

:Error1
echo The file %OPENDA_BINDIR%\openda_core.jar does not exist
pause
goto End

:Error2
echo Usage: oda_run_batch.bat OpenDAConfigFile [-Xmx value]
pause
goto End

:Error3
echo Error running OpenDA - please check the error messages
pause
goto End

:Error4
echo Error: incorrect user-specified JRE.
echo Usage: oda_run_batch.bat odafile [-jre "location"] [-Xmx value]
pause
goto End

:Error5
echo Error: -Xmx specified without a value (expected: -Xmx 2048)
pause
goto End

:End
