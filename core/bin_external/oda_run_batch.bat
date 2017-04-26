@echo off
rem Usage: oda_run_batch.bat <odafile>
rem    or: oda_run_batch.bat <odafile> -jre "location where java is installed"

rem The setlocal statement must be on top!
setlocal enabledelayedexpansion

rem Either
rem    take care that %OPENDA_BINDIR% is set as env. var
rem    (e.g. by running setup_openda.bat on the bin dir)
rem or
rem    Set openda_bindir right here below.
rem OPENDADIR added to PATH by setup_openda.bat
call %~dp0\setup_openda.bat

rem ==== check command line argumtents
if "%1"=="" goto Error2

if "%2" == "" goto no_jre
if not "%2"=="-jre" goto Error4
set OPENDA_JRE="%~3%"
if exist %OPENDA_JRE%\bin\java.exe goto JAVA_OK
goto Error4

:no_jre
rem ==== if not specified by the user,
rem      check if jre is available as distributed with openda ====
set OPENDA_JRE="%OPENDA_BINDIR%..\jre"
if exist "%OPENDA_JRE%\bin\java.exe" goto JAVA_OK

rem no openda jre is available, check if there is a default one
if "%JAVA_HOME%" == "" goto Error0
set OPENDA_JRE=%JAVA_HOME%

:JAVA_OK
rem ==== check availability and arguments ===
if not exist %OPENDA_BINDIR%\openda_core.jar goto Error1

rem ==== run ===
"%OPENDA_JRE%\bin\java" -Xms128m -Xmx1024m -classpath %OPENDA_BINDIR%\* org.openda.application.OpenDaApplication %1%
if errorlevel 1 goto Error3
endlocal
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
echo Usage: oda_run_batch.bat OpenDAConfigFile
pause
goto End

:Error3
echo Error running OpenDA - please check the error messages
pause
goto End

:Error4
echo Error: incorrect user-specified JRE.
echo Usage: oda_run_batch.bat odafile -jre "location where java is installed"
pause
goto End

rem ==== done ===
:End
