@echo off
rem Usage: oda_run_gui.bat
rem    or: oda_run_gui.bat <odafile>
rem    or: oda_run_gui.bat -jre "location where java is installed"
rem    or: oda_run_gui.bat <odafile> -jre "location where java is installed"

rem The setlocal statement must be on top!
setlocal enabledelayedexpansion

rem Either
rem    take care that %OPENDA_BINDIR% is set as env. var
rem    (e.g. by running setup_openda.bat on the bin dir)
rem or
rem    Set openda_bindir right here below.
rem OPENDADIR added to PATH by setup_openda.bat
call %~dp0\setup_openda.bat

set OPENDA_BINDIR=%OPENDADIR%

rem ==== check command line argumtents
if "%1"=="" goto no_arguments

if not "%1"=="-jre" goto else
   set OPENDA_JRE="%~2%"
   set odafile=
   if exist %OPENDA_JRE%\bin\java.exe goto JAVA_OK
   goto Error3
:else
   rem first argument name of odafile
   set odafile=%1%
:end

if "%2" == "" goto no_jre
if not "%2" == "-jre" goto else
   set OPENDA_JRE="%~3%"
   if exist %OPENDA_JRE%\bin\java.exe goto JAVA_OK
   goto Error3 
:else
   goto Error3
:end

:no_arguments
set odafile=

:no_jre
rem ==== if not specified by the user,
rem      check if jre is available as distributed with openda ====
set OPENDA_JRE=%OPENDA_BINDIR%\..\jre

if exist "%OPENDA_JRE%\bin\java.exe" goto JAVA_OK
rem no openda jre is available, check if there is a default one
echo JAVA_HOME %JAVA_HOME%
if "%JAVA_HOME%" == "" goto Error0
set OPENDA_JRE=%JAVA_HOME%

:JAVA_OK
rem ==== check availability and arguments ===
if not exist %OPENDA_BINDIR%\openda_core.jar goto Error1

rem ==== run ===
echo %odafile%
"%OPENDA_JRE%\bin\java" -Xms512m -Xmx8g -classpath %OPENDA_BINDIR%\* org.openda.application.OpenDaApplication -gui %odafile%
if errorlevel 1 goto Error2
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
echo Error running OpenDA - please check the error messages
pause
goto End

:Error3
echo Error: incorrect user-specified JRE.
echo Usage: oda_run_gui.bat odafile -jre "location where java is installed"
echo    or: oda_run_gui.bat -jre "location where java is installed"
pause
goto End

rem ==== done ===
:End
