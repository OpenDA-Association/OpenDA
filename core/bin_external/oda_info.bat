@echo off
rem The setlocal statement must be on top!
setlocal enabledelayedexpansion

rem
rem Purpose : startup script for Reflection tool. Lists all classes that implement the most important openda interfaces.
rem Args    : (optional) interface name
rem Examples: oda_info.bat
rem Author  : M. verlaan/A. Markus
rem License : GPL

rem Either
rem    take care that %OPENDA_BINDIR% is set as env. var
rem    (e.g. by running setup_openda.bat on the bin dir)
rem or
rem    Set openda_bindir right here below.
call %~dp0\setup_openda.bat

rem ==== check Java runtime ====
if "%JAVA_HOME%" == "" set JAVA_HOME=..
if not exist "%JAVA_HOME%\jre\bin\java.exe" goto Error0

set addJar=
for /r %OPENDA_BINDIR% %%G in (*.jar) do set addJar=!addJar!;"%%G"
%JAVA_HOME%\jre\bin\java -classpath %addJar% org.openda.utils.Reflection %1
if errorlevel 1 goto Error3
endlocal
goto End

..\jre\bin\java org.openda.utils.Reflection %1

rem ==== show errors ===
:Error0
echo No JAVA runtime found - please check this
goto End

:Error1
echo The file %OPENDA_BINDIR%\openda_core.jar does not exist
pause
goto End

:Error2
echo Usage: run_openda_app OpenDAConfigFile
pause
goto End

if "%1" == "" goto Error1

:Error3
echo Error running OpenDA - please check the error messages
pause
goto End

rem ==== done ===
:End
