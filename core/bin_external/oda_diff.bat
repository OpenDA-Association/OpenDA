@echo off
rem The setlocal statement must be on top!
setlocal enabledelayedexpansion

rem
rem Purpose : startup script for DataCopier tool. 
rem Args    : use -h
rem Examples: oda_copier.bat
rem Author  : M. verlaan/A. Markus
rem License : GPL

rem Either
rem    take care that %OPENDA_BINDIR% is set as env. var
rem    (e.g. by running setup_openda.bat on the bin dir)
rem or
rem    Set openda_bindir right here below.

rem :loop
rem set file_ind = 0 
rem IF NOT "%1"=="" (
rem     IF "%1"=="-h" (
rem         goto Usage
rem     )
rem     ELSE IF "%1"=="-c" (
rem         SET dataobect[%%file_ind]=%2
rem         SHIFT
rem     )
rem     ELSE IF "%1"=="-a" (
rem         SET arguments[%%file_ind]=%2
rem         SHIFT
rem     )
rem     ELSE (
rem         SET files[%%file_ind]=%1
rem         SHIFT
rem     )
rem     SHIFT
rem     GOTO :loop
rem )
rem ECHO Username = %user%
rem ECHO Other option = %other%
call %~dp0\setup_openda.bat

rem ==== check Java runtime ====
if "%JAVA_HOME%" == "" set JAVA_HOME=..
if not exist "%JAVA_HOME%\bin\java.exe" goto Error0

"%JAVA_HOME%\bin\java" -classpath %OPENDA_BINDIR%\* org.openda.exchange.iotools.DataObjectDiff %*
if errorlevel 1 goto Error3
endlocal
goto End

rem ==== show errors ===
:Error0
echo No JAVA runtime found - please check this
goto End

:Error1
echo The file %OPENDA_BINDIR%\openda_core.jar does not exist
pause
goto End

:Usage
echo Usage: oda_diff.bat 
pause
goto End

if "%1" == "" goto Error1

:Error3
echo Error running OpenDA - please check the error messages
pause
goto End

rem ==== done ===
:End
