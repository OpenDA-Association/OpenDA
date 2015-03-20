@echo off
rem The setlocal statement must be on top!
setlocal enabledelayedexpansion

rem
rem Purpose : startup script for oda_dumpio tool
rem Args    : filename
rem           optional classname of ioObject -> if you omit the class name a list of possible names is given
rem Examples: oda_dumpio.sh bodem.inc        : gives a list of possible ioObjects
rem           oda_dumpio.sh hoekvhld.noos org.openda.exchange.ioobjects.NoosTimeSeriesIoObject : give summary of contents
rem Author  : M. verlaan
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

rem ==== check availability and arguments ===
if not exist %OPENDA_BINDIR%\openda_core.jar goto Error1

set addJar=
for /r %OPENDA_BINDIR% %%G in (*.jar) do set addJar=!addJar!;"%%G"
..\jre\bin\java -classpath %addJar% org.openda.exchange.iotools.ioDumper %1 %2
if errorlevel 1 goto Error3
endlocal
goto End

..\jre\bin\java org.openda.exchange.iotools.ioDumper $1 $2

rem ==== show errors ===
:Error0
echo No JAVA runtime found - please check this
goto End
:Error1
echo The file %OPENDA_BINDIR%\openda_core.jar does not exist
pause
goto End

:Error3
echo Error running OpenDA - please check the error messages
pause
goto End

rem ==== done ===
:End
