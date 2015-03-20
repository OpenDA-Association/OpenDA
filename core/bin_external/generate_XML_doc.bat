@echo off
rem The setlocal statement must be on top!
setlocal enabledelayedexpansion

rem Either
rem    take care that %OPENDA_BINDIR% is set as env. var
call %~dp0\setup_openda.bat

rem    Add %OPENDA_BINDIR% to the path
rem    So that AltovaAutomation.dll will be found
set PATH=%OPENDA_BINDIR%;%PATH%

rem ==== check Java runtime ====
if "%JAVA_HOME%" == "" set JAVA_HOME=..
if not exist "%JAVA_HOME%\bin\java.exe" goto Error0

rem ==== run ===
"%JAVA_HOME%\bin\java" -Xms128m -Xmx1024m -classpath XMLSpyAPI.jar;AltovaAutomation.jar;openda_core.jar org.openda.tools.GenerateXmlDoc %1 %2 %3 %4
if errorlevel 1 goto Error1
endlocal
goto End

rem ==== show errors ===
:Error0
echo No JAVA runtime found - please check this
goto End

:Error1
echo Error running GenerateXmlDoc - please check the error messages
echo Usage: generate_XML_doc.bat [-s <SourceDir>] [-d <DestDir>]
pause
goto End

rem ==== done ===
:End




