@echo off
rem The setlocal statement must be on top!
setlocal enabledelayedexpansion

rem Either
rem    take care that %OPENDA_BINDIR% is set as env. var
rem    (e.g. by running setup_openda.bat on the bin dir)
rem or
rem    Set openda_bindir right here below.
call %~dp0\setup_openda.bat

rem    For running examples that use native code,
rem    add %OPENDA_BINDIR% to the path
rem    Note that it should appear as the first directory
set PATH=%OPENDA_BINDIR%;%PATH%

rem ==== check if jre available as distributed with openda ====
set OPENDA_JRE=%OPENDA_BINDIR%..\jre
if not exist "%OPENDA_JRE%\bin\java.exe" goto else
rem openda jre is available
set JAVA_HOME=%OPENDA_JRE%
goto endif
:else
rem no openda jre is available, check if there is a default one
if "%JAVA_HOME%" == "" goto Error0
:endif

rem ==== check availability and arguments ===
if not exist %OPENDA_BINDIR%\openda_core.jar goto Error1

rem ==== run ===
set addJar=
for /r %OPENDA_BINDIR% %%G in (*.jar) do set addJar=!addJar!;"%%G"
"%JAVA_HOME%\bin\java" -Xms128m -Xmx1024m -classpath %addJar% org.openda.model_swan.swivt.CaseExtractorGUI
if errorlevel 1 goto Error2
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

:Error2
echo Error running OpenDA - please check the error messages
pause
goto End

rem ==== done ===
:End
