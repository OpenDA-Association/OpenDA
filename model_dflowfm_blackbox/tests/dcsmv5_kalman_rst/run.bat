rem
rem This script runs the standard tests
rem

rem
rem calibration without constraint
rem
set DFLOWFMDIR=d:\checkouts\2025.02

REM set OPENDA_BINDIR, PATH and CLASSPATH
cd ..\..\..\bin
set OPENDA_BINDIR=%CD%
set PATH=%OPENDA_BINDIR%;%PATH%
REM ==== check if jre available as distributed with openda ====
set OPENDA_JRE=%OPENDA_BINDIR%\..\jre
echo %OPENDA_JRE%
if not exist "%OPENDA_JRE%\bin\java.exe" goto else
rem openda jre is available
set JAVA_HOME=%OPENDA_JRE%
goto endif
:else
rem no openda jre is available, check if there is a default one
if "%JAVA_HOME%" == "" goto exitwitherror0
:endif
set CLASSPATH=%OPENDA_BINDIR%\*

cd ..\model_dflowfm_blackbox\tests\dcsmv5_kalman_rst
oda_run_batch.bat ENKF.oda


