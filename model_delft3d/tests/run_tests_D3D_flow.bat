@echo off
setlocal enabledelayedexpansion

REM Runs all tests in the tests directories one by one, and puts the output and 
REM results in a single directory test_results

REM set OPENDA_BINDIR, PATH and CLASSPATH
cd ..\..\..\bin
set OPENDA_BINDIR=%CD%
set PATH=%CD%;%PATH%

set CLASSPATH=%OPENDA_BINDIR%\*

cd ..\models\java\tests
if exist test_results rd /s/q test_results
mkdir test_results

echo.

set CURDIR=d3d_04_15_wind_calibration
mkdir test_results\%CURDIR%
call :run_single_test kalib04-15 results_dud.m results_dud.csv

set CURDIR=d3d_estuary_1d
mkdir test_results\%CURDIR%
call :run_single_test d3d_est1d_enkf enkf_results.m

set CURDIR=d3d_lake_2d
mkdir test_results\%CURDIR%
call :run_single_test d3d_lake2d_enkf enkf_results.m

set CURDIR=d3d_lak3_2d_gridnoise
mkdir test_results\%CURDIR%
call :run_single_test d3d_lake2d_enkf enkf_results.m

echo.
if defined ErrorOccurred goto exitwitherror
echo All tests were performed and finished without error
exit 0

:exitwitherror
echo One or more tests were not performed or finished with an error!
exit 1 

endlocal

:run_single_test

echo Running test: %CURDIR%\%1.oda
"%JAVA_HOME%\bin\java" -Xms128m -Xmx1024m -classpath %CLASSPATH% org.openda.application.OpenDaApplication %CURDIR%\%1.oda 1>test_results\%CURDIR%\%1.out 2>test_results\%CURDIR%\%1.err
if %errorlevel% gtr 0 goto Error1
if not (%2)==() copy %CURDIR%\%2 test_results\%CURDIR%\%1_%2 >nul
if not (%3)==() copy %CURDIR%\%3 test_results\%CURDIR%\%1_%3 >nul
if not (%4)==() copy %CURDIR%\%4 test_results\%CURDIR%\%1_%4 >nul
if not (%5)==() copy %CURDIR%\%5 test_results\%CURDIR%\%1_%5 >nul
if not (%6)==() copy %CURDIR%\%6 test_results\%CURDIR%\%1_%6 >nul
goto :eof

:Error1
echo ***Error occurred in test %CURDIR%\%1
set ErrorOccurred=1
goto :eof

:donotrun_single_test

echo ***Warning: %CURDIR%\%1.oda test is disabled
set ErrorOccurred=1
goto :eof
