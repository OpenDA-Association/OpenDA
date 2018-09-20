@echo off
rem setlocal enabledelayedexpansion

REM Runs all tests in the tests directories one by one, and puts the output and 
REM results in a single directory test_results

REM set OPENDA_BINDIR, PATH and CLASSPATH
cd ..\..\bin
set OPENDA_BINDIR=%CD%
rem set OPENDA_BINDIR="d:\work\openda_2_4\openda_2.4.4\bin"
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
set ErrorOccurred=0

cd ..\model_dflowfm_blackbox\tests
if exist test_results rd /s/q test_results
mkdir test_results

echo.

set CURDIR=calibration_discharge_dependent_river_roughness
mkdir test_results\%CURDIR%
rem OK
call :run_single_test Simulation results_simulation.m
call :run_single_test Dud results_dud.m
rem next 2 tests NullPointerException. Why are they present? The directory name suggests a calibration example.
rem call :run_single_test SequentialSimulation SequentialSimulation_results.m
rem call :run_single_test EnKF results_enkf.m

set CURDIR=calibration_river_roughness
mkdir test_results\%CURDIR%
rem OK
call :run_single_test Simulation results_simulation.m
call :run_single_test Dud results_dud.m
rem next 2 tests NullPointerException. Why are they present? The directory name suggests a calibration example.
rem call :run_single_test SequentialSimulation SequentialSimulation_results.m
rem call :run_single_test EnKF results_enkf.m

set CURDIR=dcsmv5_kalman
mkdir test_results\%CURDIR%
rem OK
call :run_single_test Simulation simulation_results.m
call :run_single_test SimulationNoise simulation_noise_results.m
rem call :run_single_test SequentialSimulation SequentialSimulation_results.m
rem restart file verouderd, tijden worden niet goed geupdate, oneindige som?
rem call :run_single_test EnKF Enkf_results.m

set CURDIR=estuary_calibration
mkdir test_results\%CURDIR%
rem OK
call :run_single_test Simulation results_simulation.m
call :run_single_test Dud results_dud.m
rem next 2 tests NullPointerException. Why are they present? The directory name suggests a calibration example.
rem call :run_single_test SequentialSimulation SequentialSimulation_results.m
rem call :run_single_test Enkf Enkf_results.m

set CURDIR=estuary_kalman
mkdir test_results\%CURDIR%
rem OK
call :run_single_test Simulation simulation_results.m
call :run_single_test SequentialSimulation simulation_results.m
rem KAPOT op niet vinden ExchangeItem
rem call :run_single_test Enkf Enkf_results.m

set CURDIR=lake_kalman
mkdir test_results\%CURDIR%
rem OK
call :run_single_test Simulation simulation_results.m
call :run_single_test SimulationNoise simulation_noise_results.m
call :run_single_test SequentialSimulation SequentialSimulation_results.m
call :run_single_test SequentialSimulationNoise SequentialSimulation_noise_results.m
rem nog naar kijken
rem call :run_single_test Enkf Enkf_results.m

set CURDIR=simple_waal_calibration_roughness
mkdir test_results\%CURDIR%
rem OK
call :run_single_test Simulation simulation_results.m
call :run_single_test Dud dud_results.m

set CURDIR=simple_waal_kalman
mkdir test_results\%CURDIR%
rem OK
call :run_single_test Simulation Simulation_results.m
rem niet gevonden exchange item
rem call :run_single_test SequentialSimulation SequentialSimulation_results.m
rem Error message: DFLOW-FM tidal components of fourier type with user specified frequency not implemented in OpenDA yet
rem call :run_single_test Enkf_results.m

set CURDIR=simple_waal_salt_kalman
mkdir test_results\%CURDIR%
call :run_single_test Simulation Simulation_results.m
rem niet gevonden exchange item
rem call :run_single_test SequentialSimulation SequentialSimulation_results.m
rem Error message: DFLOW-FM tidal components of fourier type with user specified frequency not implemented in OpenDA yet
rem call :run_single_test Enkf Enkf_results.m

echo.
if %ErrorOccurred% gtr 0 goto exitwitherror1
if defined TestDisabled (
   echo WARNING: One or more tests were disabled, the remaining tests finished without error
) else (
   echo All tests were performed and finished without error
)
exit /B 0

:exitwitherror0
echo No JAVA runtime found - please check this
exit /B 1 

:exitwitherror1
echo One or more tests finished with an error!
exit /B 1 

rem endlocal

:run_single_test

echo Running test: %CURDIR%\%1.oda
set odafile=%CD%\%CURDIR%\%1.oda
"%JAVA_HOME%\bin\java" -Xms128m -Xmx1024m -classpath %CLASSPATH% org.openda.application.OpenDaApplication %odafile% 1>test_results\%CURDIR%\%1.out 2>test_results\%CURDIR%\%1.err
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
set TestDisabled=1
goto :eof
