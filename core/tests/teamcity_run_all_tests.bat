@echo off
setlocal enabledelayedexpansion

REM Runs all tests in the tests directories one by one, and puts the output and 
REM results in a single directory test_results
REM Note: this test assumes that on teamcity the required binaries and jar files have been build
REM           and that these binaries and jars have been copied to core\bin

REM set OPENDA_BINDIR, PATH and CLASSPATH
cd ..\bin
set OPENDA_BINDIR=%CD%
set Path=%CD%;%Path%

rem no openda jre is available, check if there is a default one
if "%JAVA_HOME%" == "" goto exitwitherror0

set CLASSPATH=%OPENDA_BINDIR%\*

cd ..\tests
if exist test_results rd /s/q test_results
mkdir test_results

echo.

set CURDIR=native_oscillator
mkdir test_results\%CURDIR%
call :run_single_test enkf_javaobs       enkf_results.m
call :run_single_test enkf_sqlobs    enkf_sqlobs_restults.m
call :run_single_test simplex       simplex_results.m

REM Only deactivate this part if the correct version of MPICH2 is installed. 
REM ----
REM since the tests in native_parallel work with a special script to start
REM a parallel run, the approach is quite different here

REM Set environment variable %MPI_DIR% 
REM set MPIDIR=/installation_directory_mpich2

REM set CURDIR=native_parallel
REM mkdir test_results\%CURDIR%
REM echo thistextmakesthebatchfilecontinuewithoutpausing >tempfile.txt

REM echo Running test: %CURDIR%\masterworker\run.bat
REM cd %CURDIR%\masterworker
REM call run.bat 1>..\..\test_results\%CURDIR%\PolluteEnKFOpenDaConfig_mw_results.out 2>..\..\test_results\%CURDIR%\PolluteEnKFOpenDaConfig_mw_results.err <..\..\tempfile.txt
REM copy EnKF-results.txt ..\..\test_results\%CURDIR%\PolluteEnKFOpenDaConfig_mw_results.txt >nul
REM echo ***See log files for possible errors

REM echo Running test: %CURDIR%\workerworker\run.bat
REM cd ..\workerworker
REM call run.bat 1>..\..\test_results\%CURDIR%\PolluteEnKFOpenDaConfig_ww_results.out 2>..\..\test_results\%CURDIR%\PolluteEnKFOpenDaConfig_ww_results.err <..\..\tempfile.txt
REM copy EnKF-results.txt ..\..\test_results\%CURDIR%\PolluteEnKFOpenDaConfig_ww_results.txt >nul
REM echo ***See log files for possible errors
REM cd ..\..\
REM del tempfile.txt
REM ----

set CURDIR=simple_lorenz
mkdir test_results\%CURDIR%
call :run_single_test lorenzEnkf               enkf_results.m 
call :run_single_test lorenzEnkf_write_restart enkf_results_write_restart.m
call :run_single_test lorenzEnsr               ensr_results.m
call :run_single_test lorenzRRF                particle_filter_results.m
call :run_single_test lorenzSimulation         simulation_results.m

set CURDIR=simple_lorenz_transformed_observations
mkdir test_results\%CURDIR%
call :run_single_test DudEnkf        dudenkf_results.m
call :run_single_test Enkf           enkf_results.m
call :run_single_test Ensr           ensr_results.m
call :run_single_test ParticleFilter particle_filter_results.m
call :run_single_test Simulation     simulation_results.m

set CURDIR=simple_lorenz96
mkdir test_results\%CURDIR%
call :run_single_test Enkf           enkf_results.m
call :run_single_test Ensr           ensr_results.m
call :run_single_test ParticleFilter particle_filter_results.m
call :run_single_test Simulation     simulation_results.m
call :run_single_test ThreeDVar      threedvar_results.m

set CURDIR=simple_oscillator
mkdir test_results\%CURDIR%
call :run_single_test BFGS                  bfgs_results.m
call :run_single_test ConGrad               congrad_results.m
call :run_single_test Dud                   dud_results.m
call :run_single_test DudEnkf               dudenkf_results.m
call :donotrun_single_test Dudensr               dudensr_results.m
call :run_single_test Enkf                  enkf_results.m
call :run_single_test Enkf_async_generate_gain enkf_async_generate_gain_results.m
call :run_single_test Enkf_fixedAnalysis    enkf_fixed_results.m
call :run_single_test Enkf_generate_gain    enkf_generate_gain_results.m
call :run_single_test Enkf_missing_obs      enkf_missingdata_results.m
call :run_single_test Enkf_write_restart    enkf_write_restart_results.m
call :run_single_test Enkf_startfrom_restart enkf_read_restart_results.m
call :run_single_test Ensr                  ensr_results.m
call :run_single_test Ensr_fixedAnalysis    ensr_fixed_results.m
call :run_single_test GriddedFullSearch     gfs_results.m
call :run_single_test ParticleFilter        particle_filter_results.m
call :run_single_test ParticleFilter_fixedAnalysis particle_filter_fixed_results.m
call :run_single_test Powell                powell_results.m
call :run_single_test PowellWithConstraint  powell_constraint_results.m
call :run_single_test SequentialEnsembleSimulation sequentialEnsembleSimulation_results.m
call :run_single_test SequentialSimulation sequentialSimulation_results.m
call :run_single_test SequentialSimulation_fixedAnalysisTimes sequentialSimulation_fixedAnalysisTimes_results.m
call :run_single_test Simplex               simplex_results.m
call :run_single_test SimplexWithConstraint simplex_constraint_results.m
call :run_single_test Simulation            simulation_results.m
call :run_single_test Steadystate           steadystate_results.m
call :run_single_test Steadystate_async     steadystate_async_results.m

set CURDIR=simple_resultwriters
mkdir test_results\%CURDIR%
call :run_single_test DudMultipleResultwriters  results_dud.m results_dud.csv results_dud_.nc
call :run_single_test DudWritersWithSelections  results_dud_algorithm.m results_dud_model.m results_dud_observer.m results_dud_other.m results_dud_costTotal_only.m
call :run_single_test EnsrMultipleResultwriters results_ensr.m results_ensr_.nc

set CURDIR=simple_two_oscillators
mkdir test_results\%CURDIR%
call :run_single_test Dud                    dud_results.m
call :run_single_test Dud1                   dud1_results.m 
call :run_single_test Dud2                   dud2_results.m
call :run_single_test DudWithConstraint      dud_constraint_results.m
call :donotrun_single_test Enkf                   enkf_results.m
call :donotrun_single_test Ensr                   ensr_results.m
call :run_single_test GriddedFullSearch      gfs_results.m  
call :donotrun_single_test ParticleFilter         particle_filter_results.m
call :run_single_test Powell                 powell_results.m
call :run_single_test PowellWithConstraint   powell_constraint_results.m
call :run_single_test Simplex                simplex_results.m
call :run_single_test SimplexWithConstraint  simplex_constraint_results.m
call :run_single_test Simulation             simulation_results.m  
call :run_single_test Simulation1            simulation1_results.m
call :run_single_test Simulation2            simulation1_results.m

echo.
if defined ErrorOccurred goto exitwitherror1
if defined TestDisabled (
   echo WARNING: One or more tests were disabled, the remaining tests finished without error
) else (
   echo All tests were performed and finished without error
)
exit 0

:exitwitherror0
echo No JAVA_HOME found - please check this
exit 1

:exitwitherror1
echo One or more tests finished with an error!
exit 1

endlocal

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
type test_results\%CURDIR%\%1.err
set ErrorOccurred=1
goto :eof

:donotrun_single_test

echo ***Warning: %CURDIR%\%1.oda test is disabled
set TestDisabled=1
goto :eof

:eof
