@echo off
setlocal enabledelayedexpansion

REM Runs all tests in the tests directories one by one, and puts the output and 
REM results in a single directory test_results

REM set OPENDA_BINDIR, PATH and CLASSPATH
cd ..\..\bin
set OPENDA_BINDIR=%CD%
set PATH=%CD%;%PATH%

REM ==== check if jre available as distributed with openda ====
set OPENDA_JRE=%OPENDA_BINDIR%..\jre
if not exist "%OPENDA_JRE%\bin\java.exe" goto else
rem openda jre is available
set JAVA_HOME=%OPENDA_JRE%
goto endif
:else
rem no openda jre is available, check if there is a default one
if "%JAVA_HOME%" == "" goto exitwitherror0
:endif

set CLASSPATH=%OPENDA_BINDIR%\*

cd ..\model_swan\tests
if exist test_results rd /s/q test_results
mkdir test_results

echo.

set CURDIR=s00c2
mkdir test_results\%CURDIR%
call :run_single_test SwanEnKF             EnKF_results.m
call :run_single_test SwanSimulation       simulation_results.m

set CURDIR=swan_l21triad
mkdir test_results\%CURDIR%
call :run_single_test swanSimulation             results_simulation.m
call :run_single_test swanSimplex                results_simplex.m
call :run_single_test swanPowell                 results_powell.m    
call :run_single_test swanDud                    results_dud.m
call :run_single_test swanGriddedFullSearch      results_griddedFullSearch.m
call :run_single_test swanSimplexWithConstr      results_simplexWithConstr.m
call :run_single_test swanPowellWithConstr       results_powellWithConstr.m
call :run_single_test swanDudWithConstr          results_dudWithConstr.m

set CURDIR=swan_l21triad_two_twin
mkdir test_results\%CURDIR%
call :run_single_test Dud1                         results_dud1.m
call :run_single_test Dud2                         results_dud2.m
call :run_single_test Dud_combined12               results_dud.m
call :run_single_test Simplex_combined12           results_simplex.m
call :run_single_test Powell_combined12            results_powell.m
call :run_single_test GriddedFullSearch_combined12 results_griddedfullsearch.m
call :run_single_test DudWithConstraint_combined12     results_dudWithConstr.m
call :run_single_test SimplexWithConstraint_combined12 results_simplexWithConstr.m
call :run_single_test PowellWithConstraint_combined12  results_powellWithConstr.m

set CURDIR=kalman_twin_windbound
mkdir test_results\%CURDIR%
call :run_single_test simulation                   simulation_results.m
call :run_single_test enkf_wind_bound              enkf_windbound_results.m

echo.
if defined ErrorOccurred goto exitwitherror1
if defined TestDisabled (
   echo WARNING: One or more tests were disabled, the remaining tests finished without error
) else (
   echo All tests were performed and finished without error
)
exit 0

:exitwitherror0
echo No JAVA runtime found - please check this
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
set ErrorOccurred=1
goto :eof

:donotrun_single_test

echo ***Warning: %CURDIR%\%1.oda test is disabled
set TestDisabled=1
goto :eof
