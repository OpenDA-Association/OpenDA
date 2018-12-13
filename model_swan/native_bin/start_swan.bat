@echo off
set swandir=%~dp0%
set executable=%swandir%win64\4120A_3_del_w64_i13_omp.exe
set dlldir=%swandir%win64\libraries\
rem
rem add dll's to PATH and start SWAN
rem 
set PATH=%dlldir%;%PATH%
%executable%
goto eof

:eof
exit /B %ERRORLEVEL%