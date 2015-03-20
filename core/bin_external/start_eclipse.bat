@echo off
rem start eclipse development environment with libraries included
rem This wrapper is needed for unit tests that make use of native libraries,
rem that is DLL's.

rem local settings for this computer
call %~dp0\setup_openda.bat

set OPENDA_BIN_EXT_DIR=%~dp0
rem if starting from OPENDADIR ie public/bin
set OPENDA_NATIVE_BIN_PATH=%OPENDA_BIN_EXT_DIR%..\core\native_bin\%ODASYSTEM%
@echo OPENDA add libdir: %OPENDA_NATIVE_BIN_PATH%
set PATH=%OPENDA_NATIVE_BIN_PATH%
rem if starting from public\core\bin_external
set OPENDA_NATIVE_BIN_PATH=%OPENDA_BIN_EXT_DIR%..\native_bin\%ODASYSTEM%
@echo OPENDA add libdir: %OPENDA_NATIVE_BIN_PATH%
set PATH=%OPENDA_NATIVE_BIN_PATH%;%PATH%

set JAVA_HOME=c:\Program Files (x86)\Java\jdk1.6.0_45
set PATH=%JAVA_HOME%\bin;%PATH%

@echo PATH=%PATH%

set ECLIPSE=C:\eclipse\eclipse.exe
if exist %ECLIPSE% goto end
   set ECLIPSE=D:\verlaan\eclipse\eclipse.exe
:end

%ECLIPSE%
