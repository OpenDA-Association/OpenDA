@echo off

rem Setup bin. dir for openda.
rem Script has to be run from the dir. containing the script.

set OPENDADIR=%~dp0
set PATH=%~dp0;%PATH%
@echo OPENDADIR set to %OPENDADIR%

set ODASYSTEM=win64_ifort
echo "System set to %ODASYSTEM%"
set ODALIBDIR=%OPENDADIR%%ODASYSTEM%
set PATH=%ODALIBDIR%;%PATH%

rem set OPENDA_BINDIR to OPENDADIR for now to keep things working
set OPENDA_BINDIR=%OPENDADIR%
