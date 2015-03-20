@echo off
set OPENDA_BINDIR=%~dp0
@echo OPENDA_BINDIR set to %OPENDA_BINDIR%
set PATH=%~dp0;%PATH%
@echo PATH set to %PATH%
call "C:\Program Files\JetBrains\IntelliJ IDEA Community Edition 10.5.2\bin\idea.exe" %~dp0..\openda.ipr