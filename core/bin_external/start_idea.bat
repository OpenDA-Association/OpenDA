@echo off

set IDEA="c:\Program Files (x86)\JetBrains\IntelliJ IDEA Community Edition 13.1.3\bin\idea.exe"

set OPENDA_BIN_EXT_DIR=%~dp0
set OPENDA_NATIVE_BIN_PATH=%OPENDA_BIN_EXT_DIR%..\native_bin\win32_ifort

@echo OPENDA-native binaries will be read from %OPENDA_NATIVE_BIN_PATH%

set PATH=%OPENDA_NATIVE_BIN_PATH%;%PATH%

%IDEA%
