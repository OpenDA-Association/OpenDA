@echo off

rem adjust bindir if batch file should be run from elsewhere

set BIN_DIR=.

rem ==== check availability and arguments ===
if not exist %BIN_DIR%\model_f90dll.jar goto Error1
if not exist %BIN_DIR%\core.jar goto Error1
if not exist %BIN_DIR%\junit-3.8.1.jar goto Error1
if not exist %BIN_DIR%\jna.jar goto Error1

set JARS=%BIN_DIR%\model_f90dll.jar;%BIN_DIR%\core.jar;%BIN_DIR%\junit-3.8.1.jar;%BIN_DIR%\jna.jar
set MAIN_CLASS=org.openda.examples.simplef90model.SimpleModelInstanceTestExe

rem ==== run ===
echo java -classpath %JARS% %MAIN_CLASS% %1 %2
java -classpath %JARS% %MAIN_CLASS% %1 %2
goto End

rem ==== show errors ===

:Error1
echo One or more of the files
echo     %BIN_DIR%\model_f90dll.jar, %BIN_DIR%\core.jar,
echo     %BIN_DIR%\junit-3.8.1, %BIN_DIR%\jna.jar,
echo do not exist
pause
goto End

rem ==== done ===
:End
