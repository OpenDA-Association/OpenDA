@echo off
rem   MOD_V2.0
rem   Copyright (c) 2012 OpenDA Association
rem   All rights reserved.
rem   
rem   This file is part of OpenDA. 
rem   
rem   OpenDA is free software: you can redistribute it and/or modify 
rem   it under the terms of the GNU Lesser General Public License as 
rem   published by the Free Software Foundation, either version 3 of 
rem   the License, or (at your option) any later version. 
rem   
rem   OpenDA is distributed in the hope that it will be useful, 
rem   but WITHOUT ANY WARRANTY; without even the implied warranty of 
rem   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
rem   GNU Lesser General Public License for more details. 
rem   
rem   You should have received a copy of the GNU Lesser General Public License
rem   along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.


rem set variables (compiler, .Net DLL dir., java resources dir, module names)

set IKVMSTUB=..\ikvm\bin\ikvmstub.exe

set DLL_DIR=..\..\dotnet\src\OpenDA.DotNet.Bridge\bin\Debug
set JAR_DIR=..\..\java\resources

set INTERFACES=OpenDA.DotNet.Interfaces
set BRIDGE=OpenDA.DotNet.Bridge


rem clean up

del %JAR_DIR%\%INTERFACES%.jar
del %JAR_DIR%\%BRIDGE%.jar


rem perform compilation

%IKVMSTUB% %DLL_DIR%\%INTERFACES%.dll
%IKVMSTUB% %DLL_DIR%\%BRIDGE%.dll


rem move results to resources dir

copy %INTERFACES%.jar %JAR_DIR%
copy %BRIDGE%.jar %JAR_DIR%
del %INTERFACES%.jar
del %BRIDGE%.jar
