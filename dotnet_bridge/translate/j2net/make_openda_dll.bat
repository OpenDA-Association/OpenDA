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


rem clean up

rmdir /s /q .\src\org\openda
del *.jar


rem copy the generated jar files to this working directory

copy ..\..\..\bin\*.jar


rem copy the sources to create a src-tree with all sources

mkdir .\src\org\openda
xcopy /s ..\..\..\core\java\src\org\openda .\src\org\openda

mkdir .\src\org\openda\observers
mkdir .\src\org\openda\models
mkdir .\src\org\openda\algorithms
mkdir .\src\org\openda\application
mkdir .\src\org\openda\dotnet
xcopy /s ..\..\..\observers\java\src\org\openda\observers .\src\org\openda\observers
xcopy /s ..\..\..\models\java\src\org\openda\models .\src\org\openda\models
xcopy /s ..\..\..\algorithms\java\src\org\openda\algorithms .\src\org\openda\algorithms
xcopy /s ..\..\..\application\java\src\org\openda\application .\src\org\openda\application
xcopy /s ..\..\..\dotnet_bridge\java\src\org\openda\dotnet .\src\org\openda\dotnet


rem set and echo settings for compile action

set IKVMC=..\ikvm\bin\ikvmc.exe -debug
set OUTDIR=-out:..\..\dotnet\src\external\OpenDA_j2net
set SRC_ROOT=.\src
echo SETTINGS:
echo IKVMC: %IKVMC%
echo OUTDIR: %OUTDIR%
echo SRC_ROOT: %SRC_ROOT%

set DLL_DIR=..\..\dotnet\src\OpenDA.DotNet.Bridge\bin\Debug
set INTERFACES=OpenDA.DotNet.Interfaces
set BRIDGE=OpenDA.DotNet.Bridge


rem perform compilation including debug actions

%IKVMC% -srcpath:%SRC_ROOT% -r:%DLL_DIR%\%INTERFACES% -r:%DLL_DIR%\%BRIDGE% alloy.jar ganymed.jar Jama-1.0.2.jar sgt_v30.jar swing-layout-1.0.jar junit-3.8.2.jar xercesImpl.jar xalan.jar castor-0.9.5.jar xom-1.2.6.jar core_castor.jar observers_castor.jar DATools_castor.jar DUE_subset.jar openda_core.jar observers.jar models.jar algorithms.jar application.jar mscorlib.jar OpenDA.DotNet.Bridge.jar OpenDA.DotNet.Interfaces.jar dotnet_bridge.jar %OUTDIR%\fullOpenDa.dll 2>fullOpenDa.txt
