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
