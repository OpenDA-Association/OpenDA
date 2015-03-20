/* MOD_V2.0
* Copyright (c) 2012 OpenDA Association
* All rights reserved.
* 
* This file is part of OpenDA. 
* 
* OpenDA is free software: you can redistribute it and/or modify 
* it under the terms of the GNU Lesser General Public License as 
* published by the Free Software Foundation, either version 3 of 
* the License, or (at your option) any later version. 
* 
* OpenDA is distributed in the hope that it will be useful, 
* but WITHOUT ANY WARRANTY; without even the implied warranty of 
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
* GNU Lesser General Public License for more details. 
* 
* You should have received a copy of the GNU Lesser General Public License
* along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.
*/


This directory contains the scripts to translate the openda code to dotnet,
and to translate dotnet openda models code to java.

Perform the following steps:
- Build the OpenDA.DotNet solution (see ../dotnet/src)
- Build OpenDA (target "build" in build_incl_dotnet_bridge.xml
- Go to the directory j2net, and run make_openda_dll.bat
  (This will produce fullOpenDa.dll, to be referenced by dotnet projects.)
- Rebuild the OpenDA.DotNet solution (see ../dotnet/src)

If you have made change to the OpenDA.DotNet.Bridge and/or
the OpenDA.DotNet.Interfaces code, perform the following
additional steps:
- Go to the directory net2j, and run make_java_stubs.bat
  (This will produce OpenDA.DotNet.Bridge.jar and OpenDA.DotNet.Interfaces.jar,
   that are referenced by the java part of the bridge.)
- Build OpenDA (target "build" in build_incl_dotnet_bridge.xml
- Go to the directory j2net, and run make_openda_dll.bat
- Rebuild the OpenDA.DotNet solution (see ../dotnet/src)

If the new fullOpenDa.dll has been built on another machine with the same directory
structure, and you want to be able to debug, you can use the
update_openda_sources.bat command to take care that your sources (referenced by the
pdb) are put on the right place.

