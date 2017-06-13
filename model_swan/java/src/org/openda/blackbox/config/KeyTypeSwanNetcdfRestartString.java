/* OpenDA v2.4 
* Copyright (c) 2017 OpenDA Association 
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

package org.openda.blackbox.config;

import java.io.File;

/**
 * Class implementation of SwanRestartString KeyType
 */
public class KeyTypeSwanNetcdfRestartString extends KeyTypeString {
    static String RESTART_PREFIX = "INITIAL HOTSTART";
    static String RESTART_POSTFIX=" NETCDF";
    
    public String getValueAsString(String value) {
        return value;
    }

    public String parseRestartFile(File restartFile, String restartFileName) {
        if (restartFileName == null || "".equals(restartFileName) || restartFile == null || !restartFile.exists()) return "";
        String restartContent = "";
        restartContent += RESTART_PREFIX + " '" + restartFileName + "'"+RESTART_POSTFIX;
        return restartContent;

    }
}
