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


package org.openda.utils;

import java.io.*;

/**
 * Read info from unitTestInfoFile and provide the values as methods.
 */
public class UnitTestInfo {

    private String moduleName = null;
    private File unitTestsRootDir;

    public UnitTestInfo(File unitTestInfoFile) {
        try {
            BufferedReader inputFileBufferedReader = new BufferedReader(new FileReader(unitTestInfoFile));
            String line = inputFileBufferedReader.readLine();
            int lineNr=0;
            while (line != null) {
                lineNr++;
                line = line.trim();
                if ( ! line.startsWith("#") && ! (line.length()==0) ) {
                    String fieldSeparators = "=";
                    String[] fields = line.split(fieldSeparators);
                    if ( fields.length != 2 ) {
                        throw new RuntimeException("Invalid syntax in unit test info file, line " +
                                lineNr + ", file: " + unitTestInfoFile.getAbsolutePath());
                    }
                    parseKeyValue(unitTestInfoFile, fields[0].trim().toLowerCase(), fields[1].trim());
                }
                line = inputFileBufferedReader.readLine();
            }
        } catch (FileNotFoundException e) {
            throw new RuntimeException("Could not open unit test info file " +
                    unitTestInfoFile.getAbsolutePath());
        } catch (IOException e) {
            throw new RuntimeException("Could not read fields from unit test info file " +
                    unitTestInfoFile.getAbsolutePath() + " (error:" + e.getMessage() + ")");
        }
    }

    private void parseKeyValue(File unitTestInfoFile, String key, String value) {
        if (key.equals("modulename")) {
            moduleName = value;
        } else if (key.equals("unittestsrootdir")) {
            unitTestsRootDir = new File(unitTestInfoFile.getParent(), value);
            if (!unitTestsRootDir.exists()) {
                throw new RuntimeException("Unit tests root dir " + value + " does not exist (file: " +
                        unitTestInfoFile.getAbsolutePath() + ")");
            }
        }
    }

    public String getModuleName() {
        return moduleName;
    }

    public File getUnitTestsRootDir() {
        return unitTestsRootDir;
    }
}
