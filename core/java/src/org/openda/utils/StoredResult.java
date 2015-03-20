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

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;

public class StoredResult {
    private String directory;
    private String description;
    private boolean addDirectory = false;

    public StoredResult(String directory, String description) {
        this.directory = directory;
        this.description = description;
    }

    public StoredResult(String directory, String description, boolean addDir) {
        this.directory = directory;
        this.description = description;
        this.addDirectory = addDir;
    }


    public StoredResult() {
        this.directory = null;
        this.description = null;
    }

    public String toString() {
        if (!addDirectory) {
            return description;
        } else {
            return description + " -- " + directory;
        }
    }

    public String getDirectory() {
        return directory;
    }

    public static StoredResult[] getStoredResults(File registerFile, boolean addDir) {
        try {
            LineNumberReader reader = new LineNumberReader(new FileReader(registerFile));

            StoredResult[] list = null;
            String line = null;
            String directory = null;
            String description = null;
            int count = 0;
            while ((line = reader.readLine()) != null) {
                if (line.startsWith("Iteration ")) {
                    description = line;
                    count ++;
                    StoredResult[] newList = new StoredResult[count];
                    for (int i = 0; i < count-1; i ++) {
                        newList[i] = list[i];
                    }
                    newList[count-1] = new StoredResult(directory, description, addDir);
                    list = newList;
                } else {
                    directory = line;
                }
            }

            reader.close();
            if (list == null) {
                list = new StoredResult[0];
            }
            return list;
        } catch (IOException e) {
            return new StoredResult[0];
        }
    }
}
