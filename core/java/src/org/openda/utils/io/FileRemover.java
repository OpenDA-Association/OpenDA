/* OpenDA v2.4.3 
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
package org.openda.utils.io;

import org.openda.interfaces.IConfigurable;

import java.io.File;

/**
 * Utility class to remove files from an instance directory
 */
public class FileRemover implements IConfigurable {
    
    public void initialize(File workingDir, String[] arguments) {
        if (!workingDir.exists()) {
            throw new RuntimeException("working directory does not exist: " + workingDir.getAbsolutePath());
        }
        for (String argument : arguments) {
            File file = new File(workingDir, argument);
            if (file.exists()) {
                deleteFile(file);
            } else {
                // check if it is a wild card
                if (argument.startsWith("*.")) {
                    String extension = argument.substring(1);
                    File[] listedFiles = workingDir.listFiles();
                    for (File listedfile : listedFiles) {
                        if (listedfile.getName().endsWith(extension)) {
                            deleteFile(listedfile);
                        }
                    }
                }
				else if (argument.endsWith("*.*")) {
					String prefix = argument.substring(0, argument.length()- "*.*".length());
					File[] listedFiles = workingDir.listFiles();
					for (File listedfile : listedFiles) {
						if (listedfile.getName().startsWith(prefix)) {
							deleteFile(listedfile);
						}
					}
				} else if (file.getName().startsWith("*.")){
					String extension = argument.substring(argument.lastIndexOf("."));
					File[] listedFiles = file.getParentFile().listFiles();
					for (File listedfile : listedFiles) {
						if (listedfile.getName().endsWith(extension)) {
							deleteFile(listedfile);
						}
					}
				}
			}
        }
    }

    private void deleteFile(File file) {
        if (file.isDirectory()) {
            throw new RuntimeException("FileRemover will not remove a directory: " + file.getAbsolutePath());
        }
        if (!file.delete()) {
            throw new RuntimeException("Could not remove file: " + file.getAbsolutePath());
        }
    }
}
