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


package org.openda.application.gui;

import org.openda.blackbox.config.BBUtils;

import java.io.*;

/**
 * Settings for openda user (last visited application directory, etc.)
 */
public class OpenDaUserSettings {

    private final String userSettingsFileName = "opendaUserSettings";
    private final String lastDirString = "lastUserDirectory=";
    private File lastUsedDir = null;

    protected OpenDaUserSettings() {
        readUserSettings();
    }

    public File getLastUsedDir() {
        return lastUsedDir;
    }

    public void setLastUsedDir(File directory) {
        if (directory != null && directory.exists() && directory.isDirectory()) {
            lastUsedDir = directory;
            writeUserSettings();
        }
    }

    private void writeUserSettings() {
        File opendaUserSettingsFile = BBUtils.getFileOnOpenDaTempDir(userSettingsFileName);
        if (opendaUserSettingsFile != null) {
            try {
                BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(opendaUserSettingsFile));

                bufferedWriter.write(lastDirString + lastUsedDir.getCanonicalPath());
                bufferedWriter.newLine();

                bufferedWriter.close();
            } catch (IOException e) {
                // no action, user settings not stored
            }
        }
    }

    private void readUserSettings() {
        File opendaUserSettingsFile = BBUtils.getFileOnOpenDaTempDir(userSettingsFileName);
        if (opendaUserSettingsFile != null && opendaUserSettingsFile.exists()) {
            try {
                BufferedReader inputFileBufferedReader = new BufferedReader(new FileReader(opendaUserSettingsFile));

                String lastDirline = inputFileBufferedReader.readLine();
                lastUsedDir = new File(lastDirline.substring(lastDirString.length()));
                if (!lastUsedDir.exists() || !lastUsedDir.isDirectory()) {
                    lastUsedDir = null;
                }

                inputFileBufferedReader.close();
            } catch (IOException e) {
                // no action, temp dir fir user settings not available
            }
        }
    }
}
