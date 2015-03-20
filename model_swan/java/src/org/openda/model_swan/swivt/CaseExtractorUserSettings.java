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

package org.openda.model_swan.swivt;

import org.openda.blackbox.config.BBUtils;

import java.io.*;

/**
 * Settings for openda user (last visited application directory, etc.)
 */
public class CaseExtractorUserSettings {

    private String userSettingsFileName = "swanCaseExtractorUserSettings";

    private final String swivtCaseDirString = "swivtCaseDir=";
    private final String calibrationParentDirString = "calibrationParentDir=";
    private final String windowsSwanExeString = "windowsSwanExe=";
    private final String storeWindowsSwanExeRelativeString = "storeWindowsSwanExeRelative=";
    private final String linuxSwanCalBinDirString = "linuxSwanCalBinDir=";
    private final String storelinuxSwanBinDirRelativeString = "storelinuxSwanBinDirRelative=";
    private final String linuxSwanExeString = "linuxSequentialSwanExe=";
    private final String linuxParallelSwanExeString = "linuxParallelSwanExe=";
    private final String guiWidthString = "guiWidth=";
    private final String guiHeightString = "guiHeight=";

    private String swivtCaseDirPath = null;
    private String windowsSwanExePath = null;
    private boolean storeWindowsSwanExeRelative = false;
    private String linuxSwanCalBinDir = null;
    private boolean storelinuxSwanBinDirRelative = false;
    private String linuxSequentialSwanExe = null;
    private String linuxParallelSwanExe = null;
    private String calibrationParentDir;
    private int guiWidth = 800;
    private int guiHeight = 300;

    protected CaseExtractorUserSettings() {
        this(false);
    }

    protected CaseExtractorUserSettings(boolean runningInTestMode) {
        if (runningInTestMode) {
            userSettingsFileName += "_test";
        }
        readUserSettings();
    }

    public String getSwivtCaseDirPath() {
        return swivtCaseDirPath;
    }

    public void setSwivtCaseDirPath(String swivtCaseDirPath) {
        this.swivtCaseDirPath = swivtCaseDirPath;
        writeUserSettings();
    }

    public String getCalibrationParentDir() {
        return calibrationParentDir;
    }

    public void setCalibrationParentDir(String calibrationParentDir) {
        this.calibrationParentDir = calibrationParentDir;
        writeUserSettings();
     }

    public String getWindowsSwanExePath() {
        return windowsSwanExePath;
    }

    public void setWindowsSwanExePath(String windowsSwanExePath) {
        this.windowsSwanExePath = windowsSwanExePath;
        writeUserSettings();
    }

    public boolean doStoreWindowsSwanExeRelative() {
        return storeWindowsSwanExeRelative;
    }

    public void setStoreWindowsSwanExeRelative(boolean storeWindowsSwanExeRelative) {
        this.storeWindowsSwanExeRelative = storeWindowsSwanExeRelative;
        writeUserSettings();
    }

    public String getLinuxSwanCalBinDir() {
        return linuxSwanCalBinDir;
    }

    public void setLinuxSwanCalBinDir(String linuxSwanCalBinDir) {
        this.linuxSwanCalBinDir = linuxSwanCalBinDir;
        writeUserSettings();
    }

    public boolean doStorelinuxSwanBinDirRelative() {
        return storelinuxSwanBinDirRelative;
    }

    public void setStorelinuxSwanBinDirRelative(boolean storelinuxSwanBinDirRelative) {
        this.storelinuxSwanBinDirRelative = storelinuxSwanBinDirRelative;
        writeUserSettings();
    }

    public String getLinuxSequentialSwanExe() {
        return linuxSequentialSwanExe;
    }

    public void setLinuxSequentialSwanExe(String linuxSequentialSwanExe) {
        this.linuxSequentialSwanExe = linuxSequentialSwanExe;
        writeUserSettings();
    }

    public String getLinuxParallelSwanExe() {
        return linuxParallelSwanExe;
    }

    public void setLinuxParallelSwanExe(String linuxParallelSwanExe) {
        this.linuxParallelSwanExe = linuxParallelSwanExe;
        writeUserSettings();
    }

    public int getGuiWidth() {
        return guiWidth;
    }

    public void setGuiWidth(int guiWidth) {
        this.guiWidth = guiWidth;
        writeUserSettings();
    }

    public int getGuiHeight() {
        return guiHeight;
    }

    public void setGuiHeight(int guiHeight) {
        this.guiHeight = guiHeight;
        writeUserSettings();
    }

    private void writeUserSettings() {
        File caseExtrUserSettingsFile = BBUtils.getFileOnOpenDaTempDir(userSettingsFileName);
        if (caseExtrUserSettingsFile != null) {
            try {
                BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(caseExtrUserSettingsFile));

                bufferedWriter.write(swivtCaseDirString + swivtCaseDirPath);
                bufferedWriter.newLine();
                bufferedWriter.write(calibrationParentDirString + calibrationParentDir);
                bufferedWriter.newLine();
                bufferedWriter.write(windowsSwanExeString + windowsSwanExePath);
                bufferedWriter.newLine();
                bufferedWriter.write(storeWindowsSwanExeRelativeString + storeWindowsSwanExeRelative);
                bufferedWriter.newLine();
                bufferedWriter.write(linuxSwanCalBinDirString + linuxSwanCalBinDir);
                bufferedWriter.newLine();
                bufferedWriter.write(storelinuxSwanBinDirRelativeString + storelinuxSwanBinDirRelative);
                bufferedWriter.newLine();
                bufferedWriter.write(linuxSwanExeString + linuxSequentialSwanExe);
                bufferedWriter.newLine();
                bufferedWriter.write(linuxParallelSwanExeString + linuxParallelSwanExe);
                bufferedWriter.newLine();
                bufferedWriter.write(guiWidthString + guiWidth);
                bufferedWriter.newLine();
                bufferedWriter.write(guiHeightString + guiHeight);
                bufferedWriter.newLine();

                bufferedWriter.close();
            } catch (IOException e) {
                // no action, user settings not stored
            }
        }
    }

    private void readUserSettings() {
        File caseExtrUserSettingsFile = BBUtils.getFileOnOpenDaTempDir(userSettingsFileName);
        if (caseExtrUserSettingsFile != null && caseExtrUserSettingsFile.exists()) {
            try {
                BufferedReader inputFileBufferedReader = new BufferedReader(new FileReader(caseExtrUserSettingsFile));

                String line;
                while ((line = inputFileBufferedReader.readLine())!=null) {
                    if (line.contains(swivtCaseDirString)) {
                        swivtCaseDirPath = line.substring(swivtCaseDirString.length());
                    }
                    else if (line.contains(calibrationParentDirString)) {
                        calibrationParentDir = line.substring(calibrationParentDirString.length());
                    }
                    else if (line.contains(windowsSwanExeString)) {
                windowsSwanExePath = line.substring(windowsSwanExeString.length());
                    }
                    else if (line.contains(storeWindowsSwanExeRelativeString)) {
                        storeWindowsSwanExeRelative = Boolean.valueOf(
                                line.substring(storeWindowsSwanExeRelativeString.length()));
                    }
                    else if (line.contains(linuxSwanCalBinDirString)) {
                        linuxSwanCalBinDir =line.substring(linuxSwanCalBinDirString.length());
                    }
                    else if (line.contains(storelinuxSwanBinDirRelativeString)) {
                        storelinuxSwanBinDirRelative = Boolean.valueOf(
                                line.substring(storelinuxSwanBinDirRelativeString.length()));
                    }
                    else if (line.contains(linuxSwanExeString)) {
                linuxSequentialSwanExe = line.substring(linuxSwanExeString.length());
                    }
                    else if (line.contains(linuxParallelSwanExeString)) {
                linuxParallelSwanExe = line.substring(linuxParallelSwanExeString.length());
                    }
                    else if (line.contains(guiWidthString)) {
                        guiWidth = Integer.valueOf(line.substring(guiWidthString.length()));
                    }
                    else if (line.contains(guiHeightString)) {
                        guiHeight = Integer.valueOf(line.substring(guiHeightString.length()));
                    }
                }
                inputFileBufferedReader.close();
            } catch (IOException e) {
                // no action, user settings not retrieved
            }
        }
    }
}