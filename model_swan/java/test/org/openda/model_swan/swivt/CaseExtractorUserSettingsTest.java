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

import junit.framework.TestCase;

/**
 * Test for case extractor user settings file
 */
public class CaseExtractorUserSettingsTest extends TestCase {

    public void testCaseExtractorUserSettings() {

        CaseExtractorUserSettings caseExtractorUserSettings1 = new CaseExtractorUserSettings(true);

        caseExtractorUserSettings1.setSwivtCaseDirPath("aaa");
        caseExtractorUserSettings1.setCalibrationParentDir("PPP");
        caseExtractorUserSettings1.setWindowsSwanExePath("bbb");
        caseExtractorUserSettings1.setStoreWindowsSwanExeRelative(true);
        caseExtractorUserSettings1.setLinuxSwanCalBinDir("ddd");
        caseExtractorUserSettings1.setStorelinuxSwanBinDirRelative(false);
        caseExtractorUserSettings1.setLinuxSequentialSwanExe("fff");
        caseExtractorUserSettings1.setLinuxParallelSwanExe("ggg");
        caseExtractorUserSettings1.setGuiWidth(777);
        caseExtractorUserSettings1.setGuiHeight(77);

        CaseExtractorUserSettings caseExtractorUserSettings2 = new CaseExtractorUserSettings(true);

        assertEquals("getSwivtCaseDirPath", caseExtractorUserSettings2.getSwivtCaseDirPath(), "aaa");
        assertEquals("getCalibrationParentDir", caseExtractorUserSettings2.getCalibrationParentDir(), "PPP");
        assertEquals("getWindowsSwanExePath", "bbb", caseExtractorUserSettings2.getWindowsSwanExePath());
        assertEquals("doStoreWindowsSwanExeRelative", true, caseExtractorUserSettings2.doStoreWindowsSwanExeRelative());
        assertEquals("getLinuxSwanCalBinDir", "ddd", caseExtractorUserSettings2.getLinuxSwanCalBinDir());
        assertEquals("doStorelinuxSwanBinDirRelative", false, caseExtractorUserSettings2.doStorelinuxSwanBinDirRelative());
        assertEquals("getLinuxSequentialSwanExe", "fff", caseExtractorUserSettings2.getLinuxSequentialSwanExe());
        assertEquals("getLinuxParallelSwanExe", "ggg", caseExtractorUserSettings2.getLinuxParallelSwanExe());
        assertEquals("getGuiWidth", 777, caseExtractorUserSettings2.getGuiWidth());
        assertEquals("setGuiHeight", 77, caseExtractorUserSettings2.getGuiHeight());
    }
}
