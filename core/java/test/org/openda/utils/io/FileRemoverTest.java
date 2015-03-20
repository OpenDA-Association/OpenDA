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
package org.openda.utils.io;

import junit.framework.TestCase;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Tests for utility class to remove files from an instance directory
 */
public class FileRemoverTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    private final String fileAaaTxtName = "aaa.txt";
    private final String fileAaaOutName = "aaa.out";
    private final String fileBbbTxtName = "bbb.txt";
    private final String fileBbbOutName = "bbb.out";
    private final String fileAacTxtName = "aac.txt";
    private final String fileAacOutName = "aac.out";
    private final String fileBbcTxtName = "bbc.txt";
    private final String fileBbcOutName = "bbc.out";
    private final String fileBbcLogName = "bbc.log";

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(FileRemoverTest.class,"core");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testFixedFileNames() {

        FileRemover remover = new FileRemover();
        remover.initialize(testRunDataDir, new String[] {fileAaaOutName, fileAacTxtName, fileBbbOutName});

        assertTrue (fileAaaTxtName, new File(testRunDataDir, fileAaaTxtName).exists());
        assertFalse(fileAaaOutName, new File(testRunDataDir, fileAaaOutName).exists());
        assertTrue (fileBbbTxtName, new File(testRunDataDir, fileBbbTxtName).exists());
        assertFalse(fileBbbOutName, new File(testRunDataDir, fileBbbOutName).exists());
        assertFalse(fileAacTxtName, new File(testRunDataDir, fileAacTxtName).exists());
        assertTrue (fileAacOutName, new File(testRunDataDir, fileAacOutName).exists());
        assertTrue (fileBbcTxtName, new File(testRunDataDir, fileBbcTxtName).exists());
        assertTrue (fileBbcOutName, new File(testRunDataDir, fileBbcOutName).exists());
        assertTrue (fileBbcLogName, new File(testRunDataDir, fileBbcLogName).exists());

    }

    public void testWildCards1() {

        FileRemover remover = new FileRemover();
        remover.initialize(testRunDataDir, new String[] {"*.out"});

        assertTrue (fileAaaTxtName, new File(testRunDataDir, fileAaaTxtName).exists());
        assertFalse(fileAaaOutName, new File(testRunDataDir, fileAaaOutName).exists());
        assertTrue (fileBbbTxtName, new File(testRunDataDir, fileBbbTxtName).exists());
        assertFalse(fileBbbOutName, new File(testRunDataDir, fileBbbOutName).exists());
        assertTrue (fileAacTxtName, new File(testRunDataDir, fileAacTxtName).exists());
        assertFalse(fileAacOutName, new File(testRunDataDir, fileAacOutName).exists());
        assertTrue (fileBbcTxtName, new File(testRunDataDir, fileBbcTxtName).exists());
        assertFalse(fileBbcOutName, new File(testRunDataDir, fileBbcOutName).exists());
        assertTrue (fileBbcLogName, new File(testRunDataDir, fileBbcLogName).exists());

    }

    public void testWildCards2() {

        FileRemover remover = new FileRemover();
        remover.initialize(testRunDataDir, new String[] {"*.txt", "*.log" });

        assertFalse(fileAaaTxtName, new File(testRunDataDir, fileAaaTxtName).exists());
        assertTrue (fileAaaOutName, new File(testRunDataDir, fileAaaOutName).exists());
        assertFalse(fileBbbTxtName, new File(testRunDataDir, fileBbbTxtName).exists());
        assertTrue (fileBbbOutName, new File(testRunDataDir, fileBbbOutName).exists());
        assertFalse(fileAacTxtName, new File(testRunDataDir, fileAacTxtName).exists());
        assertTrue (fileAacOutName, new File(testRunDataDir, fileAacOutName).exists());
        assertFalse(fileBbcTxtName, new File(testRunDataDir, fileBbcTxtName).exists());
        assertTrue (fileBbcOutName, new File(testRunDataDir, fileBbcOutName).exists());
        assertFalse(fileBbcLogName, new File(testRunDataDir, fileBbcLogName).exists());

    }

    public void testFixedAndWildCard() {

        FileRemover remover = new FileRemover();
        remover.initialize(testRunDataDir, new String[] {fileAaaOutName, "*.txt" });

        assertFalse(fileAaaTxtName, new File(testRunDataDir, fileAaaTxtName).exists());
        assertFalse(fileAaaOutName, new File(testRunDataDir, fileAaaOutName).exists());
        assertFalse(fileBbbTxtName, new File(testRunDataDir, fileBbbTxtName).exists());
        assertTrue (fileBbbOutName, new File(testRunDataDir, fileBbbOutName).exists());
        assertFalse(fileAacTxtName, new File(testRunDataDir, fileAacTxtName).exists());
        assertTrue (fileAacOutName, new File(testRunDataDir, fileAacOutName).exists());
        assertFalse(fileBbcTxtName, new File(testRunDataDir, fileBbcTxtName).exists());
        assertTrue (fileBbcOutName, new File(testRunDataDir, fileBbcOutName).exists());
        assertTrue (fileBbcLogName, new File(testRunDataDir, fileBbcLogName).exists());
    }
}
