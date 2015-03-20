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
 * Tests for utility class FileCopier.
 *
 * @author Arno Kockx
 */
public class FileCopierTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(FileCopierTest.class, "core");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testCopyFile() {
        String sourceFilePath = "fileCopierTest/original/original.txt";
        String destinationFilePath = "fileCopierTest/copy.txt";
        File destinationFile = new File(testRunDataDir, destinationFilePath);

        assertFalse(destinationFile.exists());

        FileCopier fileCopier = new FileCopier();
        fileCopier.initialize(testRunDataDir, new String[]{sourceFilePath, destinationFilePath});

        assertTrue(destinationFile.exists());
    }
}
