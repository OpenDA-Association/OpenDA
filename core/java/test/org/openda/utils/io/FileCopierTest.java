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
import java.text.ParseException;
import java.text.SimpleDateFormat;

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

    public void testCopyFileWithPrefix() throws ParseException {
        String sourceFilePath = "fileCopierTest/original/original.txt";
        String destinationFileName = "fileCopierTest/target/copy.txt";
        File destinationFile = new File(testRunDataDir, destinationFileName);
        File destinationDirectory = new File(testRunDataDir, "fileCopierTest/target/");

        assertFalse(destinationFile.exists());

        FileCopier fileCopier = new FileCopier();
		long currentTimeMillis = System.currentTimeMillis();
		fileCopier.initialize(testRunDataDir, new String[]{sourceFilePath, destinationFileName, "currentTimePrefix=true"});

		File[] files = destinationDirectory.listFiles();
		assertNotNull(files);
		assertEquals(1, files.length);

		String name = files[0].getName();
		assertTrue(name.endsWith("_copy.txt"));
		assertEquals(23, name.length());

		long timeFromFilename = new SimpleDateFormat("yyyyMMddHHmmss").parse(name.substring(0, 14)).getTime();
		long timeDiff = currentTimeMillis - timeFromFilename;
		assertTrue(timeDiff < 5000);
	}

	public void testCopyFileWithPostfix() throws ParseException {
		String sourceFilePath = "fileCopierTest/original/original.txt";
		String destinationFileName = "fileCopierTest/target/copy.txt";
		File destinationFile = new File(testRunDataDir, destinationFileName);
		File destinationDirectory = new File(testRunDataDir, "fileCopierTest/target/");

		assertFalse(destinationFile.exists());

		FileCopier fileCopier = new FileCopier();
		long currentTimeMillis = System.currentTimeMillis();
		fileCopier.initialize(testRunDataDir, new String[]{sourceFilePath, destinationFileName, "currentTimePrefix=false", "currentTimePostfix=true"});

		File[] files = destinationDirectory.listFiles();
		assertNotNull(files);
		assertEquals(1, files.length);

		String name = files[0].getName();
		assertTrue(name.startsWith("copy_"));
		assertTrue(name.endsWith(".txt"));
		assertEquals(23, name.length());

		long timeFromFilename = new SimpleDateFormat("yyyyMMddHHmmss").parse(name.substring(5, 19)).getTime();
		long timeDiff = Math.abs(currentTimeMillis - timeFromFilename);
		assertTrue(timeDiff < 5000);
	}
}
