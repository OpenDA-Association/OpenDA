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

import junit.framework.TestCase;

public class OpenDATestSupportTest extends TestCase {
	
	public void testTestSupportBasics() {
		OpenDaTestSupport testData = new OpenDaTestSupport(OpenDATestSupportTest.class,"core");
				
		File testRunDataDir = testData.getTestRunDataDir();
		System.out.println("Test run data directory is "+testRunDataDir);
		
		// look for copy of input files
		File inputFile = new File(testRunDataDir,"input.txt");
		assertTrue("Found input file", inputFile.exists());
		
		//check output
		File okRef = new File(testRunDataDir,"input_reference.txt");
		boolean filesEqual = testData.FilesAreIdentical(inputFile, okRef);
		assertTrue("two equal files", filesEqual);
			
		File notOkRef = new File(testRunDataDir,"input_wrong.txt");
		boolean filesNotEqual = testData.FilesAreIdentical(inputFile, notOkRef);
		assertTrue("two different files", !filesNotEqual);
		
		boolean foundOk = testData.FileContains(inputFile, "ok");
		assertTrue("yes ok exists in this file", foundOk);

		boolean foundNotOk = testData.FileContains(inputFile, ".*bubblegum.*");
		assertTrue("not bubblegum does not exist in this file", !foundNotOk);
		
		File HeaderNotOkRef = new File(testRunDataDir,"input_different_first_line.txt");
		boolean HeaderNotEqual = testData.FilesAreIdentical(inputFile, HeaderNotOkRef);
		assertTrue("different 1st line", !HeaderNotEqual);

		File HeaderOkRef = new File(testRunDataDir,"input_different_first_line.txt");
		boolean HeaderEqual = testData.FilesAreIdentical(inputFile, HeaderOkRef,1);
		assertTrue("same (except first line", HeaderEqual);
}

	public void testTestSupportInternals() {
		OpenDaTestSupport testData = new OpenDaTestSupport(OpenDATestSupportTest.class,"core");
		
		File projectRootDir = testData.getProjectRootDir();
		System.out.println("Project root directory is "+projectRootDir);

		File javaTestDir = testData.getJavaTestDir();
		System.out.println("Java test directory is "+javaTestDir);

		File testDataDir = testData.getTestDataDir();
		System.out.println("Test data directory is "+testDataDir);
		
		File testRunDataDir = testData.getTestRunDataDir();
		System.out.println("Test run data directory is "+testRunDataDir);
		
		// look for copy of input files
		File inputFile = new File(testRunDataDir,"input.txt");
		assertTrue("Found input file", inputFile.exists());
			
		File moduleRootDir = testData.getModuleRootDir();
		System.out.println("Module root directory is "+moduleRootDir);
		assertTrue(moduleRootDir.getAbsolutePath().endsWith("core"));

	}

	
}
