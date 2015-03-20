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
package org.openda.tools;
import java.io.File;
import java.io.IOException;

import org.openda.utils.OpenDaTestSupport;

import junit.framework.TestCase;

public class GenerateXmlDocTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(GenerateXmlDocTest.class,"core");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public static void testDummy() {
		// No action. Test only exist to avoid warnings on empty test class when
		//            the test below is de-activated by renaming it to tst...()
	}

	public void tstGenerateOneFile() {
		//
		// Test does not need to be part of the unit tests. It is meant for
		// developing/debugging when working on the documenation generator
		//
		System.out.println("==============================================================================");
		System.out.println("Generate html Documentation of XML schema");
		System.out.println("==============================================================================");

		File dest = new File(testRunDataDir,"openDaApplication.html");
		File dest_ref = new File(testRunDataDir,"openDaApplication.html_ref");
    	String sourceDir  = testRunDataDir.getAbsolutePath();
    	String destDir  = testRunDataDir.getAbsolutePath();
    	String args[] = new String[4];
    	args[0] = "-s";
    	args[1] = sourceDir;
    	args[2] = "-d";
    	args[3] = destDir;
        GenerateXmlDoc.main(args);
		boolean filesEqual = testData.FilesAreIdentical(dest, dest_ref);
		assertTrue("generated openDaApplication.html equals reference", filesEqual);
		
		/**
		 * For some reason, The test is (or should be, in YOUR case) successful but nevertheless 
		 * the library AltovaAutomation.dll gives an exception access violation. This is some 
		 * sort of interaction between the said library and Junit, and should not be worried about.
		 */
	
}

}
