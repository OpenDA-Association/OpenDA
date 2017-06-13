/* OpenDA v2.4 
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
package org.openda.application;
import java.io.File;
import java.io.IOException;

import org.openda.utils.OpenDaTestSupport;

import junit.framework.TestCase;

public class TestRunnerTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(ApplicationRunnerTest.class,"application");
        testRunDataDir = testData.getTestRunDataDir();
    }

	public void testTestRunner1() {
		System.out.println("==============================================================================");
		System.out.println("Run mutimple tests");
		System.out.println("==============================================================================");

		File testsDir= new File(new File(testRunDataDir,"simple_oscillator"), "tests");
		String args[] = new String[]{testsDir.getAbsolutePath()};
		
		try {
			TestRunner.main(args);			
		} catch (Exception e) {
			System.out.println("The error is detected correctly.");
		}
		
	}
	
	public void testTestRunner2() {
		System.out.println("==============================================================================");
		System.out.println("Create default config files");
		System.out.println("==============================================================================");

		String args[] = new String[]{"-c",testRunDataDir.getAbsolutePath()};
		TestRunner.main(args);
		
		File someTstDir = new File(new File(testRunDataDir,"simple_oscillator"),"tests");
		File someTstFile = new File(someTstDir,"Enkf_missing_obs.tst");
		assertTrue(someTstFile.exists());
		assertTrue(testData.FileContains(someTstFile, "<check>"));
		
	}
	
	public void testTestRunner3() {
		System.out.println("==============================================================================");
		System.out.println("Run test with multiple steps");
		System.out.println("==============================================================================");

		File testFile = new File(new File(testRunDataDir,"simple_oscillator"),"Steadystate.tst");
		String args[] = new String[]{testFile.getAbsolutePath()};
		TestRunner.main(args);
		
	}


}
