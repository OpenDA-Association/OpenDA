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
package org.openda.model_delft3d;
import junit.framework.TestCase;
import org.openda.application.ApplicationRunner;
import org.openda.application.OpenDaApplication;
import org.openda.blackbox.config.BBUtils;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Tests for D3D flow black box wrapper wind field subselection classes
 */
public class D3dWindCalibrationTest extends TestCase {


	private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(D3dWindCalibrationTest.class,"model_delft3d");

        testRunDataDir = new File(testData.getTestRunDataDir(), "test_4");

    }

	public static void testDummy() {
		// No action. Test only exists to avoid warnings on empty test class when
		//            the test below is de-activated by renaming it to tst...()
	}

	public void tstDudD3d() throws IOException {
    	String configFileName=null;
    	if (!BBUtils.RUNNING_ON_WINDOWS) {
    		if(BBUtils.RUNNING_ON_64bit){
    			configFileName="kalib04-15_linux64_gnu.oda";
    		}else{
    			//no testing on linux 32-bit
    			return;
    		}
    	}else{
    		configFileName="kalib04-15.oda";
    	}

    	String inputFile  = ( new File(testRunDataDir,configFileName)).getAbsolutePath();
    	System.out.println("path ="+inputFile);
    	String args[] = new String[1];
    	args[0] = inputFile;
		ApplicationRunner.setRunningInTest(true);
		OpenDaApplication.main(args);

		//TODO: check the output files?

		 // check that the Dud-results are as expected
        File windFile_reload = new File(testRunDataDir,"results_dud.csv" );

        File ref_windFile = new File(testRunDataDir, "ref_results_dud.csv");
      //  assertTrue(testData.FilesAreIdentical(windFile_reload, ref_windFile));


		System.out.println("done?");
    }


}
