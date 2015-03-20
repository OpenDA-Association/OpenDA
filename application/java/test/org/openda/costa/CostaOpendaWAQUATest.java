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

package org.openda.costa;

import junit.framework.TestCase;
import org.openda.application.ApplicationRunner;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * TODO: description
 */
public class CostaOpendaWAQUATest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(CostaOpendaAppTest.class,"application");
        testRunDataDir = testData.getTestRunDataDir();
    }

	public static void testDummy() {
		// No action. Test only exist to avoid warnings on empty test class when
		//            the test below is de-activated by renaming it to tst...()
	}

	// Test with a waqua model: csm8 NEW
     public void tstWaqua_EnKF() throws IOException {
     ApplicationRunner.setRunningInTest(true);
         // the xml file is until now being made by waqpro.pl !
         
     File config  = new File(testRunDataDir,"dcsm-new/WaquaOpenDaConfig4756.xml");
     String args[] = new String[1];
     args[0] = config.getAbsolutePath();
     org.openda.application.OpenDaApplication.main(args);
     }


    // Test with waqua model, with noos-observations:

    public void tstWaqua_EnKFnoos() throws IOException {
    ApplicationRunner.setRunningInTest(true);
        // the xml file is until now being made by waqpro.pl !

    //File config  = new File(testRunDataDir,"csm8_temp_noos.xml");
    //String args[] = new String[1];
    //args[0] = config.getAbsolutePath();
    //org.openda.application.OpenDaApplication.main(args);
    }



}




