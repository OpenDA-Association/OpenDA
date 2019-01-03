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
//import sun.nio.cs.ext.EUC_TW;

/**
 * Test for d3d-flow in black box wrapper
 */
public class D3dBlackBoxTest extends TestCase {

    OpenDaTestSupport testData = null;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(D3dBlackBoxTest.class,"model_delft3d");
    }

    public static void testDummy() {
        // No action. Test only exist to avoid warnings on empty test class when
        //            the test below is de-activated by renaming it to tst...()
    }

    public void tstF34NoActualComputation() {
    	String configFileName=null;
    	if (!BBUtils.RUNNING_ON_WINDOWS) {
    		if(BBUtils.RUNNING_ON_64bit){
    			configFileName="d3dDudOpenDaConfig_linux64_gnu.xml";
    		}else{
    			//no testing on linux 32-bit
    			return;
    		}
    	}else{
    		configFileName="d3dDudOpenDaConfig.xml";
    	}
        File applicationConfigDir = new File(testData.getTestRunDataDir(), "bb_test_1");
        File applicationConfigFile = new File(applicationConfigDir, configFileName);
        ApplicationRunner.setRunningInTest(true);
        OpenDaApplication.main(new String[]{applicationConfigFile.getAbsolutePath()});
    }

    public void tstWithMissingObservedValues() {
    	String configFileName=null;
    	if (!BBUtils.RUNNING_ON_WINDOWS) {
    		if(BBUtils.RUNNING_ON_64bit){
    			configFileName="d3dDudConfigWithErrors_linux64_gnu.xml";
    		}else{
    			//no testing on linux 32-bit
    			return;
    		}
    	}else{
    		configFileName="d3dDudConfigWithErrors.xml";
    	}
        File applicationConfigDir = new File(testData.getTestRunDataDir(), "bb_test_1");
        File applicationConfigFile = new File(applicationConfigDir, configFileName);
        ApplicationRunner.setRunningInTest(true);
        try {
            OpenDaApplication.main(new String[]{applicationConfigFile.getAbsolutePath()});
        } catch (RuntimeException e) {
            String message = e.getMessage();
            assertEquals("Expected exception",message,"Error, see exception thrown by ApplicationRunner)");
            // TODO: get real exception level
//            assertTrue("Expected exception, 1",
//                    message.equals("Error(s) in getting observed values from black box model"));
//            assertTrue("Expected exception, 2",
//                    message.equals("No computed values available for W3.water level, 199008042345, 199008060115"));
//            assertTrue("Expected exception, 3",
//                    message.equals("No computed values available for W4.water level, 199008050131, 199008050317, 199008050732, 199008051718, 199008052019"));
        }
    }
}
