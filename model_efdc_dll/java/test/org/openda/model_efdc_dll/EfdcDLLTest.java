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

package org.openda.model_efdc_dll;

import junit.framework.TestCase;
import org.openda.blackbox.config.BBUtils;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;

import java.io.File;
import java.util.TimeZone;

//import org.openda.blackbox.config.BBUtils;

/**
 * Test for the Efdc model dll
 */
public class EfdcDLLTest extends TestCase {

    private OpenDaTestSupport testData;

    protected void setUp() {
    	testData = new OpenDaTestSupport(EfdcDLLTest.class,"model_efdc_dll");
    }

    public void testModelDLL() {

        final int instanceCount = 2;
        EfdcDLL[] modelDLLs = new EfdcDLL[instanceCount];
        
        File moduleRootDir = testData.getModuleRootDir();
        System.out.println(moduleRootDir);
        
        File fortranDll;
        if (EfdcDLL.RUNNING_ON_WINDOWS) {
            fortranDll = new File(moduleRootDir, "native_bin/win" + System.getProperty("sun.arch.data.model") + "_ifort/EfdcFortranDLL.dll");
        } else if (EfdcDLL.RUNNING_ON_MAC){
            System.out.println("native_bin/darwin/lib/libEFDC.dylib");
            fortranDll = new File(moduleRootDir, "native_bin/darwin/lib/libEFDC.dylib");
        }
        else {
            System.out.println("native_bin/linux" + System.getProperty("sun.arch.data.model") + "_gnu/lib/libEFDC.so");
            fortranDll = new File(moduleRootDir, "native_bin/linux" + System.getProperty("sun.arch.data.model") + "_gnu/lib/libEFDC.so");
        }
        File modelTemplateDir = new File(testData.getTestRunDataDir(), "model");
        System.out.println(modelTemplateDir);
        
        // load the native dll
        File modelInstancesParentDir = testData.getTestRunDataDir();
        EfdcDLL.initialize(fortranDll, modelInstancesParentDir, modelTemplateDir, TimeZone.getTimeZone("GMT"));

        // create instance directories and intialize the model instances
        System.out.println("Creating instances");
        for (int i = 0; i < instanceCount; i++) {
            File instanceDir = new File(modelInstancesParentDir, "work" + i);
            BBUtils.makeDirectoryClone(modelTemplateDir, instanceDir);
            modelDLLs[i] = EfdcDLL.getForModelInstance(instanceDir);
        }
        
        // test getting values after init
        System.out.println("Getting values");
        double expected = 5. / 86400.;
        double precission = ( BBUtils.RUNNING_ON_64bit ) ? 1e-15 : 1e-7;
        System.out.println("Testing precission = " +  precission);
        assertEquals("Time Step", expected, modelDLLs[0].getDeltaT(), expected*precission  );
        expected = 55979. + 9./24.;
        assertEquals("Start Time", expected, modelDLLs[0].getStartTime(), expected*precission );
        expected = 55979. + 9./24. + 1.0/24.0/60.0;
        assertEquals("End Time", expected, modelDLLs[0].getEndTime(), expected*precission );
        expected = 60. / 86400.;
        assertEquals("Reference Period",expected, modelDLLs[0].getReferencePeriod(), expected*precission );
        
        // compute instances	
        System.out.println("Computing");
        //compute for one minute.
        Time startTime =  new Time(modelDLLs[0].getStartTime());
        Time endTime = new Time(modelDLLs[0].getStartTime()+ 1.0/24.0/60.0);
        for (int i = 0; i < instanceCount; i++) {
            modelDLLs[i].compute(startTime, endTime);
            assertEquals(endTime.getMJD(), modelDLLs[i].getCurrentTime(), 1e-5);
        }
        
        // cleanup
        System.out.println("Finishing instances");
        for (int i = 0; i < instanceCount; i++) {
            modelDLLs[i].finish();
        }
        EfdcDLL.free();
    }
}
