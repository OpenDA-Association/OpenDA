/* OpenDA v2.4.1 
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
package org.openda.model_efdc_dll;

import java.io.File;
import java.util.TimeZone;

import junit.framework.TestCase;

import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IModelInstance;
import org.openda.interfaces.IModelState;
import org.openda.interfaces.ITime;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;

/**
 * Test for the DLL based model instance
 */
public class EfdcRestartTest extends TestCase {

    private OpenDaTestSupport testData;

    protected void setUp() {
    	testData = new OpenDaTestSupport(EfdcRestartTest.class,"model_efdc_dll");
    }

    public void testRestartDllBasedModel() {

        final int instanceCount = 2;
        File modelInstancesParentDir = testData.getTestRunDataDir();
        File moduleRootDir = testData.getModuleRootDir();
        File fortranDll;
        if (EfdcDLL.RUNNING_ON_WINDOWS) {
        	fortranDll = new File(moduleRootDir, "native_bin/win32_ifort/EfdcFortranDLL.dll");
        } else if (EfdcDLL.RUNNING_ON_MAC){
            System.out.println("native_bin/darwin/lib/libEFDC.dylib");
            fortranDll = new File(moduleRootDir, "native_bin/darwin/lib/libEFDC.dylib");
        } else {
            fortranDll = new File(moduleRootDir, "native_bin/linux" + System.getProperty("sun.arch.data.model") + "_gnu/lib/libEFDC.so");
        }
        System.out.println("DLL="+fortranDll.getAbsolutePath());

        runModelInstancesTest(fortranDll, modelInstancesParentDir, instanceCount);
    }

    private  static void runModelInstancesTest(File simpleFortranDll, File modelParentDir, int instanceCount) {

        IModelInstance[] modelInstances = new EfdcModelInstance[instanceCount];

        // create instance directories and intialize the model instances
        File modelTemplateDir = new File(modelParentDir, "model");

        EfdcDLL.initialize(simpleFortranDll, modelParentDir, modelTemplateDir, TimeZone.getTimeZone("GMT"));

        for (int i = 0; i < instanceCount; i++) {
            File instanceDir = new File(modelParentDir, "work" + i);
    		BBUtils.makeDirectoryClone(modelTemplateDir, instanceDir);
            modelInstances[i] = new EfdcModelInstance(instanceDir, new String[]{}, "model_output.nc", "analysis_output.nc", i, null);
        }

        // test save and compute
        System.out.println("Testing Save methods");
        for (int i = 0; i < modelInstances.length; i++) {

            IModelInstance modelInstance = modelInstances[i];
            
            // save Initial state  
            File saveFile = new File(modelTemplateDir, "state" + i + ".zip");
            IModelState myState0 = modelInstance.saveInternalState();
            myState0.savePersistentState(saveFile);
            modelInstance.releaseInternalState(myState0);
            
            //compute for one minute.
            ITime targetTime = new Time(modelInstance.getCurrentTime().getBeginTime().getMJD() + 1.0/24.0/60.0);
            modelInstance.compute(targetTime);
        }
        
        System.out.println("Testing Load methods");
        
        // test load and compute
        for (int i = 0; i <modelInstances.length ; i++) {
            IModelInstance modelInstance = modelInstances[i];
            File saveFile = new File(modelTemplateDir, "state" + i + ".zip");
            
            IModelState myState1 = modelInstance.loadPersistentState(saveFile);
            modelInstance.restoreInternalState(myState1);
            
            //compute for one minute.
            ITime targetTime = new Time(modelInstance.getCurrentTime().getBeginTime().getMJD() + 1.0/24.0/60.0);
            modelInstance.compute(targetTime);
            System.out.println("Done:" + i);
        }   
    }
}
