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
import org.openda.utils.OpenDaTestSupport;

/**
 * Test for the DLL based model instance
 */
public class EfdcGridExchangeItemToxicsTest extends TestCase {
    private OpenDaTestSupport testData;

    protected void setUp() {
    	testData = new OpenDaTestSupport(EfdcGridExchangeItemToxicsTest.class,"model_efdc_dll");
    }

    public void testGridExchangeItem() {

        final int instanceCount = 2;
        File modelInstancesParentDir = testData.getTestRunDataDir();
        File moduleRootDir = testData.getModuleRootDir();
        File fortranDll;
        String operatingSystemName = System.getProperty("os.name");
        System.out.println(operatingSystemName);
        if ( operatingSystemName.startsWith("Windows")) {
        	fortranDll = new File(moduleRootDir, "native_bin/win" + System.getProperty("sun.arch.data.model") + "_ifort/EfdcFortranDLL.dll");
        } else if (operatingSystemName.equalsIgnoreCase("Linux")) {
        	fortranDll = new File(moduleRootDir, "native_bin/linux" + System.getProperty("sun.arch.data.model") +  "_gnu/lib/libEFDC.so");
        } else if (EfdcDLL.RUNNING_ON_MAC){
            System.out.println("native_bin/darwin/lib/libEFDC.dylib");
            fortranDll = new File(moduleRootDir, "native_bin/darwin/lib/libEFDC.dylib");
        } else {
        	throw new RuntimeException("EFDC is not supported on this archictecture or operating system");
        }
        

        runModelInstancesTest(fortranDll, modelInstancesParentDir, instanceCount);
    }

    private static void runModelInstancesTest(File simpleFortranDll, File modelParentDir, int instanceCount) {

        IModelInstance[] modelInstances = new EfdcModelInstance[instanceCount];

        // create instance directories and intialize the model instances
        File modelTemplateDir = new File(modelParentDir, "model_GEUM");

        EfdcDLL.initialize(simpleFortranDll, modelParentDir, modelTemplateDir, TimeZone.getTimeZone("GMT"));

        for (int i = 0; i < instanceCount; i++) {
            File instanceDir = new File(modelParentDir, "work" + i);
    		BBUtils.makeDirectoryClone(modelTemplateDir, instanceDir);
            modelInstances[i] = new EfdcModelInstance(instanceDir, new String[]{}, "model_output.nc", "analysis_output.nc", i, null);
        }

        // test getting and setting values
        for ( IModelInstance modelInstance : modelInstances ) {

            // Replace current values for boundary exchange item
            for ( String exchangeItemId : modelInstance.getExchangeItemIDs()) {
            	if ( exchangeItemId.matches("^Grid.*") ) {
            		System.out.println(exchangeItemId);
                    EfdcGridExchangeItem exchangeItem = 
                            (EfdcGridExchangeItem) modelInstance.getDataObjectExchangeItem(exchangeItemId);
                    double[] values = exchangeItem.getValuesAsDoubles();
                    exchangeItem.setValuesAsDoubles(values);
            	}
            }
            
        }
    }
}
