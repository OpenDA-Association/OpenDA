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

import junit.framework.TestCase;
import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IModelInstance;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.util.TimeZone;

/**
 * Test for the DLL based model instance
 */
public class EfdcScalarExchangeItemLayerTest extends TestCase {
    private OpenDaTestSupport testData;

    protected void setUp() {
    	testData = new OpenDaTestSupport(EfdcScalarExchangeItemLayerTest.class,"model_efdc_dll");
    }

    public void testScalarExchangeItem() {

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

        String[] exchangeItemIDs = new String[4];
        
        exchangeItemIDs[0] = "1.Precipitation";
        exchangeItemIDs[1] = "1_layer1.DissolvedONitrogen";
        exchangeItemIDs[2] = "22_layer5.DissolvedONitrogen";
        exchangeItemIDs[3] = "1_layer3.Discharge";

        // create instance directories and intialize the model instances
        File modelTemplateDir = new File(modelParentDir, "model_layers");

        EfdcDLL.initialize(simpleFortranDll, modelParentDir, modelTemplateDir, TimeZone.getTimeZone("GMT"));

        for (int i = 0; i < instanceCount; i++) {
            File instanceDir = new File(modelParentDir, "work" + i);
    		BBUtils.makeDirectoryClone(modelTemplateDir, instanceDir);
            modelInstances[i] = new EfdcModelInstance(instanceDir, new String[]{}, "model_output.nc", "analysis_output.nc", i,  null);
        }
        
        
        // test getting and setting values
        for ( IModelInstance modelInstance : modelInstances) {

            for (String exchangeItemId :  exchangeItemIDs ) {

                // Replace current values for boundary exchange item
                EfdcScalarTimeSeriesExchangeItem exchangeItem = (EfdcScalarTimeSeriesExchangeItem) modelInstance.getDataObjectExchangeItem(exchangeItemId);
                double[] times = exchangeItem.getTimeInfo().getTimes();
                double[] newTimes = new double[times.length +1];
                System.arraycopy( times, 0, newTimes, 0, times.length );
                newTimes[times.length] =  newTimes[times.length-1] - newTimes[1] + newTimes[2];
                
                //newTimes = times;
                exchangeItem.setTimesForUnitTest(newTimes);
                // enlarge dataseries by one
                double[] values = new double[newTimes.length];
                for (int j = 0; j < times.length; j++) {
                    values[j ] = j ;
                }
                exchangeItem.setValuesAsDoubles(values);
     
                // Get them back
                double[] testValues = exchangeItem.getValuesAsDoubles();
                assertEquals( exchangeItemId + ".length", testValues.length, values.length);
                assertEquals( exchangeItemId + "[2]", 2 , testValues[2], 1e-5);
            	
            }
                
        }
    }
}
