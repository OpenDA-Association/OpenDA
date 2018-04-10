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

import java.io.File;
import java.util.TimeZone;

import junit.framework.TestCase;

import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IModelInstance;
import org.openda.interfaces.ITime;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;

/**
 * Test for the DLL based model instance
 */
public class EfdcModelInstanceTest extends TestCase {
    private OpenDaTestSupport testData;

    protected void setUp() {
    	testData = new OpenDaTestSupport(EfdcModelInstanceTest.class,"model_efdc_dll");
    }

    public void testDLLBasedModel() {

        final int instanceCount = 3;
        File modelInstancesParentDir = testData.getTestRunDataDir();
        File moduleRootDir = testData.getModuleRootDir();
        File fortranDll;
        if (EfdcDLL.RUNNING_ON_WINDOWS) {
        	fortranDll = new File(moduleRootDir, "native_bin/win" + System.getProperty("sun.arch.data.model") + "_ifort/EfdcFortranDLL.dll");
        } else if (EfdcDLL.RUNNING_ON_MAC){
            System.out.println("native_bin/darwin/lib/libEFDC.dylib");
            fortranDll = new File(moduleRootDir, "native_bin/darwin/lib/libEFDC.dylib");
        } else {
        	fortranDll = new File(moduleRootDir, "native_bin/linux" + System.getProperty("sun.arch.data.model") + "_gnu/lib/libEFDC.so");
        }

        runModelInstancesTest(fortranDll, modelInstancesParentDir, instanceCount);
    }

    private static void runModelInstancesTest(File simpleFortranDll, File modelParentDir, int instanceCount) {

        IModelInstance[] modelInstances = new EfdcModelInstance[instanceCount];

        // create instance directories and intialize the model instances
        File modelTemplateDir = new File(modelParentDir, "model");

        EfdcDLL.initialize(simpleFortranDll, modelParentDir, modelTemplateDir, TimeZone.getTimeZone("GMT"));

        for (int i = 0; i < instanceCount; i++) {
            File instanceDir = new File(modelParentDir, "work" + i);
    		BBUtils.makeDirectoryClone(modelTemplateDir, instanceDir);
            modelInstances[i] = new EfdcModelInstance(instanceDir, new String[]{}, "model_output.nc", "analysis_output.nc", i, null);
        }

        // test getting and setting values
        for (int i = 0; i < modelInstances.length; i++) {

            IModelInstance modelInstance = modelInstances[i];

            //compute for one minute.
            ITime targetTime = new Time(modelInstance.getCurrentTime().getBeginTime().getMJD() + 1.0/24.0/60.0);
            modelInstance.compute(targetTime);
            
            // Replace current values for boundary exchange item
            EfdcScalarTimeSeriesExchangeItem exchangeItem = (EfdcScalarTimeSeriesExchangeItem) modelInstance.getDataObjectExchangeItem("1.Precipitation");
            double[] times = exchangeItem.getTimeInfo().getTimes();
            exchangeItem.setTimesForUnitTest(times);
            double[] values = new double[times.length];
            for (int j = 0; j < times.length; j++) {
                values[j] = j + 100.0 *i;  
            }
            exchangeItem.setValuesAsDoubles(values);
 
            // Get them back
            double[] precipitation = exchangeItem.getValuesAsDoubles();
            assertEquals("precipitation.length", precipitation.length, values.length);
            assertEquals("precipitation[3]", 3 + 100.0*i, precipitation[3], 1e-5);
            
            // Insert shorter time series
            double[] myTime1 = new double[10];
            System.arraycopy(times,0,myTime1,0,myTime1.length);

            exchangeItem.setTimesForUnitTest(myTime1);
            double[] myTime2 = exchangeItem.getTimeInfo().getTimes();
            assertEquals("times.length", myTime1.length, myTime2.length);
            assertEquals("myTime[9]", myTime1[9], myTime2[9]);
            
            //Insert new time series longer than original
            if (i == -1) {
                myTime1 = new double[times.length+10];
                for (int j = 0; j < myTime1.length; j++) {
                   myTime1[j] = j;
                }
                exchangeItem.setTimesForUnitTest(myTime1);
                myTime2 = exchangeItem.getTimeInfo().getTimes();
                assertEquals("times.length", myTime1.length, myTime2.length);
                assertEquals("myTime[10]", myTime1[10], myTime2[10], 1e-5);
            }
         // Replace current values for boundary exchange item
            exchangeItem = (EfdcScalarTimeSeriesExchangeItem) modelInstance.getDataObjectExchangeItem("1.WaterLevel");
            times = exchangeItem.getTimeInfo().getTimes();
            exchangeItem.setTimesForUnitTest(times);
            values = new double[times.length];
            for (int j = 0; j < times.length; j++) {
                values[j] = j + 100.0 *i;  
            }
            exchangeItem.setValuesAsDoubles(values);
            
            // Get them back
            double[] waterLevel = exchangeItem.getValuesAsDoubles();
            assertEquals("waterLevel.length", waterLevel.length, values.length);
            assertEquals("waterLevel[3]", 3 + 100.0*i, waterLevel[3], 1e-5);
            
            // Replace current values for boundary exchange item
            exchangeItem = (EfdcScalarTimeSeriesExchangeItem) modelInstance.getDataObjectExchangeItem("22.Discharge");
            times = exchangeItem.getTimeInfo().getTimes();
            exchangeItem.setTimesForUnitTest(times);
            values = new double[times.length];
            for (int j = 0; j < times.length; j++) {
                values[j] = j + 100.0 *i;  
            }
            exchangeItem.setValuesAsDoubles(values);
            
            // Get them back
            double[] discharge = exchangeItem.getValuesAsDoubles();
            assertEquals("discharge.length", discharge.length, values.length);
            assertEquals("discharge[2]", 2 + 100.0*i, discharge[2], 1e-5);
            
            // Replace current values for boundary exchange item
            exchangeItem = (EfdcScalarTimeSeriesExchangeItem) modelInstance.getDataObjectExchangeItem("19.WaterTemperature");
            times = exchangeItem.getTimeInfo().getTimes();
            exchangeItem.setTimesForUnitTest(times);
            values = new double[times.length];
            for (int j = 0; j < times.length; j++) {
                values[j] = j + 100.0 *i;  
            }
            exchangeItem.setValuesAsDoubles(values);
            
            // Get them back
            double[] valuesOut = exchangeItem.getValuesAsDoubles();
            assertEquals("waterTemperature.length", valuesOut.length, values.length);
            assertEquals("waterTemperature[115]", 115 + 100.0*i, valuesOut[115], 1e-5);
            
            // Replace current values for boundary exchange item
            exchangeItem = (EfdcScalarTimeSeriesExchangeItem) modelInstance.getDataObjectExchangeItem("19.RefractoryPOCarbon");
            times = exchangeItem.getTimeInfo().getTimes();
            exchangeItem.setTimesForUnitTest(times);
            values = new double[times.length];
            for (int j = 0; j < times.length; j++) {
                values[j] = j + 100.0 *i;  
            }
            exchangeItem.setValuesAsDoubles(values);
            
            // Get them back
            valuesOut = exchangeItem.getValuesAsDoubles();
            assertEquals("RefractoryPOCarbon", valuesOut.length, values.length);
            assertEquals("RefractoryPOCarbon valuesOut[3]", 3 + 100.0*i, valuesOut[3], 1e-5);
            
        }

        for (int i = 0; i < instanceCount; i++) {
            modelInstances[i].finish();
        }
    }
}
