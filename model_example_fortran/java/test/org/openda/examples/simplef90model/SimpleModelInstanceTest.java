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
package org.openda.examples.simplef90model;

import junit.framework.TestCase;

import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IInstance;
import org.openda.interfaces.IModelInstance;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;

/**
 * Test for the DLL based model instance
 */
public class SimpleModelInstanceTest extends TestCase {

    private OpenDaTestSupport testData;
    private boolean skipTest = false;

    protected void setUp() {
    	testData = new OpenDaTestSupport(SimpleModelInstanceTest.class,"model_example_fortran");
    }

    public void testDLLBasedModel() {
    	// TODO Currently native build does work on linux 
        if (!BBUtils.RUNNING_ON_WINDOWS) {
            return;
        }

    	
        final int instanceCount = 3;
        File modelInstancesParentDir = testData.getTestRunDataDir();
        File moduleRootDir = testData.getModuleRootDir();
        File simpleFortranDll;
        if (SimpleModelDLL.RUNNING_ON_WINDOWS) {
        	simpleFortranDll = new File(moduleRootDir, "native_bin/win32_ifort/simplefortrandll.dll");
        } else {
        	simpleFortranDll = new File(moduleRootDir, "native_bin/linux32_gnu/libsimplefortran.so");
        }

        runSimpleModelInstancesTest(simpleFortranDll, modelInstancesParentDir, instanceCount);
    }

    static void runSimpleModelInstancesTest(File simpleFortranDll, File modelParentDir, int instanceCount) {

        IModelInstance[] modelInstances = new SimpleModelInstance[instanceCount];

        // create instance directories and intialize the model instances
        File modelTemplateDir = new File(modelParentDir, "model");
        String schemFileName = "mySchematization.txt";

        SimpleModelInstance.initialize(simpleFortranDll, modelParentDir, modelTemplateDir, schemFileName);

        IInstance parent = null;  // Will in reality be the factory

        for (int i = 0; i < instanceCount; i++) {
            File instanceDir = SimpleModelDLLTest.createInstanceDirectory(i, "work", modelParentDir);
            modelInstances[i] = SimpleModelInstance.getInstance(instanceDir, parent);
        }

        // test getting and setting values
        for (int i = 0; i < modelInstances.length; i++) {

            IModelInstance modelInstance = modelInstances[i];

            // adjust gravity
            IPrevExchangeItem exchangeItem = modelInstance.getExchangeItem("Gravity");
            assertEquals("gravity", 9.1d, exchangeItem.getValuesAsDoubles()[0], 1e-10);
            exchangeItem.setValues(9.1d + .01d * (i+1));

            // adjust frictions
            exchangeItem = modelInstance.getExchangeItem("GridPoints.Friction");
            double[] frictions = exchangeItem.getValuesAsDoubles();
            assertEquals("frictions.lenght", 4, frictions.length);
            assertEquals("frictions[3]", 400.0, frictions[3], 1e-10);
            for (int j = 0; j < frictions.length; j++) {
                frictions[j] += 1000*(i+1);
            }
            exchangeItem.setValues(frictions);

            // get and check water levels
            exchangeItem = modelInstance.getExchangeItem("GridPoints.WaterLevel");
            double[] waterlevels = exchangeItem.getValuesAsDoubles();
            assertEquals("waterlevels.lenght", 4, waterlevels.length);
            assertEquals("waterlevels[3]", 4.0, waterlevels[3], 1e-10);
        }

        // retrieve adjusted values
        for (int i = 0; i < instanceCount; i++) {
            double gravity = (Double) modelInstances[i].getExchangeItem("Gravity").getValues();
            assertEquals("gravity", 9.1d + .01d * (i+1), gravity, 1e-10);
            double[] frictions = modelInstances[i].getExchangeItem("GridPoints.Friction").getValuesAsDoubles();
            assertEquals("frictions", 400.0 + 1000*(i+1), frictions[3], 1e-10);
        }
    }
}
