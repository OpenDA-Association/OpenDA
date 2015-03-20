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
import java.io.File;

import org.openda.blackbox.config.BBUtils;
import org.openda.utils.OpenDaTestSupport;

/**
 * Test for the simple model dll
 */
public class SimpleModelDLLTest extends TestCase {

    private OpenDaTestSupport testData;
    private boolean skipTest = false;

    protected void setUp() {
    	testData = new OpenDaTestSupport(SimpleModelDLLTest.class,"model_example_fortran");
    }

    public void testSimpleModelDLL() {

    	// TODO Currently native build does work on linux 
        if (!BBUtils.RUNNING_ON_WINDOWS) {
            return;
        }

        final int instanceCount = 2;
        SimpleModelDLL[] simpleModelDLLs = new SimpleModelDLL[instanceCount];

        File moduleRootDir = testData.getModuleRootDir();
        File simpleFortranDll;
        if (SimpleModelDLL.RUNNING_ON_WINDOWS) {
            simpleFortranDll = new File(moduleRootDir, "native_bin/win32_ifort/simplefortrandll.dll");
        } else {
            simpleFortranDll = new File(moduleRootDir, "native_bin/linux32_gnu/libsimplefortran.so");
        }
        File modelTemplateDir = new File(testData.getTestRunDataDir(), "model");
        String schemFileName = "mySchematization.txt";

        // load the native dll for this simple schematization
        File modelInstancesParentDir = testData.getTestRunDataDir();
        SimpleModelDLL.initialize(simpleFortranDll, modelInstancesParentDir, modelTemplateDir, schemFileName);

        // create instance directories and intialize the model instances
        for (int i = 0; i < instanceCount; i++) {
            File instanceDir = SimpleModelDLLTest.createInstanceDirectory(i, "work", modelInstancesParentDir);
            simpleModelDLLs[i] = SimpleModelDLL.getInstance(instanceDir);
        }

        // test getting and setting values
        for (int i = 0; i < instanceCount; i++) {

            // adjust gravity
            double gravity = simpleModelDLLs[i].getValue(SimpleModelDLL.gravity);
            assertEquals("gravity", 9.1d, gravity, 1e-10);
            gravity += .01d * (i+1);
            simpleModelDLLs[i].setValue(SimpleModelDLL.gravity, gravity);

            // TODO adjust discharges
            // double[] discharges = simpleModelDLLs[i].getValues(SimpleModelDLL.discharge_on_laterals);
            // assertEquals("discharge_on_laterals.lenght", 2, discharges.length);

            // adjust frictions
            double[] frictions = simpleModelDLLs[i].getValues(SimpleModelDLL.friction_on_grid);
            assertEquals("frictions_on_grid.lenght", 4, frictions.length);
            assertEquals("frictions[3]", 400.0, frictions[3], 1e-10);
            for (int j = 0; j < frictions.length; j++) {
                frictions[j] += 1000*(i+1);
            }
            simpleModelDLLs[i].setValues(SimpleModelDLL.friction_on_grid, frictions);

            // get and check water levels
            double[] waterlevels = simpleModelDLLs[i].getValues(SimpleModelDLL.waterlevel_on_grid);
            assertEquals("waterlevels_on_grid.lenght", 4, waterlevels.length);
            assertEquals("waterlevels[3]", 4.0, waterlevels[3], 1e-10);
        }

        // retrieve adjusted values
        for (int i = 0; i < instanceCount; i++) {
            double gravity = simpleModelDLLs[i].getValue(SimpleModelDLL.gravity);
            assertEquals("gravity", 9.1d + .01d * (i+1), gravity, 1e-10);
            double[] frictions = simpleModelDLLs[i].getValues(SimpleModelDLL.friction_on_grid);
            assertEquals("frictions", 400.0 + 1000*(i+1), frictions[3], 1e-10);
        }

        // cleanup
        for (int i = 0; i < instanceCount; i++) {
            simpleModelDLLs[i].finish();
        }
        SimpleModelDLL.free();
    }

    static File createInstanceDirectory(int i, String instanceDirPrefix, File modelParentDir) {

        // create instance directory
        File instanceDir = new File(modelParentDir, instanceDirPrefix + (i+1));
        if (instanceDir.exists()) {
            for (File file : instanceDir.listFiles()) {
                if (!file.delete()) {
                    throw new RuntimeException("could not delete file " + file.getAbsolutePath());
                }
            }
            if (!instanceDir.delete()) {
                throw new RuntimeException("could not delete dir " + instanceDir.getAbsolutePath());
            }
        }
        if (!instanceDir.mkdir()) {
            throw new RuntimeException("could not create dir " + instanceDir.getAbsolutePath());
        }
        return instanceDir;
    }
}

