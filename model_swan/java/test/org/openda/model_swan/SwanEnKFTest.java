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

package org.openda.model_swan;

import junit.framework.TestCase;
import org.openda.application.ApplicationRunner;
import org.openda.application.OpenDaApplication;
import org.openda.blackbox.wrapper.ArmaNoiseModel;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.StochVector;

import java.io.File;
import java.io.IOException;

/**
 * Test of SWAN simulation and EnKF, where SWAN is prepared as a Black Box model.
 */
public class SwanEnKFTest extends TestCase {
    private OpenDaTestSupport testData = null;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(SwanEnKFTest.class, "model_swan");
    }

    public void testSimulationSwan() throws IOException {
    	StochVector.setSeed(1234); //fix random numbers for repeatable results
        File applicationConfigFile = new File(testData.getTestRunDataDir(), "SwanSimpleBBEnKF/SwanSimulation.oda");
        ApplicationRunner.setRunningInTest(true);
        OpenDaApplication.main(new String[]{applicationConfigFile.getAbsolutePath()});
    }

    public void testEnKFSwan() throws IOException {
    	ArmaNoiseModel.resetInstanceCounter();
    	StochVector.setSeed(1234); //fix random numbers for repeatable results
        File applicationConfigFile = new File(testData.getTestRunDataDir(), "SwanSimpleBBEnKF/SwanEnKF.oda");
        ApplicationRunner.setRunningInTest(true);
        OpenDaApplication.main(new String[]{applicationConfigFile.getAbsolutePath()});
    }

}
