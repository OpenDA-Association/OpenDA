/* OpenDA v2.4.3 
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
package org.openda.model_delft3d.dll;

import junit.framework.TestCase;
import org.openda.application.ApplicationRunner;
import org.openda.blackbox.config.BBUtils;
import org.openda.blackbox.wrapper.BBStochModelFactory;
import org.openda.interfaces.*;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;
import org.openda.utils.Vector;

import java.io.File;
import java.io.IOException;

/**
 * Tests for Delft3D flow DLL
 */
public class D3dFlowStochModelFactoryTest extends TestCase {

    OpenDaTestSupport testData = null;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(D3dFlowStochModelFactoryTest.class, "public", "model_delft3d");
    }

    public static void testDummy() {
        // No action. Test only exist to avoid warnings on empty test class when
        //            the test below is de-activated by renaming it to tst...()
    }

    public void testD3dFlowStochModelFactoryOnFilterTest2DModel() {

		//TODO linux
		//currently only dll file for windows is available (not for linux), so only run this on windows.
		if (!BBUtils.RUNNING_ON_WINDOWS) {
			return;
		}

            IStochModelFactory bbStochModelFactory = new BBStochModelFactory();
            bbStochModelFactory.initialize(testData.getTestRunDataDir(),
                    new String[] {"ft2d_d3dStochModelFactoryConfig_win32_ifort.xml"});

            int instanceCount = 3;
            IStochModelInstance[] stochModelInstances = new IStochModelInstance[instanceCount];
            for (int i = 0; i < instanceCount; i++) {
                stochModelInstances[i] = bbStochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
            }

            double[][] compareVals = new double[4][3];
            compareVals[0][0] = 0.0;
            compareVals[0][1] = 0.0;
            compareVals[0][2] = 0.0;

            compareVals[1][0] = 0.00224904;
            compareVals[1][1] = 0.01222531;
            compareVals[1][2] = 0.02220205;

			compareVals[2][0] = 0.00224904;
			compareVals[2][1] = 0.02222531;
			compareVals[2][2] = 0.04220205;

			compareVals[3][0] = 0.00224904;
			compareVals[3][1] = 0.03222531;
			compareVals[3][2] = 0.06220205;

            int timeStepCount = 4;
            ITime timeHorizon = stochModelInstances[0].getTimeHorizon();
            ITime startTime = timeHorizon.getBeginTime();
            double deltaTasMJD = 1d/24d/60d/2d; // half a minute
            for (int t = 0; t < timeStepCount; t++) {
                ITime targetTime = new Time(startTime.getMJD() + deltaTasMJD);
                for (int i = 0; i < instanceCount; i++) {
                    IVector state = stochModelInstances[i].getState();
                    assertEquals("corestate[43]("+t+","+i+")",compareVals[t][i],state.getValue(45),1E-6);
                    assertEquals("state size", 2859, state.getSize());
                    IVector axpyVector = new Vector(state.getSize());
                    axpyVector.setConstant(0.01 * i);
					axpyVector.setValue(0, 0.0001);
					axpyVector.setValue(1, 0.0001);
                    stochModelInstances[i].axpyOnState(1.0, axpyVector);
                    // temp: kijken hoe state er nu uit ziet
                      state = stochModelInstances[i].getState();
                     double tmp = state.getValue(45);

                    stochModelInstances[i].compute(targetTime);
                }
            }
			for (int i = 0; i < instanceCount; i++) {
				stochModelInstances[i].finish();
			}
    }

	public void tstD3dFlow1DEstuarySSKFGET() {
		ApplicationRunner.setRunningInTest(true);
		File config = new File("d:\\temp\\openda\\Abhijit\\Est1D-SSKF-Gen\\Enkf_generate_gain.oda");
		String args[] = new String[1];
		args[0] = config.getAbsolutePath();
		org.openda.application.OpenDaApplication.main(args);
	}


	public void tstD3dFlow1DEstuaryEnkfApplicationOrg() {
		ApplicationRunner.setRunningInTest(true);
		File config = new File("d:\\temp\\openda\\d3dDLL\\org\\d3d_estuary_1d\\d3d_est1d_enkf.oda");
		String args[] = new String[1];
		args[0] = config.getAbsolutePath();
		org.openda.application.OpenDaApplication.main(args);
	}


	public void tstD3dFlow1DEstuaryEnkfApplicationMds() {
		ApplicationRunner.setRunningInTest(true);
		File config = new File("d:\\temp\\openda\\d3dDLL\\mds\\d3d_estuary_1d\\d3d_est1d_enkf.oda");
		String args[] = new String[1];
		args[0] = config.getAbsolutePath();
		org.openda.application.OpenDaApplication.main(args);
	}


	public void tstD3dFlowBakje2DEnkfApplication() {
		ApplicationRunner.setRunningInTest(true);
		File config = new File("d:\\src\\openda_1\\deltares\\models\\java\\tests\\d3d_bakje_2d\\d3d_bakje_2d_enkf.oda");
		String args[] = new String[1];
		args[0] = config.getAbsolutePath();
		org.openda.application.OpenDaApplication.main(args);
	}


	public void tstD3dFlow2DLakeEnkfApplication() {
		ApplicationRunner.setRunningInTest(true);
		File config = new File("d:\\src\\openda_1\\deltares\\models\\java\\tests\\d3d_lake_2d\\d3d_lake2d_enkf.oda");
		String args[] = new String[1];
		args[0] = config.getAbsolutePath();
		org.openda.application.OpenDaApplication.main(args);
	}

	public void tstD3dFlow2DSeaEnkfApplication() {
		ApplicationRunner.setRunningInTest(true);
		File config = new File("d:\\src\\openda_1\\deltares\\models\\java\\tests\\d3d_sea_2d\\d3d_sea2d_enkf.oda");
		String args[] = new String[1];
		args[0] = config.getAbsolutePath();
		org.openda.application.OpenDaApplication.main(args);
	}

	public void tstD3dFlowM433EnkfApplication() {
		ApplicationRunner.setRunningInTest(true);
		File config = new File("d:\\src\\openda_1\\deltares\\models\\java\\tests\\d3d_singapore_m433(klein)\\d3d_m433_enkf.oda");
		String args[] = new String[1];
		args[0] = config.getAbsolutePath();
		org.openda.application.OpenDaApplication.main(args);
	}

}
