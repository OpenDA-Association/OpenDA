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
import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.ITime;
import org.openda.model_delft3d.dll.D3dFlowModelConfig.DllType;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

import static java.lang.Math.*;

/**
 * Tests for Delft3D flow DLL
 */
public class D3dFlowDllTest extends TestCase {

	OpenDaTestSupport testData = null;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(D3dFlowDllTest.class, "public", "model_delft3d");
	}

	public static void testDummy() {
		// No action. Test only exist to avoid warnings on empty test class when
		//            the test below is de-activated by renaming it to tst...()
	}

	public void tstD3dFlowDllOnRotModel() {

		//TODO linux
		//currently only dll file for windows is available (not for linux), so only run this on windows.
		//if (!BBUtils.RUNNING_ON_WINDOWS) {
		//	return;
		//}
		String dllName;
		DllType dllType;
		if (BBUtils.RUNNING_ON_LINUX) {
			if(System.getProperty("sun.arch.data.model").equals("64")){
				dllName="model_delft3d/native_bin/linux64_gnu/lib/libflow2d3d_openda.so.0.0.0";
				dllType=DllType.linux64_gnu;
			}else{
				//no testing on linux 32-bit
				return;
			}
		}else if (BBUtils.RUNNING_ON_WINDOWS) {
			if(System.getProperty("sun.arch.data.model").equals("32")){
				dllName="model_delft3d/native_bin/win32_ifort/flow2d3d_openda.dll";
				dllType=DllType.win32_ifort;
			}else{
				//no testing on windows 64-bit
				return;
			}
		}else{
			return; //no testing but on windows and linux
		}

		File nativeDLL = new File(testData.getProjectRootDir(), dllName);
		File rotModelDir = new File(testData.getTestRunDataDir(), "rot");
		D3dFlowDll.loadDLL(dllType, nativeDLL.getAbsolutePath());
		D3dFlowDll.initializeModel(rotModelDir, "rot.mdf", 5);
		int instanceCount = 3;
		int[] instanceHandle = new int[instanceCount];

		for (int i = 0; i < instanceCount ; i++) {
			instanceHandle[i] = D3dFlowDll.createInstance(rotModelDir);
		}
		ITime timeHorizon = D3dFlowDll.getTimeHorizon();
		assertEquals("timeHorizon.Start", 51983.0d, timeHorizon.getBeginTime().getMJD());


		for (int i = 0; i < instanceCount ; i++) {
			D3dFlowDll.selectInstance(rotModelDir, instanceHandle[i]);
			double currentTime = D3dFlowDll.getCurrentTime();
			assertEquals("currentTime", 51983.0d, currentTime);
		}

		D3dFlowDll.finish(rotModelDir);
		System.out.println(" testD3dFlowDllOnRotModel finished");
	}

	public void tstD3dFlowDllOnFilterTest2DModel() {

		String dllName;
		DllType dllType;
		if (BBUtils.RUNNING_ON_LINUX) {
			if(System.getProperty("sun.arch.data.model").equals("64")){
				dllName="model_delft3d/native_bin/linux64_gnu/lib/libflow2d3d_openda.so.0.0.0";
				dllType=DllType.linux64_gnu;
			}else{
				//no testing on linux 32-bit
				return;
			}
		}else if (BBUtils.RUNNING_ON_WINDOWS) {
			if(System.getProperty("sun.arch.data.model").equals("32")){
				dllName="model_delft3d/native_bin/win32_ifort/flow2d3d_openda.dll";
				dllType=DllType.win32_ifort;
			}else{
				//no testing on windows 64-bit
				return;
			}
		}else{
			return; //no testing but on windows and linux
		}

		File nativeDLL = new File(testData.getProjectRootDir(), dllName);
		File test2dModelDir = new File(testData.getTestRunDataDir(), "filtertest2d");
		D3dFlowDll.loadDLL(dllType, nativeDLL.getAbsolutePath());
		D3dFlowDll.initializeModel(test2dModelDir, "test_2d.mdf", 5);
		int instanceCount = 3;
		int[] instanceHandle = new int[instanceCount];

		for (int i = 0; i < instanceCount ; i++) {
			instanceHandle[i] = D3dFlowDll.createInstance(test2dModelDir);
		}
		ITime timeHorizon = D3dFlowDll.getTimeHorizon();
		assertEquals("timeHorizon.Start", 55391.00000d, timeHorizon.getBeginTime().getMJD(),1.0E-6);
		assertEquals("timeHorizon.End", 55391.10000d, timeHorizon.getEndTime().getMJD(),1.0E-6);

		int sepBoundNoiseId = D3dFlowDll.getBoundaryExchangeItemID("H_bnd2",
				D3dFlowExchangeItemConfig.EI_bound_HQ);
		assertEquals("sepBoundNoiseId", 6009, sepBoundNoiseId);
		int tempBoundNoiseId = D3dFlowDll.getBoundaryExchangeItemID("H_bnd1",
				D3dFlowExchangeItemConfig.EI_bound_temp);
		assertEquals("tempBoundNoiseId", 5010, tempBoundNoiseId);
		int zwlMonitorpointId = D3dFlowDll.getBoundaryExchangeItemID("H_bnd2_A",
				D3dFlowExchangeItemConfig.EI_waterlevel);
		assertEquals("zwlMonitorpointId", 5001, zwlMonitorpointId);

		for (int i = 0; i < instanceCount ; i++) {
			D3dFlowDll.selectInstance(test2dModelDir, instanceHandle[i]);
			double currentTime = D3dFlowDll.getCurrentTime();
			assertEquals("currentTime", 55391.00000d, currentTime);

			// Perform a neutral operation: no changes at boundary
			int retVal = D3dFlowDll.setBoundaryNoise(sepBoundNoiseId, 1.0, D3dFlowExchangeItemConfig.OPER_multiply);
			if (retVal != 0) {
				throw new RuntimeException("Error in D3dFlowDll.setBoundaryNoise, retVal " + retVal);
			}

			D3dFlowDll.performTimeStep(test2dModelDir, 1);
			currentTime = D3dFlowDll.getCurrentTime();
			assertEquals("currentTime", 55391.0003472d, currentTime, 1.0E-6);

			if (i == 1) {
				// Set a constant noise on 2nd instance
				retVal = D3dFlowDll.setBoundaryNoise(sepBoundNoiseId, 1.1, D3dFlowExchangeItemConfig.OPER_multiply);
				if (retVal != 0) {
					throw new RuntimeException("Error in D3dFlowDll.setBoundaryNoise, retVal " + retVal);
				}
			}

			D3dFlowDll.performTimeStep(test2dModelDir, 1);

			// now ask the waterlevel in monitor station on location of boundary!

			double sepVal = D3dFlowDll.getResultValue(zwlMonitorpointId);
			if (i == 1){
				assertEquals("sep in monitorpoint",1.1683E-005 , sepVal,1.0E-9);
				// this is wrong, we need to reconsider monitor points
			}
			if (i != 1){
				assertEquals("sep in monitorpoint",1.0815E-005 , sepVal,1.0E-9);
			}
		}
		D3dFlowDll.storeCurrentInstance(test2dModelDir);    // store the last instance

		// now save the instances to disk for restart purposes!
		for (int i = 0; i < instanceCount ; i++) {
			D3dFlowDll.selectInstance(test2dModelDir, instanceHandle[i]);

			String fileName;
			//Note: only netcdf-files should be used for restart, since the
			// native code does not support non-netcdf reading of state files
			fileName = "restart_t2_ensemble_".concat(String.valueOf(i)).concat(".nc");
			D3dFlowDll.storeCurrentInstanceRestartfile(test2dModelDir, fileName);

		}
		// Now we run four more timesteps; save state 2 to local array;
		// reset the time; restart state2; and check.
		for (int tstep=0; tstep < 4; tstep ++) {
			for (int i = 0; i < instanceCount ; i++) {
				D3dFlowDll.selectInstance(test2dModelDir, instanceHandle[i]);
				D3dFlowDll.performTimeStep(test2dModelDir, 1);
			}
		}
		D3dFlowDll.selectInstance(test2dModelDir, instanceHandle[2]);
		double[] state2_end = D3dFlowDll.getStateValues();
		D3dFlowDll.selectInstance(test2dModelDir, instanceHandle[1]);
		double[] state1_end = D3dFlowDll.getStateValues();
		// these states are different because of the difference in noise.

		//resetting the time is not possible. Therefore: repeat everything!
		D3dFlowDll.finish(test2dModelDir);

		File nativeDLL2 = new File(testData.getProjectRootDir(), dllName);
		File test2dModelDir2 = new File(testData.getTestRunDataDir(), "filtertest2d");
		D3dFlowDll.loadDLL(dllType, nativeDLL2.getAbsolutePath());
		D3dFlowDll.initializeModel(test2dModelDir2, "test_2d.mdf", 5);

		instanceHandle[0] = D3dFlowDll.createInstance(test2dModelDir);
		instanceHandle[1] = D3dFlowDll.createInstance(test2dModelDir);
		for (int tstep=0; tstep < 2; tstep ++) {
			D3dFlowDll.performTimeStep(test2dModelDir2, 1);
		}
		double[] state0_2 = D3dFlowDll.getStateValues();
		// now load the restart file for instance 0.
		D3dFlowDll.selectInstanceFromRestartfile(test2dModelDir2, instanceHandle[0],"restart_t2_ensemble_0.nc");
		double[] state0_rst = D3dFlowDll.getStateValues();
		// state0_rst and state0_2 are equal:
		double retval =  diffStates(state0_rst,state0_2);
		assertEquals("aaa",0.0,retval, 1E-8);

		// now load the restart file for instance 1.

		D3dFlowDll.selectInstance(test2dModelDir2, instanceHandle[1]);
		double[] state1_2 = D3dFlowDll.getStateValues();
		state1_2[2] = state1_2[2]+2.0;
		D3dFlowDll.setStateValues(state1_2);

		D3dFlowDll.selectInstanceFromRestartfile(test2dModelDir2, instanceHandle[1],"restart_t2_ensemble_1.nc");
		double[] state1_rst = D3dFlowDll.getStateValues();

		// state1_2 and state1_rst are (much) different:
		retval =  diffStates(state1_rst,state1_2);
		assertEquals("aaa",2.0,retval, 1E-8);

		// state0_2 and state1_rst are different:
		retval =  diffStates(state1_rst,state0_2);
		assertEquals("aaa",1.44E-6,retval, 1E-8);


		// now perform the four remaining timesteps with the restarted state.
		D3dFlowDll.selectInstance(test2dModelDir2, instanceHandle[0]);
		for (int tstep=2; tstep < 6; tstep ++) {
			D3dFlowDll.performTimeStep(test2dModelDir2, 1);
		}
		double[] state0_rst_end = D3dFlowDll.getStateValues();

		// state2_end and state0_rst_end are (almost) equal.
		retval =  diffStates(state0_rst_end,state2_end);
		assertEquals("aaa",0.002386,retval, 1E-6);

		// state2_end and state0_rst_end differ more.   They do not?
		retval =  diffStates(state0_rst_end,state1_end);
		assertEquals("aaa",0.002386,retval, 1E-6);

		D3dFlowDll.finish(test2dModelDir2);
		System.out.println("finish");
	}

	private double diffStates(double[] state1,double[] state2){
		int len = state1.length;
		double sumdiff = 0.0;
		for (int i=0; i < len; i++) {
			sumdiff = sumdiff + (state1[i]-state2[i])*(state1[i]-state2[i]);
		}
		return  sqrt(sumdiff);

	}
}

