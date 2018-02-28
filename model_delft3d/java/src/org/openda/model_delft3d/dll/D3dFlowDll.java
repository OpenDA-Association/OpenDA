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

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.ptr.DoubleByReference;
import com.sun.jna.ptr.IntByReference;
import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.ITime;
import org.openda.utils.Time;

import java.io.File;

/**
 * Delft3D flow DLL
 */
public class D3dFlowDll {

	private static D3dFlowWinIfortDll winIfortDll = null;
	private static D3dFlowLinuxGnuDll linuxGnuDll = null;
	private static D3dFlowModelConfig.DllType platform;

	private static String componentID = "D3dFlow";
	private static boolean dllHasBeenLoaded = false;
	private static String modelIdentifier = "flowModel";

	// Separat loading the Delft3D-Flow DLL from initializing the model/
	// The DLL is loaded only once, even when more than one unit test is run.
	// The model's finish() method does not unload the DLL (a DLL can not be unloaded,
	// at least not on Windows)
	static void loadDLL(D3dFlowModelConfig.DllType platform, String nativeDllPath) {
		D3dFlowDll.platform = platform;
		if (platform == D3dFlowModelConfig.DllType.win32_ifort) {
			if (!dllHasBeenLoaded) {
				String nativeDllName = new File(nativeDllPath).getName();
				File nativeDllDir = new File(nativeDllPath).getParentFile();
				File[] filesInDllDir = nativeDllDir.listFiles();
				if (filesInDllDir == null) {
					throw new RuntimeException("error in initialize: no files in DLL dir");
				}
				for (File file : filesInDllDir) {
					String fileName = file.getName();
					if (fileName.toLowerCase().endsWith(".dll") &&
							!(fileName.equalsIgnoreCase(nativeDllName)) &&
							!(fileName.toLowerCase().contains("libpollute2d")) &&
							!(fileName.toLowerCase().contains("jep")) &&
							!(fileName.toLowerCase().contains("mpich")) &&
							!(fileName.equalsIgnoreCase("dlwqlib.dll"))
							){
						// dependent dll, load it.
						//System.out.println("loading dependent dll: "+fileName+"\n");
						Native.loadLibrary(file.getAbsolutePath(), IDummyDll.class);
					}
				}
				winIfortDll = (D3dFlowWinIfortDll) Native.loadLibrary(nativeDllPath, D3dFlowWinIfortDll.class);
				dllHasBeenLoaded = true;
			}
		} else if (platform == D3dFlowModelConfig.DllType.linux64_gnu) {
			linuxGnuDll = (D3dFlowLinuxGnuDll) Native.loadLibrary(nativeDllPath, D3dFlowLinuxGnuDll.class);
		} else {
			throw new RuntimeException("loadDLL: DLL/so type not known");
		}
	}

	// Initialize the model
	static void initializeModel(File modelDir, String modelIdentifier, int maxNumInstancesInMemory) {
		setModelDirAsCWD(modelDir);
		int retVal;
		if (D3dFlowDll.platform == D3dFlowModelConfig.DllType.win32_ifort) {
			winIfortDll.SE_SET_MAX_INSTANCES_IN_MEMORY(new IntByReference(maxNumInstancesInMemory));
			retVal = winIfortDll.SE_INITIALIZE_OPENDA(componentID, modelIdentifier,
					componentID.length(), modelIdentifier.length());
		} else if (platform == D3dFlowModelConfig.DllType.linux64_gnu) {
			linuxGnuDll.se_set_max_instances_in_memory_(new IntByReference(maxNumInstancesInMemory));
			retVal = linuxGnuDll.se_initialize_openda_(componentID, modelIdentifier,
					componentID.length(), modelIdentifier.length());
		} else {
			resetCWD();
			throw new RuntimeException("initialize: DLL/so type not known");
		}
		resetCWD();
		if (retVal != 0) {
			throw new RuntimeException("Error in D3dFlowDll.SE_INITIALIZE(), retVal " + retVal);
		}
	}

	public static int createInstance(File modelDir) {
		setModelDirAsCWD(modelDir);
		int retVal;
		if (platform == D3dFlowModelConfig.DllType.win32_ifort) {
			retVal = winIfortDll.SE_CREATE_INSTANCE();
		} else if (platform == D3dFlowModelConfig.DllType.linux64_gnu) {
			retVal = linuxGnuDll.se_create_instance_();
		} else {
			resetCWD();
			throw new RuntimeException("createInstance: DLL/so type not known for model");
		}
		resetCWD();
		if (retVal < 0) {
			throw new RuntimeException("Error in D3dFlowDll.createInstance(), retVal");
		}
		return retVal;
	}

	public static void selectInstance(File modelDir, int instanceId) {
		setModelDirAsCWD(modelDir);
		if (platform == D3dFlowModelConfig.DllType.win32_ifort) {
			winIfortDll.SE_SELECT_INSTANCE(new IntByReference(instanceId));
		}else if (platform == D3dFlowModelConfig.DllType.linux64_gnu) {
			linuxGnuDll.se_select_instance_(new IntByReference(instanceId));
		} else {
			resetCWD();
			throw new RuntimeException("createInstance: DLL/so type not known for model");
		}
		resetCWD();
		if (instanceId < 0) {
			throw new RuntimeException("Error in D3dFlowDll.selectInstance(), returned ID " + instanceId);
		}
	}

	public static void selectInstanceFromRestartfile(File modelDir, int instanceId, String restartFileName) {
		setModelDirAsCWD(modelDir);
		if (platform == D3dFlowModelConfig.DllType.win32_ifort) {
			winIfortDll.SE_SELECT_INSTANCE_FROM_RESTARTFILE(new IntByReference(instanceId),restartFileName,
					restartFileName.length());
		}else if (platform == D3dFlowModelConfig.DllType.linux64_gnu) {
			linuxGnuDll.se_select_instance_from_restartfile_(new IntByReference(instanceId),restartFileName,
					restartFileName.length());
		} else {
			resetCWD();
			throw new RuntimeException("createInstance: DLL/so type not known for model");
		}
		resetCWD();
		if (instanceId < 0) {
			throw new RuntimeException("Error in D3dFlowDll.selectInstanceRestartfile(), returned ID " + instanceId);
		}
	}


	public static void storeCurrentInstance(File modelDir) {
		setModelDirAsCWD(modelDir);
		int retVal;
		if (platform == D3dFlowModelConfig.DllType.win32_ifort) {
			int level  = 0;    // TODO: administrate equivalents of storage levels used in Delft3Dflow somehere here.
			retVal = winIfortDll.SE_STORE_CURRENT_INSTANCE(new IntByReference(level));
		}else if (platform == D3dFlowModelConfig.DllType.linux64_gnu) {
			int level  = 0;    // TODO: administrate equivalents of storage levels used in Delft3Dflow somehere here.
			retVal = linuxGnuDll.se_store_current_instance_(new IntByReference(level));
		} else {
			resetCWD();
			throw new RuntimeException("createInstance: DLL/so type not known for model");
		}
		resetCWD();
		if (retVal < 0) {
			throw new RuntimeException("Error in D3dFlowDll.storeCurrentInstance(), retVal " + retVal);
		}
	}

	public static void storeCurrentInstanceRestartfile(File modelDir, String restartFileName) {
		setModelDirAsCWD(modelDir);
		int retVal;
		if (platform == D3dFlowModelConfig.DllType.win32_ifort) {
			retVal = winIfortDll.SE_STORE_CURRENT_INSTANCE_RESTARTFILE(restartFileName,restartFileName.length());
		}else if (platform == D3dFlowModelConfig.DllType.linux64_gnu) {
			retVal = linuxGnuDll.se_store_current_instance_restartfile_(restartFileName,restartFileName.length());
		} else {
			resetCWD();
			throw new RuntimeException("createInstance: DLL/so type not known for model");
		}
		resetCWD();
		if (retVal < 0) {
			throw new RuntimeException("Error in D3dFlowDll.storeCurrentInstanceRestartfile(), retVal " + retVal);
		}
	}


	public static void setStateValues(double[] values) {
		int retVal;
		if (platform == D3dFlowModelConfig.DllType.win32_ifort) {
			retVal = winIfortDll.SE_SET_INSTANCE_CORE_STATE(values,new IntByReference(values.length) );
			if (retVal != 0){
				throw new RuntimeException("setStateValues: failed to set statevalues");
			}
		}else if (platform == D3dFlowModelConfig.DllType.linux64_gnu) {
			retVal = linuxGnuDll.se_set_instance_core_state_(values,new IntByReference(values.length) );
			if (retVal != 0){
				throw new RuntimeException("setStateValues: failed to set statevalues");
			}
		} else {
			throw new RuntimeException("setStateValues: DLL/so type not known for model");
		}
	}
	public static double[] getStateValues() {
		if (platform == D3dFlowModelConfig.DllType.win32_ifort) {
			int nvals = getStateSize();
			double[] values = new double[nvals];
			int retVal = winIfortDll.SE_GET_INSTANCE_CORE_STATE(values,new IntByReference(nvals) );
			if (retVal != 0){
				throw new RuntimeException("getStateValues: failed to get statevalues");
			}

			return values;
		}else if (platform == D3dFlowModelConfig.DllType.linux64_gnu) {
			int nvals = getStateSize();
			double[] values = new double[nvals];
			int retVal = linuxGnuDll.se_get_instance_core_state_(values,new IntByReference(nvals) );
			if (retVal != 0){
				throw new RuntimeException("getStateValues: failed to get statevalues");
			}

			return values;
		} else {
			throw new RuntimeException("getStateValues: DLL/so type not known for model");
		}
	}

	public static int  getStateSize() {
		if (platform == D3dFlowModelConfig.DllType.win32_ifort) {
			int size = winIfortDll.SE_GET_INSTANCE_SIZE();
			if (size < 0){
				throw new RuntimeException("getStateSize: failed to get state size for model");
			}
			return size;
		}else if (platform == D3dFlowModelConfig.DllType.linux64_gnu) {
			int size = linuxGnuDll.se_get_instance_size_();
			if (size < 0){
				throw new RuntimeException("getStateSize: failed to get state size for model");
			}
			return size;
		} else {
			throw new RuntimeException("getStateValues: DLL/so type not known for model");
		}
	}

	public static void axpyOnState(double alpha, double[] axpyValues){
		if (platform == D3dFlowModelConfig.DllType.win32_ifort) {

			double[] stateValues = getStateValues();

			if (stateValues.length == axpyValues.length){
				for (int i=0; i < stateValues.length; i++){
					stateValues[i] = alpha*axpyValues[i] + stateValues[i];
				}
				setStateValues(stateValues);

			} else{
				throw new RuntimeException("axpyOnState: size(x): "+stateValues.length+
						" <> size(y): " + axpyValues.length);
			}
		}else if (platform == D3dFlowModelConfig.DllType.linux64_gnu) {

			double[] stateValues = getStateValues();

			if (stateValues.length == axpyValues.length){
				for (int i=0; i < stateValues.length; i++){
					stateValues[i] = alpha*axpyValues[i] + stateValues[i];
				}
				setStateValues(stateValues);

			} else{
				throw new RuntimeException("axpyOnState: size(x): "+stateValues.length+
						" <> size(y): " + axpyValues.length);
			}

		} else {
			throw new RuntimeException("axpyOnState: DLL/so type not known for model");
		}
	}

//	public static int getWindExchangeItemID(String boundaryId, int boundaryType) {
//		return getBoundaryExchangeItemID(boundaryId, boundaryType);
//	}
//
	public static int getBoundaryExchangeItemID(String boundaryId, int boundaryType) {
		int retVal;
		if (platform == D3dFlowModelConfig.DllType.win32_ifort) {
			retVal = winIfortDll.SE_GET_EXCHANGE_ITEM_ID_CI(
					boundaryId, new IntByReference(boundaryType), boundaryId.length());
		} else if (platform == D3dFlowModelConfig.DllType.linux64_gnu) {
			retVal = linuxGnuDll.se_get_exchange_item_id_ci_(
					boundaryId, new IntByReference(boundaryType), boundaryId.length());
		} else {
			throw new RuntimeException("getBoundaryExchangeItemID: DLL/so type not known for model");
		}
		if (retVal < 0) {
			throw new RuntimeException("Error in D3dFlowDll.getBoundaryExchangeItemID, retVal " + retVal);
		}
		return retVal;
	}

	public static int setBoundaryNoise(int boundaryNoiseId, double value, int operation) {
		int retVal;
		double startTime = 0.0;
		double endTime = 0.0;
		int nvals = 1;
		double[] valuearray = new double[]{value};

		if (platform == D3dFlowModelConfig.DllType.win32_ifort) {
			retVal = winIfortDll.SE_SET_NOISE_FOR_TIME_SPAN(new IntByReference(boundaryNoiseId),
					new DoubleByReference(startTime), new DoubleByReference(endTime),
					new IntByReference(operation),
					new IntByReference(nvals), valuearray);
		} else if (platform == D3dFlowModelConfig.DllType.linux64_gnu) {
			retVal = linuxGnuDll.se_set_noise_for_time_span_(new IntByReference(boundaryNoiseId),
					new DoubleByReference(startTime), new DoubleByReference(endTime),
					new IntByReference(operation),
					new IntByReference(nvals), valuearray);
		} else {
			throw new RuntimeException("setBoundaryNoise: DLL/so type not known for model");
		}
		if (retVal < 0) {
			throw new RuntimeException("Error in D3dFlowDll.setBoundaryNoise, retVal " + retVal);
		}
		return retVal;
	}


	public static int setBoundaryGridNoise(int boundaryNoiseId, double alpha, double[] values, int operation) {
		// a crude way for the model to give the metadata (location) is to provide
		// all these locations as well. For the moment, we will implement it this way:
		// the double array can be subdivided into triples of (xloc, yloc, value).
		int retVal ;
		double startTime = 0.0;
		double endTime = 0.0;
		int nvals = values.length/3;
		for (int i = 0; i < nvals; i++) {
			values[3*i+2] = alpha * values[3*i+2]; //only adjust  the values, not the locations
		}
		if (platform == D3dFlowModelConfig.DllType.win32_ifort) {
			retVal = winIfortDll.SE_SET_NOISE_FOR_TIME_SPAN(new IntByReference(boundaryNoiseId),
					new DoubleByReference(startTime), new DoubleByReference(endTime),
					new IntByReference(operation),
					new IntByReference(values.length), values);
		} else if (platform == D3dFlowModelConfig.DllType.linux64_gnu) {
			retVal = linuxGnuDll.se_set_noise_for_time_span_(new IntByReference(boundaryNoiseId),
					new DoubleByReference(startTime), new DoubleByReference(endTime),
					new IntByReference(operation),
					new IntByReference(values.length), values);
		} else {
			throw new RuntimeException("setBoundaryNoise: DLL/so type not known for model");
		}
		if (retVal < 0) {
			throw new RuntimeException("Error in D3dFlowDll.setBoundaryNoise, retVal " + retVal);
		}
		return retVal;
	}




	public static double[] getObservedLocalization(String location, double dist, int nvals) {
		if (platform == D3dFlowModelConfig.DllType.win32_ifort) {
			double[] values = new double[nvals];
			int retVal = winIfortDll.SE_GET_OBSERVED_LOCALIZATION
					( location, new DoubleByReference(dist), new IntByReference(nvals), values, location.length() );
			if (retVal != 0){
				throw new RuntimeException("getStateValues: failed to get statevalues ");
			}
			return values;
		} else if (platform == D3dFlowModelConfig.DllType.linux64_gnu) {
			double[] values = new double[nvals];
			int retVal = linuxGnuDll.se_get_observed_localization_
					( location, new DoubleByReference(dist), new IntByReference(nvals), values, location.length() );
			if (retVal != 0){
				throw new RuntimeException("getStateValues: failed to get statevalues ");
			}
			return values;
		} else {
			throw new RuntimeException("getStateValues: DLL/so type not known for model");
		}
	}

	public static double getResultValue(int monitorpointId) {
		int retVal;
		double startTime = 0.0;
		double endTime = 0.0;
		int nvals = 1;
		double[] valuearray = new double[nvals];

		if (platform == D3dFlowModelConfig.DllType.win32_ifort) {
			retVal = winIfortDll.SE_GET_VALUES_FOR_TIME_SPAN(new IntByReference(monitorpointId),
					new DoubleByReference(startTime), new DoubleByReference(endTime),
					new IntByReference(nvals), valuearray);
			if (retVal < 0) {
				throw new RuntimeException("Error in D3dFlowDll.getValue, retVal " + retVal);
			}
			return valuearray[0];
		} else if (platform == D3dFlowModelConfig.DllType.linux64_gnu) {
			retVal = linuxGnuDll.se_get_values_for_time_span_(new IntByReference(monitorpointId),
					new DoubleByReference(startTime), new DoubleByReference(endTime),
					new IntByReference(nvals), valuearray);
			if (retVal < 0) {
				throw new RuntimeException("Error in D3dFlowDll.getValue, retVal " + retVal);
			}
			return valuearray[0];
		} else {
			throw new RuntimeException("getValue: DLL/so type not known for model");
		}
	}



	public static ITime getTimeHorizon() {
		int retVal;
		DoubleByReference startTime = new DoubleByReference(Double.NaN);
		DoubleByReference endTime = new DoubleByReference(Double.NaN);
		if (platform == D3dFlowModelConfig.DllType.win32_ifort) {
			retVal = winIfortDll.SE_GETTIMEHORIZON(componentID, modelIdentifier,
					startTime, endTime, componentID.length(), modelIdentifier.length());
		} else if (platform == D3dFlowModelConfig.DllType.linux64_gnu) {
			retVal = linuxGnuDll.se_gettimehorizon_(componentID, modelIdentifier,
					startTime, endTime, componentID.length(), modelIdentifier.length());
		} else {
			throw new RuntimeException("getTimeHorizon: DLL/so type not known for model");
		}
		if (retVal != 0) {
			throw new RuntimeException("Error in D3dFlowDll.getTimeHorizon(), retVal " + retVal);
		}
		return new Time(startTime.getValue(), endTime.getValue());
	}

	public static double getCurrentTime() {
		DoubleByReference currentTime = new DoubleByReference(Double.NaN);
		if (platform == D3dFlowModelConfig.DllType.win32_ifort) {
			winIfortDll.SE_GETCURRENTTIME(componentID, modelIdentifier, currentTime,
					componentID.length(), modelIdentifier.length());
		} else if (platform == D3dFlowModelConfig.DllType.linux64_gnu) {
			linuxGnuDll.se_getcurrenttime_(componentID, modelIdentifier, currentTime,
					componentID.length(), modelIdentifier.length());
		} else {
			throw new RuntimeException("getCurrentTime: DLL/so type not known for model");
		}
		return currentTime.getValue();
	}

	public static void performTimeStep(File modelDir, int timeStep) {
		setModelDirAsCWD(modelDir);
		int retVal;
		if (platform == D3dFlowModelConfig.DllType.win32_ifort) {
			IntByReference timeStepByRef = new IntByReference(timeStep);
			retVal = winIfortDll.SE_PERFORMTIMESTEP(componentID, modelIdentifier, timeStepByRef,
					componentID.length(), modelIdentifier.length());
		} else if (platform == D3dFlowModelConfig.DllType.linux64_gnu) {
			IntByReference timeStepByRef = new IntByReference(timeStep);
			retVal = linuxGnuDll.se_performtimestep_(componentID, modelIdentifier, timeStepByRef,
					componentID.length(), modelIdentifier.length());
		} else {
			resetCWD();
			throw new RuntimeException("performTimeStep: DLL/so type not known for model");
		}
		resetCWD();
		if (retVal != 0) {
			throw new RuntimeException("Error in D3dFlowDll.performTimeStep(), retVal " + retVal);
		}
	}

	public static int finish(File workingDir) {
		setModelDirAsCWD(workingDir);
		int retVal;
		if (platform == D3dFlowModelConfig.DllType.win32_ifort) {
			retVal = winIfortDll.SE_FINALIZE_OPENDA(componentID, modelIdentifier,
					componentID.length(), modelIdentifier.length());
		} else if (platform == D3dFlowModelConfig.DllType.linux64_gnu) {
			retVal = linuxGnuDll.se_finalize_openda_(componentID, modelIdentifier,
					componentID.length(), modelIdentifier.length());
		} else {
			resetCWD();
			throw new RuntimeException("finalize: DLL/so type not known for model");
		}
		resetCWD();
		if (retVal != 0) {
			throw new RuntimeException("Error in D3dFlowDll.finish(), retVal " + retVal);
		}
		return retVal;
	}

	// Windows kernel (for changing current directory),
	// and methods to set / reset current working directory

	public interface Kernel32DLL extends Library {
		int SetCurrentDirectoryA(String directoryPath);
	}

	private static Kernel32DLL kernel32DLL = null;

	static {
		if (BBUtils.RUNNING_ON_WINDOWS) {
			kernel32DLL = (Kernel32DLL) Native.loadLibrary("kernel32", Kernel32DLL.class);
		}
	}

	// libc (for changing current directory),
	// and methods to set / reset current working directory

	public interface LibcDLL extends Library {
		// c-def: int chdir (const char *filename)
		int chdir(String directoryPath);
	}

	private static LibcDLL libcDLL = null;

	static {
		if (BBUtils.RUNNING_ON_LINUX) {
			libcDLL = (LibcDLL) Native.loadLibrary("libc", LibcDLL.class);
		}
	}

	private static String curDir = null;

	private static void setModelDirAsCWD(File workingDir) {
		if (BBUtils.RUNNING_ON_WINDOWS) {
			curDir = new File(".").getAbsolutePath();
			kernel32DLL.SetCurrentDirectoryA(workingDir.getAbsolutePath());
		}else if(BBUtils.RUNNING_ON_LINUX){
			libcDLL.chdir(workingDir.getAbsolutePath());
		}else{
			throw new RuntimeException("Change Dir not implemented.");
		}
	}

	private static void resetCWD() {
		if (curDir != null) {
			kernel32DLL.SetCurrentDirectoryA(curDir);
			curDir = null;
		}
	}

	private interface IDummyDll extends Library {
	}
}
