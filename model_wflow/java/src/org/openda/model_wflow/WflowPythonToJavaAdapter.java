/* OpenDA v2.4 
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

package org.openda.model_wflow;

import java.io.File;
import java.lang.Integer;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.TimeZone;

import jep.Jep;
import jep.JepException;

import java.lang.reflect.Field;
import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.DoubleBuffer;
import java.util.HashMap;
import java.util.Map;


import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.ITime;
import org.openda.utils.Time;

/**
 * Adapter class to call the WFLOW model Python methods from Java.
 * This class uses the JEPP (Java Embedded Python) framework, see http://jepp.sourceforge.net/
 *
 * For this code to work, CPython and PCRaster need to be installed.
 * For this code to work, the following folders need to be present in the corresponding environment variables:
 * PATH: folder with jep.dll file (e.g. openda_bin\win32_ifort), folder with PCRaster dll files (e.g. C:\PCRaster\apps), folder with Python (e.g. C:\python25).
 * PYTHONPATH: folder with WFLOW python scripts (e.g. wflow_bin) and folder with PCRaster python scripts (e.g. C:\PCRaster\Python).
 *
 * For information about the WFLOW model see www.openstreams.org
 * and https://publicwiki.deltares.nl/display/OpenS/wflow+-+PCRaster-Python+based+distributed+hydrological+models
 * For information about the PCRaster framework see http://pcraster.geo.uu.nl/
 *
 * @author Arno Kockx
 */
public class WflowPythonToJavaAdapter {
	private final Jep jep;
	private final DoubleArrayWrapper doubleArrayWrapper = new DoubleArrayWrapper();

	/**
	 * Create a WflowPythonToJavaAdapter.
	 * This class creates a single session of the Python interpreter
	 * that is re-used until the close method is called.
	 */
	public WflowPythonToJavaAdapter() {
		try {
            //new Jep(boolean interactive).
			this.jep = new Jep(false);
		} catch (JepException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": exception during initialization of Jepp."
					+ " Message was: " + e.getMessage(), e);
		}
	}

	/**
	 * @param pythonModuleNameOfModelToUse
	 */
	public void performPythonImports(String pythonModuleNameOfModelToUse) {
		try {
			this.jep.eval("import " + pythonModuleNameOfModelToUse);
			this.jep.eval("import wflow_adapt");
			this.jep.eval("from wflow_adapt import *");
		} catch (JepException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": exception during call to python code. Message was: " + e.getMessage(), e);
		}
	}

	/**
	 * From wflow_hbv.py usage:
	 * caseName: the name of the case (directory) to run.
	 * runId: the name of the runId within the current case.
	 * configFileName: name of the wflow configuration file (default: caseName/wflow_hbv.ini).
	 *
	 * @param pythonModuleNameOfModelToUse
	 * @param caseDirectory full path of the case directory of the current case (without any trailing \ or / sign).
	 * @param runId name of the runId within the current case (relative to the caseDirectory).
	 * @param configFileName name of the model configuration file (relative to the caseDirectory).
	 * @param cloneMapFileName name of the map file that describes the catchment (relative to the staticmaps/ folder in the caseDirectory).
	 */
	public void createWflowModel(String pythonModuleNameOfModelToUse, File caseDirectory,
			String runId, String configFileName, String cloneMapFileName) {
		try {
			this.jep.set("cloneMapFileName", cloneMapFileName);
			String caseDirectoryPath = BBUtils.getFilePathStringForPython(caseDirectory);
			this.jep.set("caseDirectory", caseDirectoryPath);
			this.jep.set("runId", runId);
			this.jep.set("configFileName", configFileName);
   			this.jep.eval("myModel = " + pythonModuleNameOfModelToUse + ".WflowModel(cloneMapFileName, caseDirectory, runId, configFileName)");
		} catch (JepException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": exception during call to python code. Message was: " + e.getMessage(), e);
		}
	}

	/**
	 * Comments from wf_DynamicFramework.py:
	 *   ## \brief Constructor
	 *   #
	 *   # \param userModel class containing the user model
	 *   # \param lastTimeStep last timestep to run
	 *   # \param firstTimestep sets the starting timestep of the model (optional,
	 *   #		default is 1)
	 *   #
	 *   def __init__(self, userModel, lastTimeStep=0, firstTimestep=1):
	 *
	 * @param pythonModuleNameOfModelToUse
	 * @param numberOfTimeSteps for the model run.
	 */
	public void createWfDynamicFramework(String pythonModuleNameOfModelToUse, int numberOfTimeSteps) {
		try {
			this.jep.set("lastTimeStepNumber", numberOfTimeSteps);
			this.jep.set("firstTimeStepNumber", 1);
			this.jep.eval("dynModelFw = " + pythonModuleNameOfModelToUse + ".wf_DynamicFramework(myModel, lastTimeStepNumber, firstTimeStepNumber)");
		} catch (JepException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": exception during call to python code. Message was: " + e.getMessage(), e);
		}
	}

	/**
	 * From wf_DynamicFramework.py:
	 * def createRunId(self,intbl="intbl",logfname="wflow.log",NoOverWrite=True,model="model",modelVersion="no version"):
	 *   """
	 *   Create runId dir and copy table files to it
	 *   Also changes the working dir to the case/runid directory
	 *   """
	 *
	 * @param pythonModuleNameOfModelToUse
	 */
	public void createRunId(String pythonModuleNameOfModelToUse) {
		try {
			this.jep.set("intbl", "intbl");
			this.jep.set("logfname", pythonModuleNameOfModelToUse + ".log");
			this.jep.set("NoOverWrite", true);
			//here modelName is only used for logging.
			this.jep.set("modelName", pythonModuleNameOfModelToUse);
			//here modelVersion is only used for logging.
			this.jep.set("modelVersion", "0.9");
			this.jep.eval("dynModelFw.createRunId(intbl, logfname, NoOverWrite, modelName, modelVersion)");
		} catch (JepException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": exception during call to python code. Message was: " + e.getMessage(), e);
		}
	}

	/**
	 * Initializes the model.
	 */
	public void runInitial() {
		try {
			//the general method frameworkBase._runInitial eventually calls the
			//model specific method WflowModel.initial.
			this.jep.eval("dynModelFw._runInitial()");
		} catch (JepException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": exception during call to python code. Message was: " + e.getMessage(), e);
		}
	}

	/**
	 * From wf_DynamicFramework.py:
	 *   def _runDynamic(self,firststep,laststep):
	 *
	 * Runs the model from the given startTimeStep (inclusive) until the given endTimeStep (inclusive).
	 * The timeStep with number 1 starts at time=0 and ends at time=timeStepLength
	 * where timeStepLength is the length of one timeStep.
	 *
	 * @param startTimeStep
	 * @param endTimeStep
	 */
	public void runDynamic(int startTimeStep, int endTimeStep) {
		try {
			//the general method wf_DynamicFramework._runDynamic eventually calls the
			//model specific method WflowModel.dynamic.
			this.jep.set("firstTimeStep", startTimeStep);
			this.jep.set("lastTimeStep", endTimeStep);
			this.jep.eval("dynModelFw._runDynamic(firstTimeStep, lastTimeStep)");
			this.jep.eval("import gc");  //added by albrecht
			this.jep.eval("gc.collect()");  //added by albrecht
		} catch (JepException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": exception during call to python code. Message was: " + e.getMessage(), e);
		}
	}

	/**
	 * From wf_DynamicFramework.py:
	 *   def wf_suspend(self, directory):
	 *     """
	 *     Suspend the state variables to disk as .map files
	 *     """
	 *
	 * Saves the current state of the model to disk.
	 */
	public void runSuspend() {
		try {
			//the general method frameworkBase._runSuspend eventually calls the model specific method
			//WflowModel.suspend which calls the general method wf_DynamicFramework.wf_suspend.
			this.jep.eval("dynModelFw._runSuspend()");
		} catch (JepException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": exception during call to python code. Message was: " + e.getMessage(), e);
		}
	}

	/**
	 * From wf_DynamicFramework.py:
	 *  def wf_resume(self, directory):
	 *    """
	 *    Resumes the state variables from disk as .map files
	 *    """
	 *
	 * Reads the current state of the model from disk.
	 */
	public void runResume() {
		try {
			//the general method frameworkBase._runResume eventually calls the model specific method
			//WflowModel.resume which calls the general method wf_DynamicFramework.wf_resume.
			this.jep.eval("dynModelFw._runResume()");
		} catch (JepException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": exception during call to python code. Message was: " + e.getMessage(), e);
		}
	}

	/**
	 * From wf_DynamicFramework.py:
	 *  def wf_QuickSuspend(self):
	 *    """
	 *    Save the state variable of the current timestep in memory
	 *    it uses the wf_supplyVariableNamesAndRoles() function to find them.
	 *    The variables are inserted into the model object
	 *    This function is normally caled as part of the run. Normally there is
	 *    no need to call it directly.
	 *    """
	 *
	 * Saves the current state of the model to memory. Overwrites the state that was last saved to memory.
	 */
	public void runQuickSuspend() {
		try {
			this.jep.eval("dynModelFw.wf_QuickSuspend()");
		} catch (JepException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": exception during call to python code. Message was: " + e.getMessage(), e);
		}
	}

	/**
	 * From wf_DynamicFramework.py:
	 *  def wf_QuickResume(self):
	 *    """
	 *    Resumes the state variable of the previous timestep in memory
	 *    it uses the wf_supplyVariableNamesAndRoles() function to find them.
	 *    The variables are inserted into the model object
	 *    """
	 *
	 * Reads the current state of the model from memory. The state that was last saved to memory is used.
	 */
	public void runQuickResume() {
		try {
			this.jep.eval("dynModelFw.wf_QuickResume()");
		} catch (JepException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": exception during call to python code. Message was: " + e.getMessage(), e);
		}
	}

	/**
	 * Returns the time of the current model timeStep.
	 *
	 * @param timeHorizon ITime object that includes the startTime and endTime of the model run.
	 * @return ITime currentTime.
	 */
	public ITime getCurrentTime(ITime timeHorizon) {
		//get time in milliseconds after the start of the run.
		long milliesAfterStartTime = getCurrentTimeAfterStartOfRun();

		//return absolute time.
		long startTimeMillies = Time.mjdToMillies(timeHorizon.getBeginTime().getMJD());
		return new Time(Time.milliesToMjd(startTimeMillies + milliesAfterStartTime));
	}

	/**
	 * From wf_DynamicFramework.py:
	 *  def wf_supplyCurrentTime(self):
	 *    """
	 *    gets the current time in seconds after the start of the run
	 *    Assumed daily timesteps if not defined in the user model
	 *    """
	 *
	 * @return currentTime in milliseconds after the start of the run.
	 */
	private long getCurrentTimeAfterStartOfRun() {
		try {
			//return time in milliseconds.
			return 1000 * (Integer) this.jep.getValue("dynModelFw.wf_supplyCurrentTime()");
		} catch (JepException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": exception during call to python code. Message was: " + e.getMessage(), e);
		}
	}

	/**
	 * From wf_DynamicFramework.py:
	 *  def wf_supplyGridDim(self):
	 *    """
	 *    return the dimension of the current model grid as list
	 *     [ Xul, Yul, xsize, ysize, rows, cols]
	 *    """
	 *
	 * @return x coordinate of first grid cell.
	 */
	public double getFirstGridCellCenterX() {
		String string;
		try {
			string = (String) this.jep.getValue("dynModelFw.wf_supplyGridDim()[0]");
		} catch (JepException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": exception during call to python code. Message was: " + e.getMessage(), e);
		}

		try {
			return Double.parseDouble(string);
		} catch (NumberFormatException e) {
			throw new IllegalStateException("The wflow model returns an invalid value for x coordinate of first grid cell: '"
					+ string + "'.");
		}
	}

	/**
	 * From wf_DynamicFramework.py:
	 *  def wf_supplyGridDim(self):
	 *    """
	 *    return the dimension of the current model grid as list
	 *     [ Xul, Yul, xsize, ysize, rows, cols]
	 *    """
	 *
	 * @return y coordinate of first grid cell.
	 */
	public double getFirstGridCellCenterY() {
		String string;
		try {
			string = (String) this.jep.getValue("dynModelFw.wf_supplyGridDim()[1]");
		} catch (JepException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": exception during call to python code. Message was: " + e.getMessage(), e);
		}

		try {
			return Double.parseDouble(string);
		} catch (NumberFormatException e) {
			throw new IllegalStateException("The wflow model returns an invalid value for y coordinate of first grid cell: '"
					+ string + "'.");
		}
	}

	/**
	 * From wf_DynamicFramework.py:
	 *  def wf_supplyGridDim(self):
	 *    """
	 *    return the dimension of the current model grid as list
	 *     [ Xul, Yul, xsize, ysize, rows, cols]
	 *    """
	 *
	 * @return x size of grid cell.
	 */
	public double getGridCellWidth() {
		String string;
		try {
			string = (String) this.jep.getValue("dynModelFw.wf_supplyGridDim()[2]");
		} catch (JepException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": exception during call to python code. Message was: " + e.getMessage(), e);
		}

		try {
			return Double.parseDouble(string);
		} catch (NumberFormatException e) {
			throw new IllegalStateException("The wflow model returns an invalid value for x size of grid cell: '" + string + "'.");
		}
	}

	/**
	 * From wf_DynamicFramework.py:
	 *  def wf_supplyGridDim(self):
	 *    """
	 *    return the dimension of the current model grid as list
	 *     [ Xul, Yul, xsize, ysize, rows, cols]
	 *    """
	 *
	 * @return y size of grid cell.
	 */
	public double getGridCellHeight() {
		String string;
		try {
			string = (String) this.jep.getValue("dynModelFw.wf_supplyGridDim()[3]");
		} catch (JepException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": exception during call to python code. Message was: " + e.getMessage(), e);
		}

		try {
			return Double.parseDouble(string);
		} catch (NumberFormatException e) {
			throw new IllegalStateException("The wflow model returns an invalid value for y size of grid cell: '" + string + "'.");
		}
	}

	/**
	 * From wf_DynamicFramework.py:
	 *  def wf_supplyGridDim(self):
	 *    """
	 *    return the dimension of the current model grid as list
	 *     [ Xul, Yul, xsize, ysize, rows, cols]
	 *    """
	 *
	 * @return number of rows of the model grid.
	 */
	public int getRowCount() {
		try {
            Object row = this.jep.getValue("dynModelFw.wf_supplyGridDim()[4]");
            return castLongToInt(row);
		} catch (JepException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": exception during call to python code. Message was: " + e.getMessage(), e);
		}
	}

    /**
	 * From wf_DynamicFramework.py:
	 *  def wf_supplyGridDim(self):
	 *    """
	 *    return the dimension of the current model grid as list
	 *     [ Xul, Yul, xsize, ysize, rows, cols]
	 *    """
	 *
	 * @return number of columns of the model grid.
	 */
	public int getColumnCount() {
		try {
            Object column = this.jep.getValue("dynModelFw.wf_supplyGridDim()[5]");
            return castLongToInt(column);
//			return (Integer) this.jep.getValue("dynModelFw.wf_supplyGridDim()[5]");
		} catch (JepException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": exception during call to python code. Message was: " + e.getMessage(), e);
		}
	}

	/**
	 * From wf_DynamicFramework.py:
	 *  def wf_supplyVariableNames(self):
	 *    """
	 *    returns the a list of variable names
	 *    """
	 *
	 * @return String[] variable names.
	 */
	public String[] getVariableNames() {
		try {
			//method jep.getValue for some reason converts a list of Strings to a single String,
			//therefore here pass an ArrayList to Python, put the strings into
			//that ArrayList and get that ArrayList back to Java.
			this.jep.eval("tempVariableNames = dynModelFw.wf_supplyVariableNames()");
			ArrayList<String> stringsList = new ArrayList<String>();
			this.jep.set("stringsList", stringsList);
			this.jep.eval("for i in range(len(tempVariableNames)): stringsList.add(tempVariableNames[i])\n");
			@SuppressWarnings("unchecked")
			ArrayList<String> list = (ArrayList<String>) this.jep.getValue("stringsList");
			return list.toArray(new String[list.size()]);
		} catch (JepException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": exception during call to python code. Message was: " + e.getMessage(), e);
		}
	}

	/**
	 * From wf_DynamicFramework.py:
	 *  def wf_supplyVariableRoles(self):
	 *    """
	 *    returns the a list of variable roles
	 *    """
	 *
	 * From wflow_hbv.py:
	 *  def supplyVariableNamesAndRoles(self):
	 *    role: 0 = input (to the model)
	 *          1 = is output (from the model)
	 *          2 = input/output (state information)
	 *          3 = model parameter
	 *
	 * @return int[] variable roles.
	 */
	public int[] getVariableRoles() {
		try {
			//method jep.getValue for some reason converts a list of integers to a single String,
			//therefore here pass a buffer array of type int[] to Python, copy the integers into
			//that array and get that array back to Java.
			this.jep.eval("tempVariableRoles = dynModelFw.wf_supplyVariableRoles()");
			int numberOfVariableRoles = (Integer) this.jep.getValue("len(tempVariableRoles)");
			int[] integersBuffer = new int[numberOfVariableRoles];
			this.jep.set("integersBuffer", integersBuffer);
			this.jep.eval("for i in range(len(tempVariableRoles)): integersBuffer[i] = tempVariableRoles[i]\n");
			return (int[]) this.jep.getValue("integersBuffer");
		} catch (JepException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": exception during call to python code. Message was: " + e.getMessage(), e);
		}
	}

	/**
	 * From wf_DynamicFramework.py:
	 *  def wf_supplyVariableUnits(self):
	 *    """
	 *    returns the a list of variable units
	 *    """
	 *
	 * From wflow_hbv.py:
	 *  def supplyVariableNamesAndRoles(self):
	 *  unit: 0 = mm/timestep
	 *        1 = m^3/sec
	 *        2 = m
	 *        3 = degree Celcius
	 *        4 = mm
	 *        5 = -
	 *
	 * @return int[] variable units.
	 */
	public int[] getVariableUnits() {
		try {
			//method jep.getValue for some reason converts a list of integers to a single String,
			//therefore here pass a buffer array of type int[] to Python, copy the integers into
			//that array and get that array back to Java.
			this.jep.eval("tempVariableUnits = dynModelFw.wf_supplyVariableUnits()");
			int numberOfVariableUnits = (Integer) this.jep.getValue("len(tempVariableUnits)");
			int[] integersBuffer = new int[numberOfVariableUnits];
			this.jep.set("integersBuffer", integersBuffer);
			this.jep.eval("for i in range(len(tempVariableUnits)): integersBuffer[i] = tempVariableUnits[i]\n");
			return (int[]) this.jep.getValue("integersBuffer");
		} catch (JepException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": exception during call to python code. Message was: " + e.getMessage(), e);
		}
	}

	/**
	 * From wf_DynamicFramework.py:
	 *  def wf_supplyMapAsList(self,mapname):
	 *    """
	 *    Returns a python list for the specified map and the current
	 *    timestep. If the maps is not dynamic the current status of the map is
	 *    returned which may be undefined for maps that are filled with data
	 *    at the end of a run
	 *    Input: mapname (string)
	 *    Return: list
	 *    """
	 *
	 * The values array contains values for all grid cells in a 1D array.
	 * This 1D array contains first the values for the first row, then the values
	 * for the second row, etc.
	 *
	 * @param variableName
	 * @return values array.
	 */
	public double[] getMapAsList(String variableName) {
		try {
            jep.eval("from numpy import *");
            jep.eval("from pcraster import *");
            String mapName = "dynModelFw._userModel()." + variableName;
            double[] buffer = new double[getMapElementCount(jep, mapName)];
            readPcRasterMapValues(jep, mapName, buffer);
            return buffer;
		} catch (JepException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": exception during call reading map as list for " + variableName + ", Message was: " + e.getMessage(), e);
		}
	}

	/**
	 * From wf_DynamicFramework.py:
	 *  def wf_setValues(self,mapname,values):
	 *    """
	 *    set a map with values from a python list or a single scalar
	 *    value. In case a single value is specified the value will be distributed
	 *    uniformly over the map. Current settings for
	 *    dimensions are assumed.
	 *
	 *    Input: mapname - string with name of map
	 *           values - single list of value of length rows * cols or a single
	 *           scalar
	 *    """
	 *
	 * @param variableName
	 * @param values to set.
	 */
	public void setValues(String variableName, double[] values) {
		try {
			this.jep.set("variableName", variableName);
			//set values in a fixed DoubleArrayWrapper object and pass that to jep.set method, so that we can use
			//the same DoubleArrayWrapper object in each call to this method, to avoid a memory leak in the Jep code.
			//Otherwise if we would call this.jep.set("tempDoubleArray", tempDoubleArray) with a different
			//instance of double[] each time, then the Jep library code would somewhere keep references
			//to all these instances, even if the old instances would no longer be needed because
			//each new instance overwrites the same Python variable "tempDoubleArray" in the Python interpreter.
			this.doubleArrayWrapper.setValues(values);
			this.jep.set("tempDoubleArrayWrapper", this.doubleArrayWrapper);
			this.jep.eval("tempMapValuesToSet = []");
			//the doubleArrayWrapper stores a Java double[] array object, which is not recognized
			//as a proper list object in the Python code. Therefore copy all double values one by one
			//to a Python list object that can be used without problems in the Python code.
			//TODO test performance when not using append here. AK
			this.jep.eval("for i in range(tempDoubleArrayWrapper.getSize()): tempMapValuesToSet.append(tempDoubleArrayWrapper.getValue(i))\n");
			this.jep.eval("dynModelFw.wf_setValues(variableName, tempMapValuesToSet)");
		} catch (JepException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": exception during call to python code. Message was: " + e.getMessage(), e);
		}
	}

	/**
	 * See wflow_adapt.py:
	 *  # Try and read config file and set default options
	 *  config = ConfigParser.SafeConfigParser()
	 *  config.optionxform = str
	 *  config.read(case + "/" + iniFile)
	 *
	 *  # Get outpumapstacks from wflow ini
	 *  mstacks  = config.options('outputmaps')
	 *  for a in mstacks:
	 *     var = config.get("outputmaps",a)
	 *
	 * @return String[] outmap names.
	 */
	public String[] getOutputMapNamesFromConfig(File configFile) {
		try {
			this.jep.eval("tempConfig = ConfigParser.SafeConfigParser()");
			this.jep.eval("tempConfig.optionxform = str");
			String configFilePath = BBUtils.getFilePathStringForPython(configFile);
			this.jep.set("configFilePath", configFilePath);
			this.jep.eval("tempConfig.read(configFilePath)");
			this.jep.eval("tempOutputMaps = tempConfig.options('outputmaps')");
			int numberOfOutputMaps = (Integer) this.jep.getValue("len(tempOutputMaps)");
			String[] outputMapNames = new String[numberOfOutputMaps];
			for (int i = 0; i < outputMapNames.length; i++) {
				this.jep.set("tempIndex", i);
				outputMapNames[i] = (String) this.jep.getValue("tempConfig.get('outputmaps', tempOutputMaps[tempIndex])");
			}
			return outputMapNames;
		} catch (JepException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": exception during call to python code. Message was: " + e.getMessage(), e);
		}
	}

	/**
	 * From wflow_adapt.py:
	 *  def mapstackxml(mapstackxml,mapstackname,locationname,parametername,Sdate,Edate,timestepsecs):
	 *    """
	 *    writes mapstack file
	 *    """
	 *
	 * @param mapStackXmlFile absolute path of the map stack xml file to write.
	 * @param mapStackFileNamePattern name pattern of the files that contain the map stack data, e.g. "TEMP0000????.???".
	 * @param locationId
	 * @param parameterId
	 * @param timeHorizon ITime object that includes the startTime and endTime of the model run.
	 * @param timeStepLength in milliseconds.
	 */
	public void writePiMapStackXmlFile(File mapStackXmlFile, String mapStackFileNamePattern,
			String locationId, String parameterId, ITime timeHorizon, long timeStepLength) {
		try {
			String mapStackXmlFilePath = BBUtils.getFilePathStringForPython(mapStackXmlFile);
			this.jep.set("mapStackXmlFile", mapStackXmlFilePath);
			this.jep.set("mapStackName", mapStackFileNamePattern);
			this.jep.set("locationName", locationId);
			this.jep.set("parameterName", parameterId);
			//create python datetime objects for start and end time.
			long startTime = Time.mjdToMillies(timeHorizon.getBeginTime().getMJD());
			this.jep.eval("startDate = " + getPythonDateTimeObjectCreationString(startTime));
			long endTime = Time.mjdToMillies(timeHorizon.getEndTime().getMJD());
			this.jep.eval("endDate = " + getPythonDateTimeObjectCreationString(endTime));
			//convert time step length to seconds.
			this.jep.set("timeStepSecs", (int) timeStepLength/1000);
			this.jep.eval("wflow_adapt.mapstackxml(mapStackXmlFile, mapStackName, locationName, parameterName, startDate, endDate, timeStepSecs)");
		} catch (JepException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": exception during call to python code. Message was: " + e.getMessage(), e);
		}
	}

	public void close() {
		this.jep.close();
	}

	/**
	 * Very thin wrapper around a double[] array.
	 */
	private final class DoubleArrayWrapper {
		double[] values = new double[]{};

		/**
		 * @param values the values to set.
		 */
		public void setValues(double[] values) {
			this.values = values;
		}

		/**
		 * @return values array.
		 */
		public double[] getValues() {
			return this.values;
		}

		/**
		 * @param index
		 * @param value to set.
		 */
		@SuppressWarnings("unused")
		public void setValue(int index, double value) {
			this.values[index] = value;
		}

		/**
		 * @param index
		 * @param value to set.
		 */
		@SuppressWarnings("unused")
		public void setValue(int index, int value) {
			this.values[index] = value;
		}

		/**
		 * @param index
		 * @return value.
		 */
		@SuppressWarnings("unused")
		public double getValue(int index) {
			return this.values[index];
		}

		/**
		 * @return length of values array.
		 */
		@SuppressWarnings("unused")
		public int getSize() {
			return this.values.length;
		}
	}

	private static String getPythonDateTimeObjectCreationString(long time) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTimeZone(TimeZone.getTimeZone("GMT"));
		calendar.setTimeInMillis(time);

		StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append("datetime(").append(calendar.get(Calendar.YEAR)).append(", ")
				.append(calendar.get(Calendar.MONTH) + 1).append(", ")
				.append(calendar.get(Calendar.DAY_OF_MONTH)).append(", ")
				.append(calendar.get(Calendar.HOUR_OF_DAY)).append(", ")
				.append(calendar.get(Calendar.MINUTE)).append(", ")
				.append(calendar.get(Calendar.SECOND)).append(")");

		return stringBuilder.toString();
	}


    // added by Onno

    public static void declareDoubleArray(Jep jep, String variableName, DoubleBuffer buffer) throws JepException {
        jep.eval("from numpy import *");
        jep.eval("from ctypes import *");
        jep.eval("PyBuffer_FromMemory = pythonapi.PyBuffer_FromReadWriteMemory");
        jep.eval("PyBuffer_FromMemory.restype = py_object");
        jep.eval("PyBuffer_FromMemory.argtypes =[c_void_p, c_int32]");
        jep.eval(variableName + "=frombuffer(PyBuffer_FromMemory(" + getAddress(buffer) + ", " + buffer.limit() * 8 + "), float64)");
    }

    //this should only be called once per numberOfElements grid size,
    //so that the same array in memory is re-used for all model instances,
    //which is possible as long as model instances do not run in parallel.
    public static DoubleBuffer createNativeDoubleBuffer(int numberOfElements) {
        ByteBuffer res = ByteBuffer.allocateDirect(numberOfElements * 8);
        res.order(ByteOrder.nativeOrder());
        return res.asDoubleBuffer();
    }

    private static Field addressField = null;

    public static long getAddress(Buffer buffer) {
        if (!buffer.isDirect())
            throw new IllegalArgumentException("!buffer.isDirect()");

        if (addressField == null) {
            try {
                addressField = Buffer.class.getDeclaredField("address");
                addressField.setAccessible(true);
            } catch (NoSuchFieldException e) {
                throw new RuntimeException(e);
            }
        }

        try {
            return addressField.getLong(buffer);
        } catch (IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }

    // todo declare buffer per thread
    private static Map<Integer, DoubleBuffer> buffers = new HashMap<Integer, DoubleBuffer>();
    private static Map<Jep, Map<Integer, DoubleBuffer>> readPcRasterMapValuesBuffers = new HashMap<Jep, Map<Integer, DoubleBuffer>>();

    public static int getMapElementCount(Jep jep, String variableName) throws JepException {
        // todo can be faster if we know  the pcr function to get the cell count
        // for only call the first time step
        jep.eval("getMapElementCount=pcr2numpy(" + variableName + ", 1E31)");
        Object value = jep.getValue("getMapElementCount.shape[0]*getMapElementCount.shape[1]");
        return castLongToInt(value);
//        return (Integer) jep.getValue("getMapElementCount.shape[0]*getMapElementCount.shape[1]");
    }

    public static synchronized void readPcRasterMapValues(Jep jep, String variableName, double[] dest) throws JepException {
        String bufferName = "readPcRasterMapValues" + dest.length;
        Map<Integer, DoubleBuffer> map = readPcRasterMapValuesBuffers.get(jep);
        if (map == null) {
            map = new HashMap<Integer, DoubleBuffer>();
            readPcRasterMapValuesBuffers.put(jep, map);
        }
        DoubleBuffer buffer = map.get(dest.length);
        if (buffer == null) {
            buffer = buffers.get(dest.length);
            if (buffer == null) {
                buffer = createNativeDoubleBuffer(dest.length);
                buffers.put(dest.length, buffer);
            }
            declareDoubleArray(jep, bufferName, buffer);
            map.put(dest.length, buffer);
        }
        jep.eval(bufferName + "[:]=reshape(pcr2numpy(" + variableName + ", 1E31), " + dest.length + ')');
        buffer.rewind();
        buffer.get(dest);
    }

    /**
     * For windows 64bit, jep.getValue turns out to return a Long object instead of Integer. So, it is not possible
     * to use the earlier trick of casting the output of jep.getValue directly to Integer for the 64bit version. The
     * following method is created to safely cast Long to Integer when needed. When the Long value is beyond the range
     * of Integer, error is generated.
     */
    private static int castLongToInt(Object value) {
        int value_int;
        if (value instanceof Integer) {
            value_int = (Integer) value;
        } else if (value instanceof Long){
            if ((Long) value < Integer.MIN_VALUE || (Long) value > Integer.MAX_VALUE){
                throw new IllegalArgumentException("Value "+ value + " is beyond the allowed range of Integer. It cannot be cast to Integer without changing its value.");
            }
            value_int = ((Long) value).intValue();
        } else {
            throw new IllegalArgumentException("Object "+ value.getClass() + " is not an instance of Long nor Integer. Only Integer and Long are supported so far.");
        }
        return value_int;
    }

}
