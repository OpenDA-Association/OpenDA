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
package org.openda.model_bmi;

import bmi.EBMI;

import java.io.File;

/**
 * @author Niels Drost
 *
 */
public class PcrglobwbBridgeTest extends AbstractModelBridgeTest {

	protected void setUp() throws Exception {
		super.setUp();

		shape = new int[] { 13, 17 };
		variableSize = shape[0] * shape[1];
		gridSpacing = new double[] { 0.5, 0.5 };
		variableName = "top_layer_soil_saturation";
		variableType = "float64";

		componentName = "pcrglobwb";

		inputVarNames = new String[] { "top_layer_soil_saturation" };
		outputVarNames = new String[] { "top_layer_soil_saturation" };
	}

	protected String getConfigFile() {
		return new File(testData.getTestRunDataDir(), "pcrglobwbBmiThriftTest/setup_RhineMeuse_30arcmin_3layers_using_input_example.ini").getAbsolutePath();
	}

	protected EBMI createModel() throws Exception {
		File modelWorkDir = new File(testData.getTestRunDataDir(), "pcrglobwbBmiThriftTest");

		// python executable must be on the PATH environment variable.
		String pythonExecutable = "python";
		File opendaPythonPath = new File(testData.getProjectRootDir(), "bin/python");

		// File modelPythonPath = new File(testData.getTestRunDataDir(),
		// "wflow_bin");
		// External location for now
		File modelPythonPath = new File("/home/niels/workspace/PCR-GLOBWB/model");

		String modelPythonModuleName = "bmiPcrglobwb";
		String modelPythonClassName = "BmiPCRGlobWB";

		String host = "127.0.0.1";
		
		return BmiModelFactory.createModelBridge(host, pythonExecutable, opendaPythonPath, modelPythonPath, modelPythonModuleName, modelPythonClassName, modelWorkDir);
	}

	// File modelConfigFile = new
	// File("/home/niels/workspace/eWaterCycle-operational/pcrglobwb_config/setup_30min_niels_laptop.ini");
	// File modelWorkDir = new File("/home/niels/workspace/PCR-GLOBWB");
	//
	// // python executable must be on the PATH environment variable.
	// String pythonExecutable = "python";
	// File opendaPythonPath = new File(testData.getProjectRootDir(),
	// "bin/python");
	//
	// String modelPythonModuleName = "bmiPcrglobwb";
	// String modelPythonClassName = "ScaledBmiPCRGlobWB";
	//
	// BMI model = LocalPythonThriftBMI.createModel(pythonExecutable,
	// opendaPythonPath, modelPythonPath, modelPythonModuleName,
	// modelPythonClassName, modelWorkDir);
	// model.initialize(modelConfigFile.getAbsolutePath());
	//
	// return model;

	// }
}
