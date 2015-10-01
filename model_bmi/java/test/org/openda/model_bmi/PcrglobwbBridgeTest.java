/*
 * Copyright 2014 Netherlands eScience Center
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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
