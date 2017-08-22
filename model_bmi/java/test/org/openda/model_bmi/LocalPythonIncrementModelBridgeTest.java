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
 * Calls an example python bmi model through the thrift bmi bridge
 * 
 * @author Niels Drost
 *
 */
public class LocalPythonIncrementModelBridgeTest extends AbstractIncrementModelBridgeTest {

	protected EBMI createModel() throws Exception {
		File modelConfigFile = new File(testData.getTestRunDataDir(), "bmiThriftIncrementModelTest");
		File modelWorkDir = modelConfigFile.getParentFile();

		// python executable must be on the PATH environment variable.
		String pythonExecutable = "python";
		File opendaPythonPath = new File(testData.getProjectRootDir(), "bin/python");
		File modelPythonPath = new File(testData.getTestRunDataDir(), "bmiThriftIncrementModelTest");
		String modelPythonModuleName = "increment_model";
		String modelPythonClassName = "IncrementModel";

		EBMI model = BmiModelFactory.createModelBridge(null, pythonExecutable, opendaPythonPath, modelPythonPath, modelPythonModuleName, modelPythonClassName, modelWorkDir);
		model.initialize(modelConfigFile.getAbsolutePath());

		return model;
	}
}
