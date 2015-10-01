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

/**
 * Tests for the IncrementModel.
 * @author Niels Drost
 *
 */
public abstract class AbstractIncrementModelBridgeTest extends AbstractModelBridgeTest {

	//configuration for the standard model tests of the increment model
	protected void setUp() throws Exception {
		super.setUp();

		shape = new int[] { 10, 10 };
		gridSpacing = new double[] { 1.0, 1.0 };
		
		gridOrigin =  new double[] { 0.0, 0.0 };
		variableName = "var1";
		variableType = "float64";
		variableUnits = "unit1";
		variableSize = shape[0] * shape[1];
		variableByteSize = variableSize * 8;
		
		attributeNames = new String[] {"author"};
		attributeName = "author";
		attributeValue = "Rolf Hut";
		
		componentName = "Increment Model";
		
		inputVarNames = new String[] {"var1"};
		outputVarNames = new String[] {"var1"};
		
		startTime = 1.0;
		endTime = 20.0;
		timeStep = 1.0;
		timeUnits = "seconds";
	}

	protected String getConfigFile() {
		//no config file for this model
		return "";
	}

	//TODO: test using increment model specific knowledge (Niels Drost)
}
