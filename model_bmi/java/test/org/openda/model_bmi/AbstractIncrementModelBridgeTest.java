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
