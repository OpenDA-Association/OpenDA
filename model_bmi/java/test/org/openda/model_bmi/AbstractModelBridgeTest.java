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
package org.openda.model_bmi;

import bmi.BMIGridType;
import bmi.EBMI;
import junit.framework.TestCase;

import org.openda.utils.OpenDaTestSupport;

import java.util.Arrays;

/**
 * Generic BMI model test. If configured correctly with the static variables
 * declared at the top of this class, it will test basic usage of most (E)BMI
 * functions.
 * 
 * @author Niels Drost
 *
 */
public abstract class AbstractModelBridgeTest extends TestCase {

	protected OpenDaTestSupport testData;

	protected int[] shape;
	protected double[] gridSpacing;
	protected double[] gridOrigin;

	// variable we use for testing
	protected String variableName;
	protected String variableType;
	protected String variableUnits;
	protected int variableSize;
	protected int variableByteSize;

	
	protected String componentName;
	protected String[] inputVarNames;
	protected String[] outputVarNames;

	protected double startTime;
	protected double endTime;
	protected double timeStep;
	protected String timeUnits;

	//list of all attribute names
	protected String[] attributeNames;
	
	//which attribute to test getting the value of
	protected String attributeName;
	protected String attributeValue;
	
	protected EBMI model;

	protected void setUp() throws Exception {
		testData = new OpenDaTestSupport(getClass(), "model_bmi");

		model = createModel();
	}

	protected void tearDown() throws Exception {
		stopModel();
	}

	protected abstract String getConfigFile() throws Exception;

	protected abstract EBMI createModel() throws Exception;

	protected void stopModel() {
		if (model == null) {
			return;
		}

		try {
			model.finalizeModel();
		} catch (Exception e) {
			e.printStackTrace(System.err);
		}
	}

	protected void initializeModel() throws Exception {
		model.initialize(getConfigFile());
	}

	public void testCreateModel() throws Exception {
		// success
	}

	public void testInitialize() throws Exception {
		model.initialize(getConfigFile());
	}

	public void testInitializeConfig() throws Exception {
		model.initializeConfig(getConfigFile());
	}

	public void testInitializeModel() throws Exception {
		model.initializeConfig(getConfigFile());
		model.initializeModel();
	}

	public void testUpdate() throws Exception {
		initializeModel();
		model.update();
	}

	public void testUpdateUntil() throws Exception {
		initializeModel();
		model.updateUntil(model.getEndTime());
	}

	public void testGetComponentName() throws Exception {
		initializeModel();
		assertEquals("incorrect model/component name", componentName, model.getComponentName());
	}

	public void testGetInputVarNames() throws Exception {
		initializeModel();

		assertTrue("incorrect input variables: Got " + Arrays.toString(model.getInputVarNames()) + " but expected " + Arrays.toString(inputVarNames),
				Arrays.equals(inputVarNames, model.getInputVarNames()));
	}

	public void testGetOutputVarNames() throws Exception {
		initializeModel();
		model.getOutputVarNames();

		assertTrue("incorrect output variables: Got " + Arrays.toString(model.getOutputVarNames()) + " but expected " + Arrays.toString(outputVarNames),
				Arrays.equals(outputVarNames, model.getOutputVarNames()));
	}

	public void testGetVarType() throws Exception {
		initializeModel();

		assertEquals("incorrect variable type", variableType, model.getVarType(variableName));
	}

	public void testGetVarUnits() throws Exception {
		initializeModel();

		assertEquals("incorrect variable units", variableUnits, model.getVarUnits(variableName));
	}

	public void testGetVarRank() throws Exception {
		initializeModel();

		assertEquals("incorrect variable rank", 2, model.getVarRank(variableName));
	}

	public void testGetVarSize() throws Exception {
		initializeModel();

		assertEquals("incorrect variable size", variableSize, model.getVarSize(variableName));
	}

	public void testGetVarNbytes() throws Exception {
		initializeModel();

		assertEquals("incorrect variable size (in bytes)", variableByteSize, model.getVarNbytes(variableName));
	}
	

	public void testGetStartTime() throws Exception {
		initializeModel();

		assertEquals("incorrect start time", startTime, model.getStartTime());
	}

	public void testGetEndTime() throws Exception {
		initializeModel();

		assertEquals("incorrect end time", endTime, model.getEndTime());
	}

	public void testGetTimeStep() throws Exception {
		initializeModel();

		assertEquals("incorrect timestep", timeStep, model.getTimeStep());
	}

	public void testGetTimeUnits() throws Exception {
		initializeModel();

		assertEquals("incorrect time units", timeUnits, model.getTimeUnits());
	}

	public void testGetCurrentTime() throws Exception {
		initializeModel();

		assertEquals("current time expected to be equal to start time initially", model.getStartTime(), model.getCurrentTime());

		model.update();

		assertEquals("current time expected to be equal to start time + timestep after a single update", model.getStartTime() + model.getTimeStep(), model.getCurrentTime());
	}

	// public void test() throws Exception {
	// initializeModel();
	// model.
	// }

	public void testGetVariable() throws Exception {
		initializeModel();

		String varType = model.getVarType(variableName);

		if (varType.equals("float64")) {
			double[] doubles = model.getDouble(variableName);

			assertEquals("incorrect number of values in variable", variableSize, doubles.length);
		} else if (varType.equals("float32")) {
			float[] floats = model.getFloat(variableName);

			assertEquals("incorrect number of values in variable", variableSize, floats.length);
		} else {
			fail("Unknown variable type: " + varType);
		}
	}

	public void testGetGridType() throws Exception {
		initializeModel();

		BMIGridType actualGridShape = model.getGridType(variableName);

		assertEquals("grid type incorrect", BMIGridType.UNIFORM, actualGridShape);
	}

	public void testGetGridShape() throws Exception {
		initializeModel();

		int[] actualGridShape = model.getGridShape(variableName);

		assertTrue("incorrect shape: " + Arrays.toString(actualGridShape), Arrays.equals(actualGridShape, shape));
	}

	public void testGetGridSpacing() throws Exception {
		initializeModel();

		double[] actualGridSpacing = model.getGridSpacing(variableName);

		assertTrue("incorrect spacing: " + Arrays.toString(actualGridSpacing), Arrays.equals(gridSpacing, actualGridSpacing));
	}

	public void testGetGridOrigin() throws Exception {
		initializeModel();

		double[] actualGridOrigin = model.getGridOrigin(variableName);

		assertTrue("incorrect origin: " + Arrays.toString(actualGridOrigin), Arrays.equals(gridOrigin, actualGridOrigin));
	}

	public void testSetVariable() throws Exception {
		initializeModel();

		String varType = model.getVarType(variableName);
		int[] varShape = model.getGridShape(variableName);

		if (varType.equals("float64")) {
			double[] doubles = new double[varShape[0] * varShape[1]];

			model.setDouble(variableName, doubles);
		} else if (varType.equals("float32")) {
			float[] floats = new float[varShape[0] * varShape[1]];

			model.setFloat(variableName, floats);
		} else {
			fail("Unknown variable type: " + varType);
		}
	}

	public void testRunModel() throws Exception {
		initializeModel();

		while (model.getCurrentTime() < model.getEndTime()) {
			model.update();
		}
	}
	
	public void testSetStartTime() throws Exception {
		model.initializeConfig(getConfigFile());

		model.setStartTime(startTime + 1.0);

		assertEquals("Failed to set start time", startTime + 1.0, model.getStartTime(), 0.0);

		model.initializeModel();

		assertEquals("Failed to set start time", startTime + 1.0, model.getStartTime(), 0.0);
	}

	public void testSetEndTime() throws Exception {
		model.initializeConfig(getConfigFile());

		model.setEndTime(endTime + 1.0);

		assertEquals("Failed to set end time", endTime + 1.0, model.getEndTime(), 0.0);

		model.initializeModel();

		assertEquals("Failed to set end time", endTime + 1.0, model.getEndTime(), 0.0);
	}


	public void testGetAttributeNames() throws Exception {
		initializeModel();

		assertTrue("incorrect output variables: Got " + Arrays.toString(model.getAttributeNames()) + " but expected " + Arrays.toString(attributeNames),
				Arrays.equals(attributeNames, model.getAttributeNames()));
	}

	// test of a extended BMI function
	public void testGetAttributeValue() throws Exception {
		initializeModel();

		assertEquals("wrong attribute value", attributeValue, model.getAttributeValue(attributeName));
	}
	
	// test of a extended BMI function
	public void testSetAttributeValue() throws Exception {
		initializeModel();

		model.setAttributeValue(attributeName, "test123");
		
		assertEquals("wrong attribute value", "test123", model.getAttributeValue(attributeName));
	}

	// setStartTime
	// setEndTime
}
