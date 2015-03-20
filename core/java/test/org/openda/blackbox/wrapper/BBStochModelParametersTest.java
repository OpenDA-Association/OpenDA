/* MOD_V2.0 
* Copyright (c) 2012 OpenDA Association
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
package org.openda.blackbox.wrapper;
import junit.framework.TestCase;
import org.openda.interfaces.*;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.StochVector;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

/**
 * Test for the stochastic configuration of parameters.
 */
public class BBStochModelParametersTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(BBStochModelParametersTest.class, "core");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testStochModelConfig_1() {

		IStochModelFactory stochModelFactory = new BBStochModelFactory();
		File workingDir = new File(testRunDataDir, "parameterUncertainty");
		stochModelFactory.initialize(workingDir, new String[]{"StochModelConfig_1.xml"});

		checkParamsAndUncertainty_1_and_2(stochModelFactory);
	}

	public void testStochModelConfig_2() {

		IStochModelFactory stochModelFactory = new BBStochModelFactory();
		File workingDir = new File(testRunDataDir, "parameterUncertainty");
		stochModelFactory.initialize(workingDir, new String[]{"StochModelConfig_2.xml"});

		checkParamsAndUncertainty_1_and_2(stochModelFactory);
	}

	private void checkParamsAndUncertainty_1_and_2(IStochModelFactory stochModelFactory) {
		IStochModelInstance stochModelInstance =
				stochModelFactory.getInstance(IStochModelFactory.OutputLevel.ModelDefault);

		IVector parameters = stochModelInstance.getParameters();
		assertTrue("parameters should be ITreeVector", parameters instanceof ITreeVector);

		ITreeVector paramsAsTreeVector = (ITreeVector) parameters;
		assertEquals("#pars", 4, paramsAsTreeVector.getSize());

		ArrayList<String> subTreeVectorIds = paramsAsTreeVector.getSubTreeVectorIds();

		assertEquals("par id 0", "locA.Par1", subTreeVectorIds.get(0));
		assertEquals("par id 1", "locA.Par2", subTreeVectorIds.get(1));
		assertEquals("par id 2", "locB.Par1", subTreeVectorIds.get(2));
		assertEquals("par id 3", "locC.Par_I", subTreeVectorIds.get(3));

		IVector paramClone = parameters.clone();
		paramClone.setConstant(444.0);
		stochModelInstance.setParameters(paramClone);

		IStochVector parameterUncertainty = stochModelInstance.getParameterUncertainty();

		IVector expectations = parameterUncertainty.getExpectations();
		assertEquals("parameterUncertainty size", 4, expectations.getSize());
		assertEquals("expectation 0", 0., expectations.getValue(0));
		assertEquals("expectation 1", 0., expectations.getValue(1));
		assertEquals("expectation 2", 0., expectations.getValue(2));
		assertEquals("expectation 3", 0., expectations.getValue(3));

		IVector stdDevs = parameterUncertainty.getStandardDeviations();
		assertEquals("stdDevs size", 4, expectations.getSize());
		assertEquals("stdDev 0, locA.Par1" , 0.1001d, stdDevs.getValue(0));
		assertEquals("stdDev 1, locA.Par2" , 0.1002d, stdDevs.getValue(1));
		assertEquals("stdDev 2, locB.Par1" , 0.201d, stdDevs.getValue(2), 1.e-8);
		assertEquals("stdDev 3, locC.Par_I", 0.301d, stdDevs.getValue(3), 1.e-8);
	}

	public void testStochModelConfig_2_realizations() {

		StochVector.setSeed(1234); //fix seed to get same results every time
		
		IStochModelFactory stochModelFactory = new BBStochModelFactory();
		File workingDir = new File(testRunDataDir, "parameterUncertainty");
		stochModelFactory.initialize(workingDir, new String[]{"StochModelConfig_2.xml"});

		checkRealizations_2_and_3(stochModelFactory);
	}

	public void testStochModelConfig_3_realizations() {

		StochVector.setSeed(1234); //fix seed to get same results every time
		
		IStochModelFactory stochModelFactory = new BBStochModelFactory();
		File workingDir = new File(testRunDataDir, "parameterUncertainty");
		stochModelFactory.initialize(workingDir, new String[]{"StochModelConfig_3.xml"});

		checkRealizations_2_and_3(stochModelFactory);
	}

	private void checkRealizations_2_and_3(IStochModelFactory stochModelFactory) {

		double[] expectedRealizations = {0.014130023740d, 0.043552820357d, -0.08376832667, 0.5020885479};

		IStochModelInstance stochModelInstance =
				stochModelFactory.getInstance(IStochModelFactory.OutputLevel.ModelDefault);

		IStochVector parameterUncertainty = stochModelInstance.getParameterUncertainty();

		IVector realization = parameterUncertainty.createRealization();
		assertEquals("#realizations", 4, realization.getSize());
		for (int i = 0; i < realization.getSize(); i++) {
			// Due to static var's in the random generator the test results of test 4 depend on whether
			// it is running individually or after another test.
			assertEquals("real." + String.valueOf(i + 1), expectedRealizations[i], realization.getValue(i), 1e-10);
		}
	}
}
