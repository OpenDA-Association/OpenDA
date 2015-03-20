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
public class BBStochModelParamDrawerTest extends TestCase {

	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(BBStochModelParamDrawerTest.class, "core");
	}

	public void testStochModelConfig_ParamDrawer_1() {

		StochVector.setSeed(1234);
		
		File testRunDataDir = testData.getTestRunDataDir();
		IStochModelFactory stochModelFactory = new BBStochModelFactory();
		File workingDir = new File(testRunDataDir, "parameterUncertainty");
		stochModelFactory.initialize(workingDir, new String[]{"StochModelConfig_ParamDrawer.xml"});

		IStochModelInstance stochModelInstance =
				stochModelFactory.getInstance(IStochModelFactory.OutputLevel.ModelDefault);

		checkParams(stochModelInstance);
		checkUncertainty(stochModelInstance);
	}

	private void checkParams(IStochModelInstance stochModelInstance) {
		IVector parameters = stochModelInstance.getParameters();
		assertTrue("parameters should be ITreeVector", parameters instanceof ITreeVector);

		ITreeVector paramsAsTreeVector = (ITreeVector) parameters;
		assertEquals("#pars", 6, paramsAsTreeVector.getSize());

		ArrayList<String> subTreeVectorIds = paramsAsTreeVector.getSubTreeVectorIds();

		assertEquals("par id 0", "locA.Par1", subTreeVectorIds.get(0));
		assertEquals("par id 1", "locA.Par2", subTreeVectorIds.get(1));
		assertEquals("par id 2", "p1", subTreeVectorIds.get(2));
		assertEquals("par id 3", "p2", subTreeVectorIds.get(3));
		assertEquals("par id 4", "locB.Par1", subTreeVectorIds.get(4));
		assertEquals("par id 5", "locC.Par_I", subTreeVectorIds.get(5));

		IVector paramClone = parameters.clone();
		paramClone.setConstant(444.0);
		stochModelInstance.setParameters(paramClone);

		IStochVector parameterUncertainty = stochModelInstance.getParameterUncertainty();

		IVector expectations = parameterUncertainty.getExpectations();
		assertEquals("parameterUncertainty size", 6, expectations.getSize());
		for (int i = 0; i < expectations.getSize(); i++) {
			assertEquals("expectation " + String.valueOf(i), 0., expectations.getValue(i));
		}
	}

	private void checkUncertainty(IStochModelInstance stochModelInstance) {

		double[] expectedRealizations = {0.01413002374d, 0.04355282035d, 0d, 0d, -0.08376832667, 0.5020885479};

		double[][] possibleValuesP1P2 = { {11, 21, 31, 41, 51, 61, 71, 81, 91, 101} ,
				                          {12, 22, 32, 42, 52, 62, 72, 82, 92, 102} };

		IStochVector parameterUncertainty = stochModelInstance.getParameterUncertainty();

		IVector realization = parameterUncertainty.createRealization();
		assertEquals("#realizations", 6, realization.getSize());
		for (int i = 0; i < realization.getSize(); i++) {
			if (i==2 || i == 3) {
				boolean valueOk = false;
				double realizationValue = realization.getValue(i);
				for (double possibleValue : possibleValuesP1P2[i-2]) {
					if(Double.compare(possibleValue, realizationValue) == 0) {
						valueOk = true;
					}
				}
				assertTrue("p1/p2 must be one of the 10 possible values", valueOk);
			} else {
				assertEquals("real." + String.valueOf(i + 1), expectedRealizations[i], realization.getValue(i), 1e-10);
			}
		}
	}
}
