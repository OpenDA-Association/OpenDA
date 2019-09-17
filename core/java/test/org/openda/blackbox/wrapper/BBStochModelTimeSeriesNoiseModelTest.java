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
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.ITime;
import org.openda.interfaces.IVector;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.StochVector;
import org.openda.utils.Time;

import java.io.File;
import java.io.IOException;

/**
 * Testing for the stoch model configuration of coloured noise on 2d maps
 */
public class BBStochModelTimeSeriesNoiseModelTest extends TestCase
{

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(BBStochModelTimeSeriesNoiseModelTest.class, "core");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testNoiseModel() {

		StochVector.setSeed(1234);
		BBStochModelFactory bbStochModelFactory = new BBStochModelFactory();
		File noiseModelDir = new File(testRunDataDir, "noiseModelTimeSeries");
		bbStochModelFactory.initialize(noiseModelDir, new String[]{"bbStochModelConfig_1.xml"});

		int instanceCount = 4;
		IStochModelInstance[] stochModelInstances = new IStochModelInstance[instanceCount];
		for (int i = 0; i < instanceCount; i++) {
			stochModelInstances[i] = bbStochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
			stochModelInstances[i].setAutomaticNoiseGeneration(true);
		}

		int timeStepCount = 5;
		ITime timeHorizon = stochModelInstances[0].getTimeHorizon();
		ITime startTime = timeHorizon.getBeginTime();
		double deltaTasMJD = 1d / 24d / 60d * 30d; // 30 minutes (3 bc.-timeseries steps)
		for (int t = 1; t <= timeStepCount; t++) {
			ITime targetTime = new Time(startTime.getMJD() + t * deltaTasMJD);
			for (int i = 0; i < instanceCount; i++) {
				stochModelInstances[i].compute(targetTime);
			}
			if (t == 3) {
				double[] values = stochModelInstances[2].getExchangeItem("model-item-A-1-b").getValuesAsDoubles(); 
				assertEquals("instance 2, model-item-A-1-b", -6.849643d, //-2.64992454d
						values[0], 1e-6d);
			}
			if (t == 4) {
				double[] values = stochModelInstances[0].getExchangeItem("model-item-A-2").getValuesAsDoubles();
				assertEquals("instance 0 model-item-A-2", -2.261432d, //-1.88463215d
						values[0], 1e-6d);
			}
		}
	}
	
	public void testNoiseModel2() {

		StochVector.setSeed(1234);
		BBStochModelFactory bbStochModelFactory = new BBStochModelFactory();
		File noiseModelDir = new File(testRunDataDir, "noiseModelTimeSeries");
		bbStochModelFactory.initialize(noiseModelDir, new String[]{"bbStochModelConfig_1.xml"});

		IStochModelInstance stochModelInstance = bbStochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
		stochModelInstance.setAutomaticNoiseGeneration(true);

		int timeStepCount = 5;
		double[] trueValues={0.0, 0.0, 1.477467, 1.47-0.35, -1.85, -3.32}; //TODO the values get summed
		ITime timeHorizon = stochModelInstance.getTimeHorizon();
		ITime startTime = timeHorizon.getBeginTime();
		double deltaTasMJD = 1d / 24d / 60d * 30d; // 30 minutes (3 bc.-timeseries steps)
		for (int t = 1; t <= timeStepCount; t++) {
			ITime targetTime = new Time(startTime.getMJD() + t * deltaTasMJD);
			stochModelInstance.compute(targetTime);
			double[] values = stochModelInstance.getExchangeItem("model-item-A-1-a").getValuesAsDoubles(); 
			assertEquals("instance 2, model-item-A-1-a", trueValues[t], values[0], 1e-2d);
		}
		
		IVector state=stochModelInstance.getState();
		stochModelInstance.axpyOnState(0.5, state); //add 50%
	}

	public void testNoiseModelStateSplit() {

		StochVector.setSeed(1234);
		BBStochModelFactory bbStochModelFactory = new BBStochModelFactory();
		File noiseModelDir = new File(testRunDataDir, "noiseModelTimeSeriesStateSplit");
		System.out.println("noiseModelDir " + noiseModelDir + " exists" + noiseModelDir.exists());

		bbStochModelFactory.initialize(noiseModelDir, new String[]{"bbStochModelConfigStateSplit.xml"});

		IStochModelInstance stochModelInstance = bbStochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
		stochModelInstance.setAutomaticNoiseGeneration(true);

		int timeStepCount = 5;
		double[] trueValues = {0.0, 0.0, 1.477467, 1.47 - 0.35, -1.85, -3.32}; //TODO the values get summed
		ITime timeHorizon = stochModelInstance.getTimeHorizon();
		ITime startTime = timeHorizon.getBeginTime();
		double deltaTasMJD = 1d / 24d / 60d * 30d; // 30 minutes (3 bc.-timeseries steps)
		for (int t = 1; t <= timeStepCount; t++) {
			ITime targetTime = new Time(startTime.getMJD() + t * deltaTasMJD);
			stochModelInstance.compute(targetTime);
			double[] values = stochModelInstance.getExchangeItem("model-item-A-1-a").getValuesAsDoubles();
			assertEquals("instance 2, model-item-A-1-a", trueValues[t], values[0], 1e-2d);
			System.out.println("assertEquals trueValues");
		}
		// How is this state vector setup?
		IVector state = stochModelInstance.getState();
		stochModelInstance.axpyOnState(0.5, state);
		double[] values = state.getValues();
		assertEquals(8, values.length);
		System.out.println("assertEquals 8 state");
		IVector state0 = stochModelInstance.getState(0);
		stochModelInstance.axpyOnState(0.5, state0, 0);
		double[] values0 = state0.getValues();
		assertEquals(5, values0.length);
		System.out.println("assertEquals 5 state0");
		IVector state1 = stochModelInstance.getState(1);
		stochModelInstance.axpyOnState(0.5, state1, 1);
		double[] values1 = state1.getValues();
		// TODO EP: check if values are correct
		assertEquals(3, values1.length);
		System.out.println("assertEquals 3 state1");
	}

	public void testBoundaryNoiseModelLN() {

		StochVector.setSeed(1234);
		BBStochModelFactory bbStochModelFactory = new BBStochModelFactory();
		File noiseModelDir = new File(testRunDataDir, "noiseModelBoundaryLN");
		bbStochModelFactory.initialize(noiseModelDir, new String[]{"bbStochModelConfigBoundaryLN.xml"});

		int instanceCount = 4;
		IStochModelInstance[] stochModelInstances = new IStochModelInstance[instanceCount];
		for (int i = 0; i < instanceCount; i++) {
			stochModelInstances[i] = bbStochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
			stochModelInstances[i].setAutomaticNoiseGeneration(true);
		}

		int timeStepCount = 5;
		ITime timeHorizon = stochModelInstances[0].getTimeHorizon();
		ITime startTime = timeHorizon.getBeginTime();
		double deltaTasMJD = 1d / 24d / 60d * 30d; // 30 minutes (3 bc.-timeseries steps)
		for (int t = 1; t <= timeStepCount; t++) {
			ITime targetTime = new Time(startTime.getMJD() + t * deltaTasMJD);
			for (int i = 0; i < instanceCount; i++) {
				stochModelInstances[i].compute(targetTime);
			}
			if (t == 3) {
				double[] values = stochModelInstances[2].getExchangeItem("model-item-A-1-b").getValuesAsDoubles();
				assertEquals("instance 2, model-item-A-1-b", Math.exp(-6.849643d), values[0], 1e-6d);
			}
			if (t == 4) {
				double[] values = stochModelInstances[0].getExchangeItem("model-item-A-2").getValuesAsDoubles();
				assertEquals("instance 0 model-item-A-2", Math.exp(-2.261432d), values[0], 1e-6d);
			}
		}
	}
}
