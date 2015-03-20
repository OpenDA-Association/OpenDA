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
import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.*;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;
import org.openda.utils.Vector;
import org.openda.utils.io.FileBasedModelState;

import java.io.File;
import java.io.IOException;
import java.util.GregorianCalendar;

/**
 * Tests for restarting a black box stoch model ensemble
 * Note: this test does not compare output yet, but inspection of the result files
 * show that the restarted noise is added correcty.
 */
public class BBStochmodelEnsembleRestartTest extends TestCase {

	private File testRunDataDir;
	private final double epsilon = 1e-8;

	protected void setUp() throws IOException {
		OpenDaTestSupport testData = new OpenDaTestSupport(BBStochmodelEnsembleRestartTest.class, "core");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testRestart() {

		// determine start and end time, and the analysis time steps
		double startTime = Time.milliesToMjd(new GregorianCalendar(2010, 7, 4).getTimeInMillis());
		startTime = startTime + 2d / 24d;
		double endTime = startTime + 1.0d;
		int timeStepCount = 24 * 6;
		int analysisStepCount = timeStepCount / 3;
		double deltaT = (endTime - startTime) / timeStepCount;
		double analysisDeltaT = (endTime - startTime) / analysisStepCount;

		double restartTime = startTime + 8d / 24d; // 08:00
		double[] analysisTimes = new double[analysisStepCount];
		for (int i = 0; i < analysisTimes.length; i++) {
			analysisTimes[i] = startTime + (i + 1) * analysisDeltaT;
		}

		final int ensembleSize = 5;

		// create model instances
		// perform the full computation

		File restartFullDir = new File(testRunDataDir, "restartFull");
		IStochModelInstance[] stochModels = createStochModelEnsemble(ensembleSize, restartFullDir);

		performComputation(stochModels, startTime, deltaT, endTime, analysisTimes);

		storeEnsembleResults(ensembleSize, restartFullDir);

		for (int i = 0; i < ensembleSize; i++) {
			stochModels[i].finish();
		}

		// reset instance counter to reproduce the same noise
		ArmaNoiseModel.resetInstanceCounter();

		// create model instances
		// perform the first part of the restarted computation
		// save the states

		File restartFirstPartDir = new File(testRunDataDir, "restartFirstPart");
		stochModels = createStochModelEnsemble(ensembleSize, restartFirstPartDir);

		performComputation(stochModels, startTime, deltaT, restartTime, analysisTimes);

		FileBasedModelState savedState = new FileBasedModelState();
		savedState.setDirContainingModelstateFiles(restartFirstPartDir);
		for (int i = 0; i < ensembleSize; i++) {
			File stochModelStateFile = new File(restartFirstPartDir, "stoch_model_state_" + String.valueOf(i) + ".zip");
			IModelState savedStochModelState = stochModels[i].saveInternalState();
			savedStochModelState.savePersistentState(stochModelStateFile);
		}

		storeEnsembleResults(ensembleSize, restartFirstPartDir);

		for (int i = 0; i < ensembleSize; i++) {
			stochModels[i].finish();
		}

		// reset instance counter to reproduce the same noise
		ArmaNoiseModel.resetInstanceCounter();

		// create model instances
		// load the states
		// perform the second part of the restarted computation

		File restartSecondPartDir = new File(testRunDataDir, "restartSecondPart");
		stochModels = createStochModelEnsemble(ensembleSize, restartSecondPartDir);

		for (int i = 0; i < ensembleSize; i++) {
			File stochModelStateFile = new File(restartFirstPartDir, "stoch_model_state_" + String.valueOf(i) + ".zip");
			IModelState savedStochModelState = stochModels[i].loadPersistentState(stochModelStateFile);
			stochModels[i].restoreInternalState(savedStochModelState);
		}

		performComputation(stochModels, restartTime, deltaT, endTime, analysisTimes);

		storeEnsembleResults(ensembleSize, restartSecondPartDir);

		for (int i = 0; i < ensembleSize; i++) {
			stochModels[i].finish();
		}
	}

	private IStochModelInstance[] createStochModelEnsemble(int ensembleSize, File ensembleRunDir) {
		IStochModelInstance[] stochModels = new IStochModelInstance[ensembleSize];
		BBStochModelFactory bbStochModelFactory;
		String[] noiseModelTestStochModelArguments;
		bbStochModelFactory = new BBStochModelFactory();
		noiseModelTestStochModelArguments = new String[]{"bbStochModelConfig.xml"};
		bbStochModelFactory.initialize(ensembleRunDir, noiseModelTestStochModelArguments);
		for (int i = 0; i < ensembleSize; i++) {
			stochModels[i] = bbStochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
			stochModels[i].setAutomaticNoiseGeneration(true);
		}
		return stochModels;
	}

	private void performComputation(IStochModelInstance[] stochModels,
									double startTime, double deltaT, double endTime,
									double[] analysisTimes) {
		double currentTime = startTime;
		while (currentTime < endTime - epsilon) {
			currentTime += deltaT;
			ITime timeStamp = new Time(currentTime);
			for (int i = 0; i < stochModels.length; i++) {
				IStochModelInstance stochModel = stochModels[i];
				stochModel.compute(timeStamp);
				if (timeStampIsAnalysisTime(currentTime, analysisTimes)) {
					IVector state = stochModel.getState();
					assertEquals("state size", 8, state.getSize());
					IVector constantVector = new Vector(state.getSize());
					constantVector.setConstant(0.01 * (i + 1));
					stochModel.axpyOnState(1.0, constantVector);
				}
			}
		}
	}

	private void storeEnsembleResults(int ensembleSize, File ensembeRunDir) {

		File storeResultsDir = new File(ensembeRunDir, "results");

		if (storeResultsDir.exists()) {
			BBUtils.deleteDirectory(storeResultsDir);
		}
		if (!storeResultsDir.mkdir()) {
			throw new RuntimeException("Could not create directory for storing ensemble results: " +
					storeResultsDir.getAbsolutePath());
		}
		for (int i = 0; i < ensembleSize; i++) {
			String instanceDirName = "work" + i;
			File instanceDir = new File(ensembeRunDir, instanceDirName);
			BBUtils.makeDirectoryClone(instanceDir, new File(storeResultsDir, instanceDirName));
		}
	}

	private boolean timeStampIsAnalysisTime(double currentTime, double[] analysisTimes) {
		for (double analysisTime : analysisTimes) {
			if (analysisTime + epsilon > currentTime && analysisTime - epsilon < currentTime) {
				return true;
			}
		}
		return false;
	}
}
