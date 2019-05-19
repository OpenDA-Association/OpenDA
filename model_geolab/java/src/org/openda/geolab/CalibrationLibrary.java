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

package org.openda.geolab;

import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochObserver;

import java.io.File;
import java.util.Arrays;

public class CalibrationLibrary implements ICalibrationLibrary {

	private CalLibStochObserver stochObserver = null;
	private CalLibStochModelFactory stochModelFactory = null;
	private File workingDir = null;
	private String exceptionErrmsg = null;
	private StackTraceElement[] exceptionStackTrace = null;
	private CalLibDudAlgorithm algorithm = null;
	private double[] optimalParameterValues = null;

	@SuppressWarnings("WeakerAccess")
	public CalibrationLibrary() {
	}

	public int initialize(File workingDir) {
		this.workingDir = workingDir;
		stochModelFactory = null;
		CalLibStochModelFactory.stochModelInstance = null;
		algorithm = null;
		return 0;
	}

	public int observerSetObsAndStdDevs(double[] observations, double[] standardDeviations) {
		try {
			stochObserver = new CalLibStochObserver(observations, standardDeviations);
			return 0;
		} catch (Exception e) {
			exceptionErrmsg = e.getMessage();
			exceptionStackTrace = e.getStackTrace();
		}
		return 0;
	}

	public int modelSetParameterDefinitions(double[] initialParameterValues, double[] standardDeviations) {
		stochModelFactory = new CalLibStochModelFactory(initialParameterValues, standardDeviations);
		return 0;
	}

	public int modelSetResults(double[] modelResults) {
		CalLibStochModelFactory.setlastModelResults(modelResults);
		return 0;
	}

	public double[] algorithmGetNextParameterValues() {
		if (algorithm == null) {
			algorithm = createAlgorithm(workingDir, stochObserver, stochModelFactory);
			Thread algorithmThread = new Thread(algorithm);
			algorithmThread.start();
		}
		double[] nextParams = CalLibStochModelFactory.waitForNextParams();
		if (nextParams == null) {
			optimalParameterValues = algorithm.getBestEstimate().getParameters().getValues();
		}
		return nextParams;
	}

	public String getErrorMessage() {
		return ((CalLibStochModelInstance)stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress)).
			getErrorString();
	}

	public double[] algorithmGetOptimalParameterValues() {
		return optimalParameterValues;
	}

	public String getExceptionMessage() {
		return exceptionErrmsg;
	}

	public String getExceptionStackTrace() {
		return Arrays.toString(exceptionStackTrace);
	}

	private CalLibDudAlgorithm createAlgorithm(File workingDir, IStochObserver observer, IStochModelFactory modelFactory) {
		CalLibDudAlgorithm algorithm = new CalLibDudAlgorithm();
		algorithm.initialize(workingDir, new String[]{DudXmlConfig});
		algorithm.setStochComponents(observer, modelFactory);
		return algorithm;
	}

	private static final String DudXmlConfig = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
		"<DudConfig xmlns=\"http://www.openda.org\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"http://www.openda.org\n" +
		"\t\t\thttp://schemas.openda.org/algorithm/dudConfig.xsd\">\n" +
		"\t<costFunction weakParameterConstraint=\"false\" class=\"org.openda.algorithms.SimulationKwadraticCostFunction\"/>\n" +
		"\t<outerLoop maxIterations=\"50\" absTolerance=\"0.0001\" relTolerance=\"0.001\" relToleranceLinearCost=\"0.0001\"/>\n" +
		"\t<lineSearch maxIterations=\"50\" maxRelStepSize=\"3\">\n" +
		"\t\t<backTracking shorteningFactor=\"0.5\" startIterationNegativeLook=\"3\"/>\n" +
		"\t</lineSearch>\n" +
		"</DudConfig>\n";
}
