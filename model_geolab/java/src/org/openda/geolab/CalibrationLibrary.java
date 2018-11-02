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

import org.openda.algorithms.Dud;
import org.openda.interfaces.IAlgorithm;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IStochObserver;

import java.io.File;
import java.util.Arrays;

public class CalibrationLibrary implements ICalibrationLibrary {

	private CalibrationLibraryStochObserver stochObserver = null;
	private CalibrationLibraryStochModelFactory stochModelFactory = null;
	private File workingDir = null;
	private String exceptionErrmsg = null;
	private StackTraceElement[] exceptionStackTrace = null;
	private CalibrationLibraryDudAlgorithm algorithm = null;

	public int initialize(File workingDir) {
		this.workingDir = workingDir;
		return 0;
	}

	@Override
	public int observerSetObsAndStdDevs(double[] observations, double[] standardDeviations) {
		try {
			stochObserver = new CalibrationLibraryStochObserver(observations, standardDeviations);
			return 0;
		} catch (Exception e) {
			exceptionErrmsg = e.getMessage();
			exceptionStackTrace = e.getStackTrace();
		}
		return 0;
	}

	public int modelSetParameterDefinitions(double[] initialParameterValues, double[] standardDeviations) {
		stochModelFactory = new CalibrationLibraryStochModelFactory(initialParameterValues, standardDeviations);
		return 0;
	}

	public int modelSetResults(double[] modelResults) {
		CalibrationLibraryStochModelFactory.setlastModelResults(modelResults);
		return 0;
	}

	public double[] algorithmGetNextParameterValues() {
		if (algorithm == null) {
			algorithm = createAlgorithm(workingDir, stochObserver, stochModelFactory);
			algorithm.prepare();
			return algorithm.getBestEstimate().getParameters().getValues();
		}
		if (algorithm.hasNext()) {
			algorithm.next();
			return algorithm.getBestEstimate().getParameters().getValues();
		} else {
			return null;
		}
	}

	public double[] algorithmGetOptimalParameterValues() {
		return null;
	}

	public String getExceptionMessage() {
		return exceptionErrmsg;
	}

	public String getExceptionStackTrace() {
		return Arrays.toString(exceptionStackTrace);
	}

	private CalibrationLibraryDudAlgorithm createAlgorithm(File workingDir, IStochObserver observer, IStochModelFactory modelFactory) {
		CalibrationLibraryDudAlgorithm algorithm = new CalibrationLibraryDudAlgorithm();
		algorithm.initialize(workingDir, new String[]{DudXmlConfig});
		algorithm.setStochComponents(observer, modelFactory);
		return algorithm;
	}

	private static final String DudXmlConfig = "<TODO>";
}
