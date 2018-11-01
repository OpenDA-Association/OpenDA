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

import java.io.File;

public class DummyCalibrationLibrary implements ICalibrationLibrary {

	private File odaFilePath;
	private double[] initialParameterValues;
	private double[] standardDeviations;
	private int iterationCounter = 0;
	private double[] lastModelResults;
	private double[] optimalParameterValues;

	public int initialize(File workingDir) {
		this.odaFilePath = workingDir;
		return 0;
	}

	@Override
	public int observerSetObsAndStdDevs(double[] observations, double[] standardDeviations) {
		throw new RuntimeException("org.openda.geolab.DummyCalibrationLibrary.observerSetObsAndStdDevs() not implemented yet");
	}

	public int modelSetParameterDefinitions(double[] initialParameterValues, double[] standardDeviations) {

		this.initialParameterValues = initialParameterValues;
		this.standardDeviations = standardDeviations;
		return 0;
	}

	public int modelSetResults(double[] modelResults) {
		this.lastModelResults = modelResults;
		return 0;
	}

	public double[] algorithmGetNextParameterValues() {
		iterationCounter++;
		int paramCount = initialParameterValues.length;
		double[] nextParameterValues = new double[paramCount];
		for (int i = 0; i < paramCount; i++) {
			nextParameterValues[i] = initialParameterValues[i] + iterationCounter*standardDeviations[i];
		}
		optimalParameterValues = nextParameterValues;
		return nextParameterValues;
	}

	public double[] algorithmGetOptimalParameterValues() {
		return optimalParameterValues;
	}

	public String getExceptionMessage() {
		return "getExceptionMessage not implemented yet";
	}
	public String getExceptionStackTrace() {
		return "getExceptionStackTrace not implemented yet";
	}
}
