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

package org.openda.geolab.application;

import java.io.File;

public class OpenDaCalibrationLibrary implements IOpenDaCalibrationLibrary {

	public int initialize(File odaFilePath) {
		return -1;
	}

	@Override
	public int observerSetObsAndStdDevs(double[] observations, double[] standardDeviations) {
		throw new RuntimeException("org.openda.geolab.application.OpenDaCalibrationLibrary.observerSetObsAndStdDevs() not implemented yet");
	}

	public int modelSetParameterDefinitions(double[] initialParameterValues, double[] standardDeviations) {
		return -1;
	}

	public int modelSetResults(double[] modelResults) {
		return -1;
	}

	public double[] algorithmGetNextParameterValues() {
		return new double[0];
	}

	public double[] algorithmGetOptimalParameterValues() {
		return null;
	}

	public String getExceptionMessage() {
		return "getExceptionMessage not implemented yet";
	}
	public String getExceptionStackTrace() {
		return "getExceptionStackTrace not implemented yet";
	}
}
