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
package org.openda.exchange.dataobjects;

import org.openda.blackbox.config.BBUtils;
import ucar.ma2.Array;
import ucar.ma2.ArrayDouble;
import ucar.ma2.InvalidRangeException;
import ucar.nc2.Dimension;
import ucar.nc2.NetcdfFile;
import ucar.nc2.NetcdfFileWriter;
import ucar.nc2.Variable;
import ucar.nc2.units.DateUnit;

import java.io.File;
import java.io.IOException;
import java.util.*;

public class NetcdfFileConcatenater {
	public static void main(String[] arguments) {
		if (arguments.length != 2) {
			throw new IllegalArgumentException("NetcdfFileConcatenater expects two arguments:\n" +
					"<targetNetcdfFile> and <netcdfFileToBeAdded>");
		}
		// file must be there. If it is indeed, the path has been made absolute by BBaction
		File netcdfFileToBeAdded = new File(arguments[1]);

		// file may be there. If so, the path has been made absolute by BBaction
		File targetNetcdfFile = new File(arguments[0]);
		if (!targetNetcdfFile.isAbsolute()) {
			// target file is not there yet; make path absolute
			targetNetcdfFile = new File(netcdfFileToBeAdded.getParentFile(), arguments[0]);
		}
		try {
			if (!targetNetcdfFile.exists()) {
				BBUtils.copyFile(netcdfFileToBeAdded, targetNetcdfFile);
			} else {
				NetcdfFile soureNetCdfFile = NetcdfFile.open(netcdfFileToBeAdded.getAbsolutePath() );
				List<Variable> variablesToBeAdded = soureNetCdfFile.getVariables();
				NetcdfFileWriter netcdfFileWriter = NetcdfFileWriter.openExisting(targetNetcdfFile.getAbsolutePath() );

				Map<Variable, Array> variableArraysMap = new HashMap<>();
				Map<Variable, Array> timeVariableArraysMap = new HashMap<>();

				for (Variable variable : variablesToBeAdded) {
					if (NetcdfUtils.isTimeVariable(variable)) continue;
					Variable timeVariableToBeAdded = NetcdfUtils.findTimeVariableForVariable(variable, soureNetCdfFile);
					if (timeVariableToBeAdded == null) continue;
					Variable targetVariable = netcdfFileWriter.findVariable(variable.getFullNameEscaped());
					if (targetVariable == null) continue;
					Variable timeVariableTarget = NetcdfUtils.findTimeVariableForVariable(targetVariable, netcdfFileWriter.getNetcdfFile());
					if (timeVariableTarget == null) continue;
					boolean concatenateTimeVariable = !timeVariableArraysMap.containsKey(timeVariableTarget);

					List<Dimension> targetDimensions = targetVariable.getDimensions();
					if (targetDimensions.size() != 2) throw new RuntimeException("Only variables supported with a time and location dimension");
					List<Dimension> addedDimensions = variable.getDimensions();
					if (addedDimensions.size() != 2) throw new RuntimeException("Only variables supported with a time and location dimension");
					Dimension targetLocationDimension = targetDimensions.get(1);
					Dimension addedLocationDimension = addedDimensions.get(1);
					int targetLocationDimensionLength = targetLocationDimension.getLength();
					if (addedLocationDimension.getLength() != targetLocationDimensionLength) throw new RuntimeException("Variables from source and target must have same location dimension size");
					double[][] targetValues = (double[][]) targetVariable.read().copyToNDJavaArray();
					double[][] addedValues = (double[][]) variable.read().copyToNDJavaArray();

					Array read = timeVariableTarget.read();
					double[] timesTarget = (double[]) read.copyTo1DJavaArray();
					String timeVariableTargetUnitsString = timeVariableTarget.getUnitsString();
					double[] timesToBeAdded = (double[]) timeVariableToBeAdded.read().copyTo1DJavaArray();
					String timeVariableToBeAddedUnitsString = timeVariableToBeAdded.getUnitsString();
					DateUnit targetDateUnit;
					DateUnit toBeAddedDateUnit;
					try {
						targetDateUnit = new DateUnit(timeVariableTargetUnitsString);
						toBeAddedDateUnit = new DateUnit(timeVariableToBeAddedUnitsString);
					} catch (Exception e) {
						throw new RuntimeException("Illegal time unit", e);
					}
					Dimension timeVariableTargetDimension = timeVariableTarget.getDimension(0);
					if (!timeVariableTargetDimension.isUnlimited()) throw new RuntimeException("Netcdf target file should have unlimited time dimension");
					Date targetDateOrigin = targetDateUnit.getDateOrigin();
					Date addedDateOrigin = toBeAddedDateUnit.getDateOrigin();
					long timeDif = addedDateOrigin.getTime() - targetDateOrigin.getTime();
					double timeDiffInUnit = timeDif / (1000 * targetDateUnit.getTimeUnit().getValueInSeconds());

					double[] convertedTimesToBeAdded = new double[timesToBeAdded.length];
					for (int i = 0; i < timesToBeAdded.length; i++) {
						convertedTimesToBeAdded[i] = timesToBeAdded[i] + timeDiffInUnit;
					}
					if (convertedTimesToBeAdded[0] < timesTarget[timesTarget.length - 1]) throw new RuntimeException("File to be added has first time before last time of target file");
					int totalTimesCombined = timeVariableTargetDimension.getLength() + timesToBeAdded.length;
					if (convertedTimesToBeAdded[0] == timesTarget[timesTarget.length - 1]) {
						addConcatenatedValueArraysToMaps(variableArraysMap, timeVariableArraysMap, targetVariable, timeVariableTarget, concatenateTimeVariable, targetLocationDimensionLength, targetValues, addedValues, timesTarget, convertedTimesToBeAdded, totalTimesCombined, true);
					} else {
						addConcatenatedValueArraysToMaps(variableArraysMap, timeVariableArraysMap, targetVariable, timeVariableTarget, concatenateTimeVariable, targetLocationDimensionLength, targetValues, addedValues, timesTarget, convertedTimesToBeAdded, totalTimesCombined, false);
					}
				}
				try {
					for (Map.Entry<Variable, Array> entry : timeVariableArraysMap.entrySet()) {
						netcdfFileWriter.write(entry.getKey(), entry.getValue());
					}
					for (Map.Entry<Variable, Array> entry : variableArraysMap.entrySet()) {
						netcdfFileWriter.write(entry.getKey(), entry.getValue());
					}
				} catch (InvalidRangeException e) {
					throw new RuntimeException("Error rewriting variables", e);
				}
				netcdfFileWriter.flush();
				netcdfFileWriter.close();
			}
		} catch (IOException e) {
			throw new RuntimeException("Error", e);
		}
	}

	private static void addConcatenatedValueArraysToMaps(Map<Variable, Array> variableArraysMap, Map<Variable, Array> timeVariableArraysMap, Variable targetVariable, Variable timeVariableTarget, boolean concatenateTimeVariable, int targetLocationDimensionLength, double[][] targetValues, double[][] addedValues, double[] timesTarget, double[] convertedTimesToBeAdded, int totalTimesCombined, boolean firstAddedTimeOverlapping) {
		if (firstAddedTimeOverlapping) totalTimesCombined--;
		ArrayDouble.D1 timeArrayDouble = new ArrayDouble.D1(totalTimesCombined);
		ArrayDouble.D2 valueArrayDouble = new ArrayDouble.D2(totalTimesCombined, targetLocationDimensionLength);

		int targetTimes = firstAddedTimeOverlapping ? timesTarget.length - 1 : timesTarget.length;
		for (int i = 0; i < targetTimes; i++) {
			if (concatenateTimeVariable) timeArrayDouble.set(i, timesTarget[i]);
			for (int j = 0; j < targetLocationDimensionLength; j++) {
				valueArrayDouble.set(i, j, targetValues[i][j]);
			}
		}
		for (int i = 0; i < convertedTimesToBeAdded.length; i++) {
			if (concatenateTimeVariable) timeArrayDouble.set(i + targetTimes, convertedTimesToBeAdded[i]);
			for (int j = 0; j < targetLocationDimensionLength; j++) {
				valueArrayDouble.set(i + targetTimes, j, addedValues[i][j]);
			}
		}
		if (concatenateTimeVariable) timeVariableArraysMap.put(timeVariableTarget, timeArrayDouble);
		variableArraysMap.put(targetVariable, valueArrayDouble);
	}

}
