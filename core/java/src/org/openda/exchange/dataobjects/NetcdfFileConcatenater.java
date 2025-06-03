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
import org.openda.utils.generalJavaUtils.StringUtilities;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import ucar.ma2.*;
import ucar.nc2.*;
import ucar.nc2.units.DateUnit;

import java.io.File;
import java.io.IOException;
import java.util.*;

public class NetcdfFileConcatenater {

	private static Logger LOGGER = LoggerFactory.getLogger(NetcdfFileConcatenater.class);

	public static void main(String[] arguments) {
		if (arguments.length < 2) {
			throw new IllegalArgumentException("NetcdfFileConcatenater expects at least two arguments:\n" +
					"<targetNetcdfFile> and <netcdfFileToBeAdded>");
		}
		boolean useOldValueOnOverlap = false;
		for (int i = 2; i < arguments.length; i++) {
			String argument = arguments[i];
			String[] keyValue = StringUtilities.getKeyValuePair(argument);
			String key = keyValue[0];
			String value = keyValue[1];
			switch (key) {
				case "useOldValueOnOverlap":
					useOldValueOnOverlap = Boolean.valueOf(value);
					break;
				default:
					throw new RuntimeException("Unknown key " + key + ". Please specify only useOldValueOnOverlap as key=value pair");
			}
		}
		// file must be there. If it is indeed, the path has been made absolute by BBaction
		File netcdfFileToBeAdded = new File(arguments[1]);

		// file may be there. If so, the path has been made absolute by BBaction
		File targetNetcdfFile = new File(arguments[0]);
		if (!targetNetcdfFile.isAbsolute()) {
			// target file is not there yet; make path absolute
			targetNetcdfFile = new File(netcdfFileToBeAdded.getParentFile(), arguments[0]);
		}

		if (netcdfFileToBeAdded.isDirectory()) {
			List<File> netcdfFilesToBeAdded = new ArrayList<>(Arrays.asList(Objects.requireNonNull(netcdfFileToBeAdded.listFiles())));
			netcdfFilesToBeAdded.sort(Comparator.comparing(File::getAbsolutePath));

			try {
				if (!targetNetcdfFile.exists()) {
					File firstNetcdfFileToBeAdded = netcdfFilesToBeAdded.get(0);

					addNetcdfFile(targetNetcdfFile, firstNetcdfFileToBeAdded);

					netcdfFilesToBeAdded.remove(firstNetcdfFileToBeAdded);
				}

				if (!netcdfFilesToBeAdded.isEmpty()) {
					concatenateNetcdfFiles(useOldValueOnOverlap, netcdfFilesToBeAdded, targetNetcdfFile);
				}

			} catch (IOException e) {
				throw new RuntimeException(e.getMessage(), e);
			}
		} else {
			try {
				if (!targetNetcdfFile.exists()) {
					addNetcdfFile(targetNetcdfFile, netcdfFileToBeAdded);
				} else {
					concatenateNetcdfFiles(useOldValueOnOverlap, netcdfFileToBeAdded, targetNetcdfFile);
				}
			} catch (IOException e) {
				throw new RuntimeException(e.getMessage(), e);
			}
		}
	}

	private static void addNetcdfFile(File targetNetcdfFile, File netcdfFileToBeAdded) throws IOException {
		NetcdfFile netcdfToAdd = null;
		try {
			netcdfToAdd = NetcdfFile.open(netcdfFileToBeAdded.getAbsolutePath());
			Dimension time = netcdfToAdd.findDimension("time");
			if (time != null && time.isUnlimited()) {
				BBUtils.copyFile(netcdfFileToBeAdded, targetNetcdfFile);
			} else {
				rewriteNetcdfFile(targetNetcdfFile, netcdfToAdd);
			}
		} finally {
			if (netcdfToAdd != null) netcdfToAdd.close();
		}
	}

	private static void concatenateNetcdfFiles(boolean useOldValueOnOverlap, List<File> netcdfFilesToBeAdded, File targetNetcdfFile) throws IOException {
		NetcdfFile sourceNetcdfFile = null;
		NetcdfFileWriter netcdfFileWriter = null;
		try {
			netcdfFileWriter = NetcdfFileWriter.openExisting(targetNetcdfFile.getAbsolutePath());

			Map<Variable, Array> variableArraysMap = new HashMap<>();
			Map<Variable, Array> timeVariableArraysMap = new HashMap<>();

			for (File netcdfFileToBeAdded : netcdfFilesToBeAdded) {
				sourceNetcdfFile = NetcdfFile.open(netcdfFileToBeAdded.getAbsolutePath());
				List<Variable> variablesToBeAdded = sourceNetcdfFile.getVariables();

				concatenateVariables(useOldValueOnOverlap, sourceNetcdfFile, variablesToBeAdded, netcdfFileWriter, variableArraysMap, timeVariableArraysMap);

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
			}
		} finally {
			if (netcdfFileWriter != null) netcdfFileWriter.close();
			if (sourceNetcdfFile != null) sourceNetcdfFile.close();
		}
	}

	private static void concatenateNetcdfFiles(boolean useOldValueOnOverlap, File netcdfFileToBeAdded, File targetNetcdfFile) throws IOException {
		NetcdfFile sourceNetcdfFile = null;
		NetcdfFileWriter netcdfFileWriter = null;
		try {
			sourceNetcdfFile = NetcdfFile.open(netcdfFileToBeAdded.getAbsolutePath());
			List<Variable> variablesToBeAdded = sourceNetcdfFile.getVariables();
			netcdfFileWriter = NetcdfFileWriter.openExisting(targetNetcdfFile.getAbsolutePath());

			Map<Variable, Array> variableArraysMap = new HashMap<>();
			Map<Variable, Array> timeVariableArraysMap = new HashMap<>();

			concatenateVariables(useOldValueOnOverlap, sourceNetcdfFile, variablesToBeAdded, netcdfFileWriter, variableArraysMap, timeVariableArraysMap);
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
		} finally {
			if (netcdfFileWriter != null) netcdfFileWriter.close();
			if (sourceNetcdfFile != null) sourceNetcdfFile.close();
		}
	}

	private static void concatenateVariables(boolean useOldValueOnOverlap, NetcdfFile sourceNetCdfFile, List<Variable> variablesToBeAdded, NetcdfFileWriter netcdfFileWriter, Map<Variable, Array> variableArraysMap, Map<Variable, Array> timeVariableArraysMap) throws IOException {
		for (Variable variable : variablesToBeAdded) {
			if (NetcdfUtils.isTimeVariable(variable)) continue;
			Variable timeVariableToBeAdded = NetcdfUtils.findTimeVariableForVariable(variable, sourceNetCdfFile);
			if (timeVariableToBeAdded == null) continue;
			Variable targetVariable = netcdfFileWriter.findVariable(variable.getFullNameEscaped());
			if (targetVariable == null) continue;
			Variable timeVariableTarget = NetcdfUtils.findTimeVariableForVariable(targetVariable, netcdfFileWriter.getNetcdfFile());
			if (timeVariableTarget == null) continue;

			List<Dimension> addedDimensions = variable.getDimensions();
			if (addedDimensions.size() != 2 && addedDimensions.size() != 3 ) {
				LOGGER.warn("Cannot concatenate '{}'", targetVariable.getShortName());
				continue;
			}
			List<Dimension> targetDimensions = targetVariable.getDimensions();
			if (targetDimensions.size() != addedDimensions.size())
				throw new RuntimeException(String.format("Dimensions mismatch for variable '%s' when concatenating files.", variable.getShortName()));

			for (int i = 1; i < targetDimensions.size(); i++) {
				Dimension targetLocationDimension = targetDimensions.get(i);
				Dimension addedLocationDimension = addedDimensions.get(i);
				int targetLocationDimensionLength = targetLocationDimension.getLength();
				if (addedLocationDimension.getLength() != targetLocationDimensionLength) throw new RuntimeException("Variables from source and target must have same location dimension size");
			}
			double[] targetValues = (double[]) targetVariable.read().get1DJavaArray(Double.TYPE);
			double[] addedValues = (double[]) variable.read().get1DJavaArray(Double.TYPE);

			Array read = timeVariableTarget.read();
			double[] timesTarget = (double[]) read.get1DJavaArray(Double.TYPE);
			String timeVariableTargetUnitsString = timeVariableTarget.getUnitsString();
			double[] timesToBeAdded = (double[]) timeVariableToBeAdded.read().get1DJavaArray(Double.TYPE);
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
			VariablesToJoin variablesToJoin = new VariablesToJoin(variableArraysMap, timeVariableArraysMap, targetVariable, timeVariableTarget);
			ArraysToJoin arraysToJoin = new ArraysToJoin(targetDimensions, targetValues, addedValues, timesTarget, convertedTimesToBeAdded);
			if (convertedTimesToBeAdded[0] == timesTarget[timesTarget.length - 1]) {
				addConcatenatedValueArraysToMaps(variablesToJoin, arraysToJoin, totalTimesCombined, true, useOldValueOnOverlap);
			} else {
				addConcatenatedValueArraysToMaps(variablesToJoin, arraysToJoin, totalTimesCombined, false, false);
			}
		}
	}


	private static void rewriteNetcdfFile(File targetNetcdfFile, NetcdfFile netcdfToAdd) throws IOException {
		NetcdfFileWriter netcdfWriter = null;

		try {
			netcdfWriter = NetcdfFileWriter.createNew(NetcdfFileWriter.Version.netcdf3, targetNetcdfFile.getCanonicalPath());

			redefineVariablesAndDimensions(netcdfToAdd, netcdfWriter);

			netcdfWriter.create();

			writeValues(netcdfToAdd, netcdfWriter);
		} catch (InvalidRangeException e) {
			throw new RuntimeException(e.getMessage(), e);
		} finally {
			if (netcdfWriter != null) netcdfWriter.close();
		}
	}

	private static void writeValues(NetcdfFile netcdfToAdd, NetcdfFileWriter netcdfWriter) throws IOException, InvalidRangeException {
		List<Variable> variables = netcdfToAdd.getVariables();
		for (Variable variable : variables) {
			String fullNameEscaped = variable.getFullNameEscaped();
			Variable netcdfFileVariable = netcdfWriter.findVariable(fullNameEscaped);
			Array read = variable.read();
			netcdfWriter.write(netcdfFileVariable, read);
		}
	}

	private static void addConcatenatedValueArraysToMaps(VariablesToJoin variablesToJoin, ArraysToJoin arraysToJoin, int totalTimesCombined, boolean firstAddedTimeOverlapping, boolean useOldValueOnOverlap) {
		if (firstAddedTimeOverlapping) totalTimesCombined--;
		ArrayDouble.D1 timeArrayDouble = new ArrayDouble.D1(totalTimesCombined);
		ArrayDouble valueArrayDouble = getArray(arraysToJoin, totalTimesCombined, firstAddedTimeOverlapping, useOldValueOnOverlap, timeArrayDouble);
		variablesToJoin.getTimeVariableArraysMap().put(variablesToJoin.getTimeVariableTarget(), timeArrayDouble);
		variablesToJoin.getVariableArraysMap().put(variablesToJoin.getTargetVariable(), valueArrayDouble);
	}

	private static ArrayDouble getArray(ArraysToJoin arraysToJoin, int totalTimesCombined, boolean firstAddedTimeOverlapping, boolean useOldValueOnOverlap, ArrayDouble.D1 timeArrayDouble) {
		if (arraysToJoin.getTargetLocationDimensions().size() == 2)
			return get2DArray(arraysToJoin, totalTimesCombined, firstAddedTimeOverlapping, useOldValueOnOverlap, timeArrayDouble);
		if (arraysToJoin.getTargetLocationDimensions().size() == 3)
			return get3DArray(arraysToJoin, totalTimesCombined, firstAddedTimeOverlapping, useOldValueOnOverlap, timeArrayDouble);
		// should not happen
		assert false;
		return null;
	}

	private static ArrayDouble get3DArray(ArraysToJoin arraysToJoin, int totalTimesCombined, boolean firstAddedTimeOverlapping, boolean useOldValueOnOverlap, ArrayDouble.D1 timeArrayDouble) {
		int targetDimensionLength1 = arraysToJoin.getTargetLocationDimensions().get(1).getLength();
		int targetDimensionLength2 = arraysToJoin.getTargetLocationDimensions().get(2).getLength();
		ArrayDouble.D3 valueArrayDoubleD3 = new ArrayDouble.D3(totalTimesCombined, targetDimensionLength1, targetDimensionLength2);

		int targetTimes = firstAddedTimeOverlapping && !useOldValueOnOverlap ? arraysToJoin.getTimesTarget().length - 1 : arraysToJoin.getTimesTarget().length;
		fillFirstPartOf3DArray(arraysToJoin.getTargetValues(), arraysToJoin.getTimesTarget(), timeArrayDouble, targetDimensionLength1, targetDimensionLength2, valueArrayDoubleD3, targetTimes);
		fillSecondPartOf3DArray(arraysToJoin.getAddedValues(), arraysToJoin.getConvertedTimesToBeAdded(), useOldValueOnOverlap, timeArrayDouble, targetDimensionLength1, targetDimensionLength2, valueArrayDoubleD3, targetTimes);
		return valueArrayDoubleD3;
	}

	private static void fillSecondPartOf3DArray(double[] addedValues, double[] convertedTimesToBeAdded, boolean useOldValueOnOverlap, ArrayDouble.D1 timeArrayDouble, int targetDimensionLength1, int targetDimensionLength2, ArrayDouble.D3 valueArrayDoubleD3, int targetTimes) {
		int length = useOldValueOnOverlap ? convertedTimesToBeAdded.length - 1 : convertedTimesToBeAdded.length;
		for (int i = 0; i < length; i++) {
			int convertedIndex = useOldValueOnOverlap ? i + 1 : i;
			timeArrayDouble.set(i + targetTimes, convertedTimesToBeAdded[convertedIndex]);
			for (int secondIndex = 0; secondIndex < targetDimensionLength1; secondIndex++) {
				for (int thirdIndex = 0; thirdIndex < targetDimensionLength2; thirdIndex++) {
					valueArrayDoubleD3.set(i + targetTimes, secondIndex, thirdIndex, addedValues[convertedIndex * targetDimensionLength1 * targetDimensionLength2 + secondIndex * targetDimensionLength2 + thirdIndex]);
				}
			}
		}
	}

	private static void fillFirstPartOf3DArray(double[] targetValues, double[] timesTarget, ArrayDouble.D1 timeArrayDouble, int targetDimensionLength1, int targetDimensionLength2, ArrayDouble.D3 valueArrayDoubleD3, int targetTimes) {
		for (int timeIndex = 0; timeIndex < targetTimes; timeIndex++) {
			timeArrayDouble.set(timeIndex, timesTarget[timeIndex]);
			for (int secondIndex = 0; secondIndex < targetDimensionLength1; secondIndex++) {
				for (int thirdIndex = 0; thirdIndex < targetDimensionLength2; thirdIndex++) {
					valueArrayDoubleD3.set(timeIndex, secondIndex, thirdIndex, targetValues[timeIndex * targetDimensionLength1 * targetDimensionLength2 + secondIndex * targetDimensionLength2 + thirdIndex]);
				}
			}
		}
	}

	private static ArrayDouble get2DArray(ArraysToJoin arraysToJoin, int totalTimesCombined, boolean firstAddedTimeOverlapping, boolean useOldValueOnOverlap, ArrayDouble.D1 timeArrayDouble) {
		int targetLocationDimensionLength = arraysToJoin.getTargetLocationDimensions().get(1).getLength();
		ArrayDouble.D2 valueArrayDoubleD2 = new ArrayDouble.D2(totalTimesCombined, targetLocationDimensionLength);

		int targetTimes = firstAddedTimeOverlapping && !useOldValueOnOverlap ? arraysToJoin.getTimesTarget().length - 1 : arraysToJoin.getTimesTarget().length;
		fillFirstPartOf2DArray(arraysToJoin.getTargetValues(), arraysToJoin.getTimesTarget(), timeArrayDouble, targetLocationDimensionLength, valueArrayDoubleD2, targetTimes);
		fillSecondPartOf2DArray(arraysToJoin.getAddedValues(), arraysToJoin.getConvertedTimesToBeAdded(), useOldValueOnOverlap, timeArrayDouble, targetLocationDimensionLength, valueArrayDoubleD2, targetTimes);
		return valueArrayDoubleD2;
	}

	private static void fillSecondPartOf2DArray(double[] addedValues, double[] convertedTimesToBeAdded, boolean useOldValueOnOverlap, ArrayDouble.D1 timeArrayDouble, int targetLocationDimensionLength, ArrayDouble.D2 valueArrayDoubleD2, int targetTimes) {
		int length = useOldValueOnOverlap ? convertedTimesToBeAdded.length - 1 : convertedTimesToBeAdded.length;
		for (int timeIndex = 0; timeIndex < length; timeIndex++) {
			int convertedIndex = useOldValueOnOverlap ? timeIndex + 1 : timeIndex;
			timeArrayDouble.set(timeIndex + targetTimes, convertedTimesToBeAdded[convertedIndex]);
			for (int secondIndex = 0; secondIndex < targetLocationDimensionLength; secondIndex++) {
				valueArrayDoubleD2.set(timeIndex + targetTimes, secondIndex, addedValues[convertedIndex * targetLocationDimensionLength + secondIndex]);
			}
		}
	}

	private static void fillFirstPartOf2DArray(double[] targetValues, double[] timesTarget, ArrayDouble.D1 timeArrayDouble, int targetLocationDimensionLength, ArrayDouble.D2 valueArrayDoubleD2, int targetTimes) {
		for (int i = 0; i < targetTimes; i++) {
			timeArrayDouble.set(i, timesTarget[i]);
			for (int j = 0; j < targetLocationDimensionLength; j++) {
				valueArrayDoubleD2.set(i, j, targetValues[i * targetLocationDimensionLength + j]);
			}
		}
	}

	private static void redefineVariablesAndDimensions(NetcdfFile source, NetcdfFileWriter target) {

		List<Dimension> dimensions = source.getDimensions();
		for (Dimension dimension : dimensions) {
			String fullNameEscaped = dimension.getFullNameEscaped();
			if ("time".equals(fullNameEscaped)) continue;
			target.addDimension(null, fullNameEscaped, dimension.getLength());
		}

		Dimension timeDimension = target.addUnlimitedDimension("time");
		List<Variable> variables = source.getVariables();
		rewriteVariables(target, variables, timeDimension);
	}

	private static void rewriteVariables(NetcdfFileWriter netcdf, List<Variable> variables, Dimension timeDimension) {
		for (Variable variable : variables) {
			List<Dimension> newDimensions = getDimensions(variable, timeDimension);
			String fullNameEscaped = variable.getFullNameEscaped();
			Variable newVariable = netcdf.addVariable(null, fullNameEscaped, variable.getDataType(), newDimensions);
			List<Attribute> attributes = variable.getAttributes();
			for (Attribute attribute : attributes) {
				netcdf.addVariableAttribute(newVariable, attribute);
			}
		}
	}

	private static List<Dimension> getDimensions(Variable variable, Dimension timeDimension) {
		List<Dimension> dimensionsAll = variable.getDimensionsAll();
		List<Dimension> newDimensions = new ArrayList<>(dimensionsAll.size());
		for (Dimension dimension : dimensionsAll) {
			switch (dimension.getFullNameEscaped()) {
				case "time":
					newDimensions.add(timeDimension);
					break;
				default:
					newDimensions.add(dimension);
					break;
			}
		}
		return newDimensions;
	}

	public static double[][] split(double[] array, int maxArrayLength) {
		if (array == null)
			throw new IllegalArgumentException("array == null");

		if (array.length <= maxArrayLength) return new double[][]{array};

		int arrayCount = array.length / maxArrayLength;
		int restArrayLength = array.length % maxArrayLength;
		if (restArrayLength != 0) arrayCount++;
		double[][] res = new double[arrayCount][];

		for (int i = 0, n = array.length / maxArrayLength; i < n; i++) {
			double[] subArray = new double[maxArrayLength];
			arraycopy(array, i * maxArrayLength, subArray, 0, maxArrayLength);
			res[i] = subArray;
		}

		if (restArrayLength != 0) {
			double[] subArray = new double[restArrayLength];
			arraycopy(array, (arrayCount - 1) * maxArrayLength, subArray, 0, restArrayLength);
			res[arrayCount - 1] = subArray;
		}

		return res;
	}

	public static void arraycopy(double[] src, int srcPos, double[] dest, int destPos, int length) {
		if (length > 10) {
			System.arraycopy(src, srcPos, dest, destPos, length);
			return;
		}

		if (src == dest && destPos > srcPos) {
			for (int i = srcPos + length - 1, j = destPos + length - 1; i >= srcPos; i--, j--) {
				dest[j] = src[i];
			}
			return;
		}

		for (int i = srcPos, j = destPos, n = srcPos + length; i < n; i++, j++) {
			dest[j] = src[i];
		}
	}

	private static class ArraysToJoin {
		private final List<Dimension> targetLocationDimensions;
		private final double[] targetValues;
		private final double[] addedValues;
		private final double[] timesTarget;
		private final double[] convertedTimesToBeAdded;

		private ArraysToJoin(List<Dimension> targetLocationDimensions, double[] targetValues, double[] addedValues, double[] timesTarget, double[] convertedTimesToBeAdded) {
			this.targetLocationDimensions = targetLocationDimensions;
			this.targetValues = targetValues;
			this.addedValues = addedValues;
			this.timesTarget = timesTarget;
			this.convertedTimesToBeAdded = convertedTimesToBeAdded;
		}

		public List<Dimension> getTargetLocationDimensions() {
			return targetLocationDimensions;
		}

		public double[] getTargetValues() {
			return targetValues;
		}

		public double[] getAddedValues() {
			return addedValues;
		}

		public double[] getTimesTarget() {
			return timesTarget;
		}

		public double[] getConvertedTimesToBeAdded() {
			return convertedTimesToBeAdded;
		}
	}

	private static class VariablesToJoin {
		private final Map<Variable, Array> variableArraysMap;
		private final Map<Variable, Array> timeVariableArraysMap;
		private final Variable targetVariable;
		private final Variable timeVariableTarget;

		private VariablesToJoin(Map<Variable, Array> variableArraysMap, Map<Variable, Array> timeVariableArraysMap, Variable targetVariable, Variable timeVariableTarget) {
			this.variableArraysMap = variableArraysMap;
			this.timeVariableArraysMap = timeVariableArraysMap;
			this.targetVariable = targetVariable;
			this.timeVariableTarget = timeVariableTarget;
		}

		public Map<Variable, Array> getVariableArraysMap() {
			return variableArraysMap;
		}

		public Map<Variable, Array> getTimeVariableArraysMap() {
			return timeVariableArraysMap;
		}

		public Variable getTargetVariable() {
			return targetVariable;
		}

		public Variable getTimeVariableTarget() {
			return timeVariableTarget;
		}
	}
}
