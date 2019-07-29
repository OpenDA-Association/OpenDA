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
import org.openda.resultwriters.CsvResultWriter;
import org.openda.resultwriters.PythonResultWriter;
import org.openda.utils.Results;

import java.io.File;
import java.util.*;

public class CalibrationLibrary implements ICalibrationLibrary {

	private CalLibStochObserver stochObserver = null;
	private CalLibStochModelFactory stochModelFactory = null;
	private File workingDir = null;
	private String exceptionErrmsg = null;
	private StackTraceElement[] exceptionStackTrace = null;
	private ICalLibAlgorithm algorithm = null;
	private double[] optimalParameterValues = null;
	private PythonResultWriter pythonResultWriter;
	private CalLibResultWriter calLibResultWriter = new CalLibResultWriter();

	private LinkedHashMap<String, CalLibAlgorithmSettings> algorithmSettings = new LinkedHashMap<>();

	@SuppressWarnings("WeakerAccess")
	public CalibrationLibrary() {
	}

	public int initialize(File workingDir) {

		try {
			this.workingDir = workingDir;

			stochModelFactory = null;
			CalLibStochModelFactory.stochModelInstance = null;
			algorithm = null;

			String algorithmName = "Dud";
			algorithmSettings.put(algorithmName, new CalLibAlgorithmSettings(CalLibDudAlgorithm.getConfigStringTemplate()));

			pythonResultWriter = new PythonResultWriter(workingDir, algorithmName + "-results.py");
			Results.addResultWriter(pythonResultWriter);
			Results.addResultWriter(calLibResultWriter);
		} catch (Exception e) {
			exceptionErrmsg = e.getMessage();
			exceptionStackTrace = e.getStackTrace();
		}

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

	public List<String> getAlgorithmNames() {
		return new ArrayList<>(algorithmSettings.keySet());
	}

	public List<String> getAlgorithmSettingNames(String algorithmName) {
		if (!algorithmSettings.containsKey(algorithmName)) {
			throw new RuntimeException("Unknown algorithm name: " + algorithmName);
		}
		return new ArrayList<>(algorithmSettings.get(algorithmName).getSettings().keySet());
	}

	public String getAlgorithmSettingValue(String algorithmName, String settingName) {
		LinkedHashMap<String, CalLibAlgorithmSetting> settings = findSetting(algorithmName, settingName);
		return settings.get(settingName).getValue();
	}

	public String getAlgorithmSettingDefault(String algorithmName, String settingName) {
		LinkedHashMap<String, CalLibAlgorithmSetting> settings = findSetting(algorithmName, settingName);
		return settings.get(settingName).getDefaultValue();
	}

	public void setAlgorithmSettingValue(String algorithmName, String settingName, String value) {
		LinkedHashMap<String, CalLibAlgorithmSetting> settings = findSetting(algorithmName, settingName);
		settings.get(settingName).setValue(value);
	}

	public void setAlgorithmSettingValue(String algorithmName, String settingName, double value) {
		LinkedHashMap<String, CalLibAlgorithmSetting> settings = findSetting(algorithmName, settingName);
		settings.get(settingName).setValue(value);
	}

	public void setAlgorithmSettingValue(String algorithmName, String settingName, int value) {
		LinkedHashMap<String, CalLibAlgorithmSetting> settings = findSetting(algorithmName, settingName);
		settings.get(settingName).setValue(value);
	}

	private LinkedHashMap<String, CalLibAlgorithmSetting> findSetting(String algorithmName, String settingName) {
		if (!algorithmSettings.containsKey(algorithmName)) {
			throw new RuntimeException("Unknown algorithm name: " + algorithmName);
		}
		LinkedHashMap<String, CalLibAlgorithmSetting> settings = algorithmSettings.get(algorithmName).getSettings();
		if (!settings.containsKey(settingName)) {
			throw new RuntimeException("Unknown settingName \""+ settingName+ "\" for algorithm \"" + algorithmName + "\"");
		}
		return settings;
	}

	public double[] algorithmGetOptimalParameterValues() {
		// pythonResultWriter.free();
		// TODO: Calling this pythonResultWriter.free() method leads to a nearly empty file.
		//       For now, accept that sequential calibration runs are appended to the log file.
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
		algorithm.initialize(workingDir, new String[] {algorithmSettings.get("Dud").getConfigString()});
		algorithm.setStochComponents(observer, modelFactory);
		return algorithm;
	}

	public int getMessageCount() {
		return calLibResultWriter.getMessageCount();
	}

	public String getNextMessage() {
		return calLibResultWriter.getNextMessage();
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
