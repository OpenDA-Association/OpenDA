package org.openda.externalfile;

import org.openda.exchange.dataobjects.NetcdfDataObject;
import org.openda.interfaces.*;
import org.openda.observationOperators.ObservationOperatorDeprecatedModel;
import org.openda.utils.StochVector;
import org.openda.utils.Time;
import org.openda.utils.Vector;
import org.openda.utils.io.AsciiFileUtils;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

public class ExternalFileModelInstance implements IStochModelInstance, IStochModelInstanceDeprecated, Cloneable {

	private final String modelResultsFile;
	private Time time;
	private Vector parameterVector;
	private Vector modelResults;
	private final File exchangeDir;
	private final File runDir;
	private final StochVector parameterUncertainties;
	private final Time fakeTime = new Time(58119,58120,1d/24d);
	private final File modelParFile;
	private final File modelParFinalFile;

	public ExternalFileModelInstance(String modelParametersFileName, String modelResultsFile, File runDir) {

		this.exchangeDir = new File(runDir, "exchangeDir");
		this.runDir = runDir;
		modelParFile = new File(exchangeDir, modelParametersFileName);
		modelParFinalFile = new File(exchangeDir, "finalModelParFile.txt");
		//stdDevParFile = new File(runDir, "stdDev" + modelParametersFileName);

		double[] parameterValuesFromFile = readValuesFromFile(modelParFile);
		this.parameterVector = new Vector(parameterValuesFromFile);

		this.modelResultsFile = modelResultsFile;
		double[] stdDevs = new double[parameterValuesFromFile.length];
		Arrays.fill(stdDevs, 0.1);
		this.parameterUncertainties = new StochVector(parameterValuesFromFile, stdDevs);
		NetcdfDataObject netcdfDataScalarTimeSeriesDataObject = null;
		try {
			 netcdfDataScalarTimeSeriesDataObject = new NetcdfDataObject();
			 netcdfDataScalarTimeSeriesDataObject.initialize(runDir, new String[]{modelResultsFile, "true", "false"});
			 String[] exchangeItemIDs = netcdfDataScalarTimeSeriesDataObject.getExchangeItemIDs();
			 if (exchangeItemIDs.length != 1)
				 throw new RuntimeException("Only 1 exchange item supported currently but " + exchangeItemIDs.length + " found in " + new File(runDir, modelResultsFile).getAbsolutePath());
			 IExchangeItem dataObjectExchangeItem = netcdfDataScalarTimeSeriesDataObject.getDataObjectExchangeItem(exchangeItemIDs[0]);
			time = getTime(dataObjectExchangeItem);
			double[] valuesAsDoubles = dataObjectExchangeItem.getValuesAsDoubles();
			modelResults = new Vector(valuesAsDoubles);
		} finally {
			if (netcdfDataScalarTimeSeriesDataObject != null) netcdfDataScalarTimeSeriesDataObject.finish();
		}
	}

	private Time getTime(IExchangeItem dataObjectExchangeItem) {
		ITimeInfo timeInfo = dataObjectExchangeItem.getTimeInfo();
		double[] times = timeInfo.getTimes();
		if (times.length == 1) return new Time(times[0]);
		double firstTime = times[0];
		double secondTime = times[1];
		double endTime = times[times.length - 1];
		double deltaTasMJD = secondTime - firstTime;
		return new Time(firstTime - deltaTasMJD, endTime, deltaTasMJD);
	}

	public IVector getState() {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getState() not implemented yet");

	}

	public IVector getState(int iDomain) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getState() not implemented yet");

	}

	public void axpyOnState(double alpha, IVector vector) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.axpyOnState() not implemented yet");

	}

	public void axpyOnState(double alpha, IVector vector, int iDomain) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.axpyOnState() not implemented yet");

	}

	public IVector getParameters() {
		if (parameterVector == null) {
			throw new RuntimeException("CalLibStochModelInstance.getParameters(): initialParameterVector == null");
		}
		return parameterVector;
	}

	public void setParameters(IVector parameters) {
		System.out.println("ExternalSocketModelInstance.setParameters: " + parameters);
		parameterVector = new Vector(parameters.getValues());
	}

	public void axpyOnParameters(double alpha, IVector vector) {
		Vector parameterVector = new Vector(this.parameterVector.getValues());
		parameterVector.axpy(alpha, vector);
		this.parameterVector = parameterVector.clone();
	}

	public IStochVector getStateUncertainty() {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getStateUncertainty() not implemented yet");

	}

	public IStochVector getParameterUncertainty() {
		return parameterUncertainties;
	}

	public IStochVector[] getWhiteNoiseUncertainty(ITime time) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getWhiteNoiseUncertainty() not implemented yet");

	}

	public boolean isWhiteNoiseStationary() {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.isWhiteNoiseStationary() not implemented yet");

	}

	public ITime[] getWhiteNoiseTimes(ITime timeSpan) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getWhiteNoiseTimes() not implemented yet");

	}

	public IVector[] getWhiteNoise(ITime timeSpan) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getWhiteNoise() not implemented yet");

	}

	public void setWhiteNoise(IVector[] whiteNoise) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.setWhiteNoise() not implemented yet");

	}

	public void axpyOnWhiteNoise(double alpha, IVector[] vector) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.axpyOnWhiteNoise() not implemented yet");

	}

	public void setAutomaticNoiseGeneration(boolean value) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.setAutomaticNoiseGeneration() not implemented yet");

	}

	public IObservationOperator getObservationOperator() {
		return new ObservationOperatorDeprecatedModel(this);

	}

	public void announceObservedValues(IObservationDescriptions observationDescriptions) {

	}

	public IVector getStateScaling() {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getStateScaling() not implemented yet");

	}

	public IVector[] getStateScaling(IObservationDescriptions observationDescriptions) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getStateScaling() not implemented yet");

	}

	public IPrevExchangeItem getExchangeItem(String exchangeItemID) {
		return null;
	}

	public ITime getTimeHorizon() {
		return time;
	}

	public ITime getCurrentTime() {
		return fakeTime;
	}

	public void compute(ITime targetTime) {

		if (modelParFile.exists()) {
			boolean delete = modelParFile.delete();
			while (!delete) {
				delete = modelParFile.delete();
				try {
					Thread.sleep(10L);
				} catch (InterruptedException e) {
					throw new RuntimeException(e.getMessage(), e);
				}
			}
		}
		try {
			FileWriter writer = new FileWriter(modelParFile, false);
			for (int i = 0; i < parameterVector.getSize(); i++) {
				String value = String.valueOf(parameterVector.getValue(i));
				writer.write(value);
				writer.write("\n");
			}
			writer.close();
		} catch (Exception e) {
			throw new RuntimeException(e.getMessage(), e);
		}

		// Tdod make configurable
		File externalSigFile = new File(exchangeDir, "FewsCanRun.sig");
		try {
			boolean newFile = externalSigFile.createNewFile();
		} catch (IOException e) {
			throw new RuntimeException(e.getMessage(), e);
		}

		File openDASigFile = new File(exchangeDir, "OpenDACanRun.sig");
		while (!openDASigFile.exists()) {
			try {
				Thread.sleep(10L);
			} catch (InterruptedException e) {
				throw new RuntimeException(e.getMessage(), e);
			}
		}
		if (openDASigFile.exists()) {
			boolean delete = openDASigFile.delete();
			while (!delete) {
				delete = openDASigFile.delete();
			}
		}

		this.modelResults = getModelResults();
	}

	private Vector getModelResults() {
		NetcdfDataObject netcdfDataScalarTimeSeriesDataObject = new NetcdfDataObject();
		netcdfDataScalarTimeSeriesDataObject.initialize(runDir, new String[]{modelResultsFile, "true", "false"});
		String[] exchangeItemIDs = netcdfDataScalarTimeSeriesDataObject.getExchangeItemIDs();
		Vector vector = null;
		for (int i = 0; i < exchangeItemIDs.length; i++) {
			IExchangeItem dataObjectExchangeItem = netcdfDataScalarTimeSeriesDataObject.getDataObjectExchangeItem(exchangeItemIDs[i]);
			double[] valuesAsDoubles = dataObjectExchangeItem.getValuesAsDoubles();
			vector = new Vector(valuesAsDoubles);
		}
		netcdfDataScalarTimeSeriesDataObject.finish();
		return vector;
	}

	private double[] readValuesFromFile(File modelResults) {
		List<String> lines = AsciiFileUtils.readLines(modelResults);
		int size = lines.size();
		double[] receivedValues = new double[size];
		for (int i = 0; i < size; i++) {
			receivedValues[i] = Double.parseDouble(lines.get(i));
		}
		return receivedValues;
	}

	public ILocalizationDomains getLocalizationDomains() {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getLocalizationDomains() not implemented yet");

	}

	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getObservedLocalization() not implemented yet");

	}

	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance, int iDomain) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getObservedLocalization() not implemented yet");

	}

	public IModelState saveInternalState() {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.saveInternalState() not implemented yet");

	}

	public void restoreInternalState(IModelState savedInternalState) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.restoreInternalState() not implemented yet");

	}

	public void releaseInternalState(IModelState savedInternalState) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.releaseInternalState() not implemented yet");

	}

	public IModelState loadPersistentState(File persistentStateFile) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.loadPersistentState() not implemented yet");

	}

	public File getModelRunDir() {
		return exchangeDir;
	}

	public String[] getExchangeItemIDs() {
		return new String[0];
	}

	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getExchangeItemIDs() not implemented yet");

	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return null;
	}

	public void finish() {

	}

	public void initialize(File workingDir, String[] arguments) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.initialize() not implemented yet");

	}

	public IInstance getParent() {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getParent() not implemented yet");

	}

	public IVector getObservedValues(IObservationDescriptions observationDescriptions) {
		return modelResults;
	}

	void sendFinalParameters() {
		try {
			FileWriter writer = new FileWriter(modelParFinalFile, false);
			for (int i = 0; i < parameterVector.getSize(); i++) {
				String value = String.valueOf(parameterVector.getValue(i));
				writer.write(value);
				writer.write("\n");
			}
			writer.close();
		} catch (Exception e) {
			throw new RuntimeException(e.getMessage(), e);
		}

	}
}
