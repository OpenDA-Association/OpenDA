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
	private double[] resultTimes;

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
		resultTimes = timeInfo.getTimes();
		if (resultTimes.length == 1) return new Time(resultTimes[0]);
		double firstTime = resultTimes[0];
		double secondTime = resultTimes[1];
		double endTime = resultTimes[resultTimes.length - 1];
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
		// not needed
	}

	public IVector getStateScaling() {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getStateScaling() not implemented yet");

	}

	public IVector[] getStateScaling(IObservationDescriptions observationDescriptions) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getStateScaling() not implemented yet");

	}

	@Override
	public IExchangeItem getExchangeItem(String exchangeItemID) {
		throw new RuntimeException("org.openda.externalfile.ExternalFileModelInstance.getExchangeItem() not implemented yet");

	}

	public ITime getTimeHorizon() {
		return time;
	}

	public ITime getCurrentTime() {
		return fakeTime;
	}

	public void compute(ITime targetTime) {

		if (modelParFile.exists()) deleteModelParFile();

		writeNewParFile(modelParFile);

		createExternalSigFile();

		deleteOpenDASigFile();

		this.modelResults = getModelResults();
	}

	private void deleteOpenDASigFile() {
		File openDASigFile = new File(exchangeDir, "OpenDACanRun.sig");
		while (!openDASigFile.exists()) {
			try {
				Thread.sleep(10L);
			} catch (InterruptedException e) {
				throw new RuntimeException(e.getMessage(), e);
			}
		}
		if (!openDASigFile.exists()) return;
		boolean delete = openDASigFile.delete();
		while (!delete) {
			delete = openDASigFile.delete();
		}
	}

	private void createExternalSigFile() {
		File externalSigFile = new File(exchangeDir, "FewsCanRun.sig");
		try {
			boolean newFileCreated = externalSigFile.createNewFile();
			assert newFileCreated;
		} catch (IOException e) {
			throw new RuntimeException(e.getMessage(), e);
		}
	}

	private void writeNewParFile(File modelParFile) {
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
	}

	private void deleteModelParFile() {
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

	@Override
	public String[] getExchangeItemIDs(IExchangeItem.Role role) {
		return new String[0];
	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return null;
	}

	public void finish() {
		// not needed
	}

	public void initialize(File workingDir, String[] arguments) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.initialize() not implemented yet");

	}

	public IInstance getParent() {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getParent() not implemented yet");

	}

	public IVector getObservedValues(IObservationDescriptions observationDescriptions) {
		return getFilteredResults(observationDescriptions);
	}

	private Vector getFilteredResults(IObservationDescriptions observationDescriptions) {
		ITime[] times = observationDescriptions.getTimes();
		double beginMJD = time.getBeginMJD();
		double stepMJD = time.getStepMJD();
		double endMJD = time.getEndMJD();
		Vector filteredResults = new Vector(times.length);
		int count = 0;
		for (ITime oneTime : times) {
			double timeMJD = oneTime.getMJD();
			if (timeMJD > endMJD) continue;
			double diffMJD = timeMJD - beginMJD;
			if (diffMJD < 0) continue;
			int index = getIndex(stepMJD, timeMJD, diffMJD);
			if (index == -1) continue;
			double value = modelResults.getValue(index);
			filteredResults.setValue(count, value);
			count++;
		}
		for (int i = count; i < times.length; i++) {
			filteredResults.remove_entry(i);
		}
		return filteredResults;
	}

	private int getIndex(double stepMJD, double timeMJD, double diffMJD) {
		int index = (int) (diffMJD / stepMJD);
		if (index >= resultTimes.length) return Arrays.binarySearch(resultTimes, timeMJD);
		return resultTimes[index] == timeMJD ? index : Arrays.binarySearch(resultTimes, timeMJD);
	}

	void sendFinalParameters() {
		writeNewParFile(modelParFinalFile);
	}
}
