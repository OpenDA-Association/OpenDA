package org.openda.fewsworkflow;

import org.openda.exchange.DoubleExchangeItem;
import org.openda.interfaces.*;
import org.openda.observationOperators.ObservationOperatorDeprecatedModel;
import org.openda.utils.StochVector;
import org.openda.utils.Time;
import org.openda.utils.Vector;
import org.openda.utils.io.AsciiFileUtils;

import java.io.*;
import java.util.LinkedHashMap;
import java.util.List;

public class FewsWorkflowModelInstance implements IStochModelInstance, IStochModelInstanceDeprecated, Cloneable {

	private final String modelResultsFile;
	private Vector parameterVector;
	private Vector modelResults;
	private File dummyModelDir;
	private final StochVector parameterUncertainties;
	private LinkedHashMap<String, DoubleExchangeItem> exchangeItems = new LinkedHashMap<>();
	private Time fakeTime = new Time(58119,58120,1d/24d);
	private File modelParFile;

	public FewsWorkflowModelInstance(String modelParametersFileName, String modelResultsFile, File dummyModelDir) {

		modelParFile = new File(dummyModelDir, modelParametersFileName);

		double[] parameterValuesFromFile = readValuesFromFile(modelParFile);
		this.parameterVector = new Vector(parameterValuesFromFile);

		this.modelResultsFile = modelResultsFile;
		this.dummyModelDir = dummyModelDir;
		this.parameterUncertainties = new StochVector(parameterValuesFromFile, new double[]{0.4, 0.8, 1.6});
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
		return exchangeItems.get(exchangeItemID);
	}

	public ITime getTimeHorizon() {
		return fakeTime;
	}

	public ITime getCurrentTime() {
		return fakeTime;
	}

	public void compute(ITime targetTime) {

		if (modelParFile.exists()) modelParFile.delete();
		try {
			FileWriter writer = new FileWriter(modelParFile, false);
			for (int i = 0; i < parameterVector.getSize(); i++) {
				writer.write(String.valueOf(parameterVector.getValue(i)));
				writer.write("\n");
			}
			writer.close();
		} catch (Exception e) {
			throw new RuntimeException(e.getMessage(), e);
		}

		// Tdod make configurable
		File externalSigFile = new File(dummyModelDir, "FewsCanRun.sig");
		try {
			boolean newFile = externalSigFile.createNewFile();
		} catch (IOException e) {
			throw new RuntimeException(e.getMessage(), e);
		}

		File openDASigFile = new File(dummyModelDir, "OpenDACanRun.sig");
		while (!openDASigFile.exists()) {
			try {
				Thread.sleep(100L);
			} catch (InterruptedException e) {
				throw new RuntimeException(e.getMessage(), e);
			}
		}
		openDASigFile.delete();

		File modelResults = new File(dummyModelDir, modelResultsFile);
		double[] receivedValues = readValuesFromFile(modelResults);

		this.modelResults = new Vector(receivedValues);
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
		return dummyModelDir;
	}

	public String[] getExchangeItemIDs() {
		return exchangeItems.keySet().toArray(new String[0]);
	}

	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		throw new RuntimeException("org.openda.externalsocket.ExternalSocketModelInstance.getExchangeItemIDs() not implemented yet");

	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return exchangeItems.get(exchangeItemID);

	}

	public void finish() {
		// TODO EP socket client send final parameters

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
}
