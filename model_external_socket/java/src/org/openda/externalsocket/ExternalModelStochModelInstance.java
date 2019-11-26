package org.openda.externalsocket;

import org.openda.interfaces.*;
import org.openda.observationOperators.ObservationOperatorDeprecatedModel;
import org.openda.utils.StochVector;
import org.openda.utils.Time;
import org.openda.utils.Vector;

import java.io.File;

public class ExternalModelStochModelInstance implements IStochModelInstance, IStochModelInstanceDeprecated, Cloneable {

	private int portNumber;
	private Vector parameterVector;
	private Vector modelResults;
	private final double[] lowerBounds;
	private final double[] upperBounds;
	private final StochVector parameterUncertainties;

	public ExternalModelStochModelInstance(int portNumber, double[] parameterValues, double[] standardDeviations, double[] lowerBounds, double[] upperBounds) {
		this.portNumber = portNumber;
		this.parameterVector = new Vector(parameterValues);
		this.lowerBounds = lowerBounds;
		this.upperBounds = upperBounds;
		this.parameterUncertainties = new StochVector(parameterValues, standardDeviations);
	}

	public IVector getState() {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.getState() not implemented yet");

	}

	public IVector getState(int iDomain) {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.getState() not implemented yet");

	}

	public void axpyOnState(double alpha, IVector vector) {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.axpyOnState() not implemented yet");

	}

	public void axpyOnState(double alpha, IVector vector, int iDomain) {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.axpyOnState() not implemented yet");

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
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.getStateUncertainty() not implemented yet");

	}

	public IStochVector getParameterUncertainty() {
		return parameterUncertainties;
	}

	public IStochVector[] getWhiteNoiseUncertainty(ITime time) {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.getWhiteNoiseUncertainty() not implemented yet");

	}

	public boolean isWhiteNoiseStationary() {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.isWhiteNoiseStationary() not implemented yet");

	}

	public ITime[] getWhiteNoiseTimes(ITime timeSpan) {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.getWhiteNoiseTimes() not implemented yet");

	}

	public IVector[] getWhiteNoise(ITime timeSpan) {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.getWhiteNoise() not implemented yet");

	}

	public void setWhiteNoise(IVector[] whiteNoise) {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.setWhiteNoise() not implemented yet");

	}

	public void axpyOnWhiteNoise(double alpha, IVector[] vector) {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.axpyOnWhiteNoise() not implemented yet");

	}

	public void setAutomaticNoiseGeneration(boolean value) {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.setAutomaticNoiseGeneration() not implemented yet");

	}

	public IObservationOperator getObservationOperator() {
		return new ObservationOperatorDeprecatedModel(this);

	}

	public void announceObservedValues(IObservationDescriptions observationDescriptions) {

	}

	public IVector getStateScaling() {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.getStateScaling() not implemented yet");

	}

	public IVector[] getStateScaling(IObservationDescriptions observationDescriptions) {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.getStateScaling() not implemented yet");

	}

	public IPrevExchangeItem getExchangeItem(String exchangeItemID) {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.getExchangeItem() not implemented yet");

	}

	public ITime getTimeHorizon() {
		// Fake time
		return new Time(58119, 58120, 1d / 24d);
	}

	public ITime getCurrentTime() {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.getCurrentTime() not implemented yet");

	}

	public void compute(ITime targetTime) {
		// No action needed (modelResults are se externally
	}

	public ILocalizationDomains getLocalizationDomains() {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.getLocalizationDomains() not implemented yet");

	}

	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.getObservedLocalization() not implemented yet");

	}

	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance, int iDomain) {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.getObservedLocalization() not implemented yet");

	}

	public IModelState saveInternalState() {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.saveInternalState() not implemented yet");

	}

	public void restoreInternalState(IModelState savedInternalState) {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.restoreInternalState() not implemented yet");

	}

	public void releaseInternalState(IModelState savedInternalState) {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.releaseInternalState() not implemented yet");

	}

	public IModelState loadPersistentState(File persistentStateFile) {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.loadPersistentState() not implemented yet");

	}

	public File getModelRunDir() {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.getModelRunDir() not implemented yet");

	}

	public String[] getExchangeItemIDs() {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.getExchangeItemIDs() not implemented yet");

	}

	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.getExchangeItemIDs() not implemented yet");

	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.getDataObjectExchangeItem() not implemented yet");

	}

	public void finish() {
		// no action needed (yet)

	}

	public void initialize(File workingDir, String[] arguments) {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.initialize() not implemented yet");

	}

	public IInstance getParent() {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelInstance.getParent() not implemented yet");

	}

	public IVector getObservedValues(IObservationDescriptions observationDescriptions) {
		SocketClient socketClient = new SocketClient(portNumber);
		int size = parameterVector.getSize();
		StringBuilder stringBuilder = new StringBuilder(10);
		stringBuilder.append("Z:");
		for (int i = 0; i < size; i++) {
			stringBuilder.append(parameterVector.getValue(i));
			stringBuilder.append(';');
		}
		String received = socketClient.sendAndReceive(stringBuilder.toString());
		String[] split = received.split(";");
		double[] receivedValues = new double[split.length];
		for (int i = 0; i < split.length; i++) {
			receivedValues[i] = Double.parseDouble(split[i]);
		}
		modelResults = new Vector(receivedValues);
		return modelResults;
	}
}
