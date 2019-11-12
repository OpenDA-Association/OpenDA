package org.openda.geolab;

import org.openda.interfaces.*;
import org.openda.observationOperators.ObservationOperatorDeprecatedModel;
import org.openda.utils.StochVector;
import org.openda.utils.Time;
import org.openda.utils.Vector;

import java.io.File;

public class CalLibStochModelInstance implements IStochModelInstance, IStochModelInstanceDeprecated, IClonableStochModelInstance, Cloneable {

	private Vector initialParameterVector;
	private CalibrationCommunicator calibrationCommunicator;

	private final StochVector parameterUncertainties;

	CalLibStochModelInstance(double[] parameterValues, double[] standardDeviations, CalibrationCommunicator calibrationCommunicator) {
		this.initialParameterVector = new Vector(parameterValues);
		this.calibrationCommunicator = calibrationCommunicator;
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
		if (initialParameterVector == null) {
			throw new RuntimeException("CalLibStochModelInstance.getParameters(): initialParameterVector == null");
		}
		return initialParameterVector;
	}

	public void setParameters(IVector parameters) {
		calibrationCommunicator.setParameters(parameters);
		initialParameterVector = new Vector(parameters.getValues());
// Next code was add to check results when calibration is run in python
// Introduce a debug (level) flag and reactivate
//		System.out.print("parameters set from algorithm:");
//		for (int i = 0; i < parameterVector.getSize(); i++) {
//			System.out.print(" " + parameterVector.getValue(i));
//		}
//		System.out.println("");
	}

	public void axpyOnParameters(double alpha, IVector vector) {
		Vector parameterVector = new Vector(initialParameterVector.getValues());
		parameterVector.axpy(alpha, vector);
		calibrationCommunicator.setParameters(parameterVector);
		initialParameterVector = parameterVector.clone();
// Next code was add to check results when calibration is run in python
// Introduce a debug (level) flag and reactivate
//		System.out.print("parameters axpy'd from algorithm:");
//		for (int i = 0; i < parameterVector.getSize(); i++) {
//			System.out.print(" " + parameterVector.getValue(i));
//		}
//		System.out.println("");
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
		if (!(observationDescriptions instanceof GeolabCalObservationDescriptions)) {
			throw new RuntimeException("Unexpected type " + observationDescriptions.getClass() +
				"org.openda.geolab.CalLibStochModelInstance.announceObservedValues()");
		}
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
		return new Time(58119, 58120, 1d/24d);
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
		if (!(observationDescriptions instanceof GeolabCalObservationDescriptions)) {
			throw new RuntimeException("Unexpected type " + observationDescriptions.getClass() +
				"org.openda.geolab.CalLibStochModelInstance.announceObservedValues()");
		}
// Next code was add to check results when calibration is run in python
// Introduce a debug (level) flag and reactivate
//		System.out.print("model results to algorithm:");
//		for (int i = 0; i < modelResults.getSize(); i++) {
//			System.out.print(" " + modelResults.getValue(i));
//		}
//		System.out.println("");
		return calibrationCommunicator.getModelResults();
	}

	void setAlgorithmDoneFlag(ExitStatus exitStatus) {
		setAlgorithmDoneFlag(exitStatus, null);
	}

	void setAlgorithmDoneFlag(ExitStatus exitStatus, String errorString) {
		calibrationCommunicator.setExitStatus(exitStatus);
		if (errorString != null) calibrationCommunicator.setErrorString(errorString);
	}

	public IStochModelInstance getCopyOf() {
		try {
			return (IStochModelInstance) this.clone();
		} catch (CloneNotSupportedException e) {
			throw new RuntimeException("Could not clone CalLibStochModelInstance");
		}
	}

	public Object clone() throws CloneNotSupportedException {
		return super.clone();
	}
}
