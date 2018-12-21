package org.openda.geolab;

import org.openda.interfaces.*;
import org.openda.observationOperators.ObservationOperatorDeprecatedModel;
import org.openda.utils.StochVector;
import org.openda.utils.Time;
import org.openda.utils.Vector;

import java.io.File;

public class CalibrationLibraryStochModelInstance implements IStochModelInstance, IStochModelInstanceDeprecated {

	private Vector modelResults = null;
	private Vector initialParameterVector;

	private final StochVector parameterUncertainties;
	private Vector parameterVector = null;
	private boolean algorithmDone = false;

	private final int sleepTimeInMillis = 200;

	CalibrationLibraryStochModelInstance(double[] parameterValues, double[] standardDeviations) {
		this.initialParameterVector = new Vector(parameterValues);
		this.parameterUncertainties = new StochVector(parameterValues, standardDeviations);
	}

	public IVector getState() {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getState() not implemented yet");

	}

	public IVector getState(int iDomain) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getState() not implemented yet");

	}

	public void axpyOnState(double alpha, IVector vector) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.axpyOnState() not implemented yet");

	}

	public void axpyOnState(double alpha, IVector vector, int iDomain) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.axpyOnState() not implemented yet");

	}

	public IVector getParameters() {
		if (initialParameterVector == null) {
			throw new RuntimeException("CalibrationLibraryStochModelInstance.getParameters(): initialParameterVector == null");
		}
		return initialParameterVector;
	}

	public void setParameters(IVector parameters) {
		parameterVector = new Vector(parameters.getValues());
// Next code was add to check results when calibration is run in python
// Introduce a debug (level) flag and reactivate
//		System.out.print("parameters set from algorithm:");
//		for (int i = 0; i < parameterVector.getSize(); i++) {
//			System.out.print(" " + parameterVector.getValue(i));
//		}
//		System.out.println("");
	}

	public void axpyOnParameters(double alpha, IVector vector) {
		parameterVector = new Vector(initialParameterVector.getValues());
		parameterVector.axpy(alpha, vector);
// Next code was add to check results when calibration is run in python
// Introduce a debug (level) flag and reactivate
//		System.out.print("parameters axpy'd from algorithm:");
//		for (int i = 0; i < parameterVector.getSize(); i++) {
//			System.out.print(" " + parameterVector.getValue(i));
//		}
//		System.out.println("");
	}

	public IStochVector getStateUncertainty() {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getStateUncertainty() not implemented yet");

	}

	public IStochVector getParameterUncertainty() {
		return parameterUncertainties;
	}

	public IStochVector[] getWhiteNoiseUncertainty(ITime time) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getWhiteNoiseUncertainty() not implemented yet");

	}

	public boolean isWhiteNoiseStationary() {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.isWhiteNoiseStationary() not implemented yet");

	}

	public ITime[] getWhiteNoiseTimes(ITime timeSpan) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getWhiteNoiseTimes() not implemented yet");

	}

	public IVector[] getWhiteNoise(ITime timeSpan) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getWhiteNoise() not implemented yet");

	}

	public void setWhiteNoise(IVector[] whiteNoise) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.setWhiteNoise() not implemented yet");

	}

	public void axpyOnWhiteNoise(double alpha, IVector[] vector) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.axpyOnWhiteNoise() not implemented yet");

	}

	public void setAutomaticNoiseGeneration(boolean value) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.setAutomaticNoiseGeneration() not implemented yet");

	}

	public IObservationOperator getObservationOperator() {
		return new ObservationOperatorDeprecatedModel(this);

	}

	public void announceObservedValues(IObservationDescriptions observationDescriptions) {
		if (!(observationDescriptions instanceof GeolabCalObservationDescriptions)) {
			throw new RuntimeException("Unexpected type " + observationDescriptions.getClass() +
				"org.openda.geolab.CalibrationLibraryStochModelInstance.announceObservedValues()");
		}
	}

	public IVector getStateScaling() {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getStateScaling() not implemented yet");

	}

	public IVector[] getStateScaling(IObservationDescriptions observationDescriptions) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getStateScaling() not implemented yet");

	}

	public IPrevExchangeItem getExchangeItem(String exchangeItemID) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getExchangeItem() not implemented yet");

	}

	public ITime getTimeHorizon() {
		// Fake time
		return new Time(58119, 58120, 1d/24d);
	}

	public ITime getCurrentTime() {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getCurrentTime() not implemented yet");

	}

	public void compute(ITime targetTime) {
		// No action needed (modelResults are se externally
	}

	public ILocalizationDomains getLocalizationDomains() {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getLocalizationDomains() not implemented yet");

	}

	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getObservedLocalization() not implemented yet");

	}

	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance, int iDomain) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getObservedLocalization() not implemented yet");

	}

	public IModelState saveInternalState() {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.saveInternalState() not implemented yet");

	}

	public void restoreInternalState(IModelState savedInternalState) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.restoreInternalState() not implemented yet");

	}

	public void releaseInternalState(IModelState savedInternalState) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.releaseInternalState() not implemented yet");

	}

	public IModelState loadPersistentState(File persistentStateFile) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.loadPersistentState() not implemented yet");

	}

	public File getModelRunDir() {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getModelRunDir() not implemented yet");

	}

	public String[] getExchangeItemIDs() {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getExchangeItemIDs() not implemented yet");

	}

	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getExchangeItemIDs() not implemented yet");

	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getDataObjectExchangeItem() not implemented yet");

	}

	public void finish() {
		// no action needed (yet)

	}

	public void initialize(File workingDir, String[] arguments) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.initialize() not implemented yet");

	}

	public IInstance getParent() {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getParent() not implemented yet");

	}

	public IVector getObservedValues(IObservationDescriptions observationDescriptions) {
		if (!(observationDescriptions instanceof GeolabCalObservationDescriptions)) {
			throw new RuntimeException("Unexpected type " + observationDescriptions.getClass() +
				"org.openda.geolab.CalibrationLibraryStochModelInstance.announceObservedValues()");
		}
		while (modelResults == null) {
			try {
				Thread.sleep(sleepTimeInMillis);
			} catch (InterruptedException e) {
				throw new RuntimeException("Thread that runs the CalibrationLibraryStochModelInstance has been interrupted");
			}
		}
// Next code was add to check results when calibration is run in python
// Introduce a debug (level) flag and reactivate
//		System.out.print("model results to algorithm:");
//		for (int i = 0; i < modelResults.getSize(); i++) {
//			System.out.print(" " + modelResults.getValue(i));
//		}
//		System.out.println("");
		IVector observedValues = modelResults;
		modelResults = null;
		return observedValues;
	}

	double[] getParametersAsSetByAlgorithm() {
		while (parameterVector == null && !algorithmDone) {
			try {
				Thread.sleep(sleepTimeInMillis);
			} catch (InterruptedException e) {
				throw new RuntimeException("Thread that runs the CalibrationLibraryStochModelInstance has been interrupted");
			}
		}
		if (algorithmDone) {
			return null;
		} else {
			double[] parameterValues = parameterVector.getValues();
			parameterVector = null;
			return parameterValues;
		}
	}

	void setModelResults(double[] modelResults) {
		this.modelResults = new Vector(modelResults);
	}

	void setAlgorithmDoneFlag(boolean flag) {
		algorithmDone = flag;
	}
}
