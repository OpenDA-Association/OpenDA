package org.openda.geolab;

import org.openda.interfaces.*;
import org.openda.utils.StochVector;
import org.openda.utils.Vector;

import java.io.File;

public class CalibrationLibraryStochModelInstance implements IStochModelInstance {
	private final Vector parameterVector;
	private final StochVector parameterUncertainties;

	public CalibrationLibraryStochModelInstance(double[] parameterValues, double[] standardDeviations) {
		this.parameterVector = new Vector(parameterValues);
		this.parameterUncertainties = new StochVector(parameterValues, standardDeviations);
	}

	@Override
	public IVector getState() {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getState() not implemented yet");

	}

	@Override
	public IVector getState(int iDomain) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getState() not implemented yet");

	}

	@Override
	public void axpyOnState(double alpha, IVector vector) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.axpyOnState() not implemented yet");

	}

	@Override
	public void axpyOnState(double alpha, IVector vector, int iDomain) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.axpyOnState() not implemented yet");

	}

	@Override
	public IVector getParameters() {
		return parameterVector;
	}

	@Override
	public void setParameters(IVector parameters) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.setParameters() not implemented yet");

	}

	@Override
	public void axpyOnParameters(double alpha, IVector vector) {
		parameterVector.axpy(alpha, vector);
	}

	@Override
	public IStochVector getStateUncertainty() {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getStateUncertainty() not implemented yet");

	}

	@Override
	public IStochVector getParameterUncertainty() {
		return parameterUncertainties;
	}

	@Override
	public IStochVector[] getWhiteNoiseUncertainty(ITime time) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getWhiteNoiseUncertainty() not implemented yet");

	}

	@Override
	public boolean isWhiteNoiseStationary() {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.isWhiteNoiseStationary() not implemented yet");

	}

	@Override
	public ITime[] getWhiteNoiseTimes(ITime timeSpan) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getWhiteNoiseTimes() not implemented yet");

	}

	@Override
	public IVector[] getWhiteNoise(ITime timeSpan) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getWhiteNoise() not implemented yet");

	}

	@Override
	public void setWhiteNoise(IVector[] whiteNoise) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.setWhiteNoise() not implemented yet");

	}

	@Override
	public void axpyOnWhiteNoise(double alpha, IVector[] vector) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.axpyOnWhiteNoise() not implemented yet");

	}

	@Override
	public void setAutomaticNoiseGeneration(boolean value) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.setAutomaticNoiseGeneration() not implemented yet");

	}

	@Override
	public IObservationOperator getObservationOperator() {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getObservationOperator() not implemented yet");

	}

	@Override
	public void announceObservedValues(IObservationDescriptions observationDescriptions) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.announceObservedValues() not implemented yet");

	}

	@Override
	public IVector getStateScaling() {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getStateScaling() not implemented yet");

	}

	@Override
	public IVector[] getStateScaling(IObservationDescriptions observationDescriptions) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getStateScaling() not implemented yet");

	}

	@Override
	public IPrevExchangeItem getExchangeItem(String exchangeItemID) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getExchangeItem() not implemented yet");

	}

	@Override
	public ITime getTimeHorizon() {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getTimeHorizon() not implemented yet");

	}

	@Override
	public ITime getCurrentTime() {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getCurrentTime() not implemented yet");

	}

	@Override
	public void compute(ITime targetTime) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.compute() not implemented yet");

	}

	@Override
	public ILocalizationDomains getLocalizationDomains() {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getLocalizationDomains() not implemented yet");

	}

	@Override
	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getObservedLocalization() not implemented yet");

	}

	@Override
	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance, int iDomain) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getObservedLocalization() not implemented yet");

	}

	@Override
	public IModelState saveInternalState() {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.saveInternalState() not implemented yet");

	}

	@Override
	public void restoreInternalState(IModelState savedInternalState) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.restoreInternalState() not implemented yet");

	}

	@Override
	public void releaseInternalState(IModelState savedInternalState) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.releaseInternalState() not implemented yet");

	}

	@Override
	public IModelState loadPersistentState(File persistentStateFile) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.loadPersistentState() not implemented yet");

	}

	@Override
	public File getModelRunDir() {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getModelRunDir() not implemented yet");

	}

	@Override
	public String[] getExchangeItemIDs() {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getExchangeItemIDs() not implemented yet");

	}

	@Override
	public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getExchangeItemIDs() not implemented yet");

	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getDataObjectExchangeItem() not implemented yet");

	}

	@Override
	public void finish() {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.finish() not implemented yet");

	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.initialize() not implemented yet");

	}

	@Override
	public IInstance getParent() {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelInstance.getParent() not implemented yet");

	}
}
