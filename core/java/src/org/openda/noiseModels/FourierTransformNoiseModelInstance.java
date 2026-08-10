package org.openda.noiseModels;

import org.openda.interfaces.*;
import org.openda.utils.Instance;

import java.io.File;

public class FourierTransformNoiseModelInstance extends Instance implements IStochModelInstance {

	@Override
	public IVector getState() {
		return null;
	}

	@Override
	public IVector getState(int iDomain) {
		return null;
	}

	@Override
	public void axpyOnState(double alpha, IVector vector) {

	}

	@Override
	public void axpyOnState(double alpha, IVector vector, int iDomain) {

	}

	@Override
	public IVector getParameters() {
		return null;
	}

	@Override
	public void setParameters(IVector parameters) {

	}

	@Override
	public void axpyOnParameters(double alpha, IVector vector) {

	}

	@Override
	public IStochVector getStateUncertainty() {
		return null;
	}

	@Override
	public IStochVector getParameterUncertainty() {
		return null;
	}

	@Override
	public IStochVector[] getWhiteNoiseUncertainty(ITime time) {
		return new IStochVector[0];
	}

	@Override
	public boolean isWhiteNoiseStationary() {
		return false;
	}

	@Override
	public ITime[] getWhiteNoiseTimes(ITime timeSpan) {
		return new ITime[0];
	}

	@Override
	public IVector[] getWhiteNoise(ITime timeSpan) {
		return new IVector[0];
	}

	@Override
	public void setWhiteNoise(IVector[] whiteNoise) {

	}

	@Override
	public void axpyOnWhiteNoise(double alpha, IVector[] vector) {

	}

	@Override
	public void setAutomaticNoiseGeneration(boolean value) {

	}

	@Override
	public IObservationOperator getObservationOperator() {
		return null;
	}

	@Override
	public void announceObservedValues(IObservationDescriptions observationDescriptions) {

	}

	@Override
	public IVector getStateScaling() {
		return null;
	}

	@Override
	public IVector[] getStateScaling(IObservationDescriptions observationDescriptions) {
		return new IVector[0];
	}

	@Override
	public IExchangeItem getExchangeItem(String exchangeItemID) {
		return null;
	}

	@Override
	public ITime getTimeHorizon() {
		return null;
	}

	@Override
	public ITime getCurrentTime() {
		return null;
	}

	@Override
	public void compute(ITime targetTime) {
		// create realizations from stds and frequencies?
		// apply time correlation?
	}

	@Override
	public ILocalizationDomains getLocalizationDomains() {
		return null;
	}

	@Override
	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {
		return new IVector[0];
	}

	@Override
	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance, int iDomain) {
		return new IVector[0];
	}

	@Override
	public IModelState saveInternalState() {
		return null;
	}

	@Override
	public void restoreInternalState(IModelState savedInternalState) {

	}

	@Override
	public void releaseInternalState(IModelState savedInternalState) {

	}

	@Override
	public IModelState loadPersistentState(File persistentStateFile) {
		return null;
	}

	@Override
	public File getModelRunDir() {
		return null;
	}

	@Override
	public String[] getExchangeItemIDs() {
		return new String[0];
	}

	@Override
	public String[] getExchangeItemIDs(IExchangeItem.Role role) {
		return new String[0];
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return null;
	}

	@Override
	public void finish() {

	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
		// read config
		// Standard deviations per frequency?
		// Power of 2 2D frequencies with Real and Imaginary numbers
		// Read grid
		// Read time horizon?
		// Determine timeCorrelationScale?
		// Create dummy exchange item
		// Create FourierTransform stochVector
		// Create state, vector with x and y sizes
	}
}
