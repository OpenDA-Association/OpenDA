package org.openda.geolab;

import org.openda.interfaces.*;
import org.openda.utils.Vector;

import java.io.File;

public class CalibrationLibraryStochObserver implements IStochObserver {

	private final double[] observations;
	private final double[] standardDeviations;

	public CalibrationLibraryStochObserver(double[] observations, double[] standardDeviations) {
		this.observations = observations;
		this.standardDeviations = standardDeviations;
	}

	@Override
	public IStochObserver createSelection(String selection) {
		throw new RuntimeException("org.openda.observers.GeolabStochObserver.createSelection() not implemented yet");

	}

	@Override
	public IStochObserver createSelection(ITime selectionTimes) {
		throw new RuntimeException("org.openda.observers.GeolabStochObserver.createSelection() not implemented yet");

	}

	@Override
	public IStochObserver createSelection(int[] selector) {
		throw new RuntimeException("org.openda.observers.GeolabStochObserver.createSelection() not implemented yet");

	}

	@Override
	public IStochObserver createSelection(Type observationType) {
		throw new RuntimeException("org.openda.observers.GeolabStochObserver.createSelection() not implemented yet");

	}

	@Override
	public ISelector createSelector(Type observationType) {
		throw new RuntimeException("org.openda.observers.GeolabStochObserver.createSelector() not implemented yet");

	}

	@Override
	public int getCount() {
		throw new RuntimeException("org.openda.observers.GeolabStochObserver.getCount() not implemented yet");

	}

	@Override
	public IVector getValues() {
		return new Vector(observations);
	}

	@Override
	public IVector getRealizations() {
		throw new RuntimeException("org.openda.observers.GeolabStochObserver.getRealizations() not implemented yet");

	}

	@Override
	public IVector getExpectations() {
		throw new RuntimeException("org.openda.observers.GeolabStochObserver.getExpectations() not implemented yet");

	}

	@Override
	public double evaluatePDF(IVector values) {
		throw new RuntimeException("org.openda.observers.GeolabStochObserver.evaluatePDF() not implemented yet");

	}

	@Override
	public IVector evaluateMarginalPDFs(IVector values) {
		throw new RuntimeException("org.openda.observers.GeolabStochObserver.evaluateMarginalPDFs() not implemented yet");

	}

	@Override
	public ISqrtCovariance getSqrtCovariance() {
		throw new RuntimeException("org.openda.observers.GeolabStochObserver.getSqrtCovariance() not implemented yet");

	}

	@Override
	public IVector getStandardDeviations() {
		return new Vector(standardDeviations);
	}

	@Override
	public ITime[] getTimes() {
		throw new RuntimeException("org.openda.observers.GeolabStochObserver.getTimes() not implemented yet");

	}

	@Override
	public void free() {
		throw new RuntimeException("org.openda.observers.GeolabStochObserver.free() not implemented yet");

	}

	@Override
	public IObservationDescriptions getObservationDescriptions() {
		throw new RuntimeException("org.openda.observers.GeolabStochObserver.getObservationDescriptions() not implemented yet");

	}

	@Override
	public void setParent(IInstance parent) {
		throw new RuntimeException("org.openda.observers.GeolabStochObserver.setParent() not implemented yet");

	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
		throw new RuntimeException("org.openda.observers.GeolabStochObserver.initialize() not implemented yet");

	}

	@Override
	public IInstance getParent() {
		throw new RuntimeException("org.openda.observers.GeolabStochObserver.getParent() not implemented yet");

	}
}
