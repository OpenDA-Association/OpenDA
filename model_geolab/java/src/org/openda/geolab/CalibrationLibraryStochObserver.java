package org.openda.geolab;

import org.openda.interfaces.*;
import org.openda.utils.Vector;

import java.io.File;

public class CalibrationLibraryStochObserver implements IStochObserver {

	private final double[] observations;
	private final double[] standardDeviations;

	CalibrationLibraryStochObserver(double[] observations, double[] standardDeviations) {
		this.observations = observations;
		this.standardDeviations = standardDeviations;
// Next code was add to check results when calibration is run in python
// Introduce a debug (level) flag and reactivate
//		System.out.println("observations:");
//		for (int i = 0; i < observations.length; i++) {
//			System.out.println(" " + observations[i]);
//		}
//		System.out.println("");
//		System.out.println("obs std devs:");
//		for (int i = 0; i < observations.length; i++) {
//			System.out.println(" " + standardDeviations[i]);
//		}
//		System.out.println("");
	}

	@Override
	public IStochObserver createSelection(String selection) {
		throw new RuntimeException("org.openda.observers.GeolabStochObserver.createSelection() not implemented yet");

	}

	@Override
	public IStochObserver createSelection(ITime selectionTimes) {
		return this;
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
		return observations.length;

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
		return new Vector(observations);
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
		return new GeolabCalObservationDescriptions(observations);

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
