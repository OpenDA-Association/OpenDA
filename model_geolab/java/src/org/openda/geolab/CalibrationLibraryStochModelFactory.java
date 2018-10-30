package org.openda.geolab;

import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IStochModelPostProcessor;

import java.io.File;

public class CalibrationLibraryStochModelFactory implements IStochModelFactory {

	private final double[] initialParameterValues;
	private final double[] standardDeviations;

	public CalibrationLibraryStochModelFactory(double[] initialParameterValues, double[] standardDeviations) {
		this.initialParameterValues = initialParameterValues;
		this.standardDeviations = standardDeviations;
	}

	@Override
	public IStochModelInstance getInstance(OutputLevel outputLevel) {
		return new CalibrationLibraryStochModelInstance(initialParameterValues, standardDeviations);
	}

	@Override
	public IStochModelPostProcessor getPostprocessorInstance(File instanceDir) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelFactory.getPostprocessorInstance() not implemented yet");

	}

	@Override
	public void finish() {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelFactory.finish() not implemented yet");

	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelFactory.initialize() not implemented yet");

	}
}
