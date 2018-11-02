package org.openda.geolab;

import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IStochModelPostProcessor;

import java.io.File;

public class CalibrationLibraryStochModelFactory implements IStochModelFactory {

	private static double[]
		modelResults;
	private final double[] initialParameterValues;
	private final double[] standardDeviations;

	public CalibrationLibraryStochModelFactory(double[] initialParameterValues, double[] standardDeviations) {
		this.initialParameterValues = initialParameterValues;
		this.standardDeviations = standardDeviations;
	}

	public static void setlastModelResults(double[] modelResults) {
		CalibrationLibraryStochModelFactory.modelResults = modelResults;
	}

	public IStochModelInstance getInstance(OutputLevel outputLevel) {
		return new CalibrationLibraryStochModelInstance(initialParameterValues, standardDeviations, modelResults);
	}

	public IStochModelPostProcessor getPostprocessorInstance(File instanceDir) {
		throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelFactory.getPostprocessorInstance() not implemented yet");

	}

	public void finish() {
		// no action needed
	}

	public void initialize(File workingDir, String[] arguments) {
		// no action needed
	}
}
