package org.openda.geolab;

import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IStochModelPostProcessor;

import java.io.File;

public class CalibrationLibraryStochModelFactory implements IStochModelFactory {

	private final double[] initialParameterValues;
	private final double[] standardDeviations;
	private static CalibrationLibraryStochModelInstance stochModelInstance;

	CalibrationLibraryStochModelFactory(double[] initialParameterValues, double[] standardDeviations) {
		this.initialParameterValues = initialParameterValues;
		this.standardDeviations = standardDeviations;
	}

	static void setlastModelResults(double[] modelResults) {
		stochModelInstance.setModelResults(modelResults);
	}

	public IStochModelInstance getInstance(OutputLevel outputLevel) {
		if (stochModelInstance == null) {
			stochModelInstance = new CalibrationLibraryStochModelInstance(initialParameterValues, standardDeviations);
		}
		return stochModelInstance;
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

	static double[] waitForNextParams() {
		return stochModelInstance.getParametersAsSetByAlgorithm();
	}
}
