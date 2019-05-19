package org.openda.geolab;

import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IStochModelPostProcessor;

import java.io.File;

public class CalLibStochModelFactory implements IStochModelFactory {

	private final double[] initialParameterValues;
	private final double[] standardDeviations;
	static CalLibStochModelInstance stochModelInstance;

	CalLibStochModelFactory(double[] initialParameterValues, double[] standardDeviations) {
		this.initialParameterValues = initialParameterValues;
		this.standardDeviations = standardDeviations;
// Next code was add to check results when calibration is run in python
// Introduce a debug (level) flag and reactivate
//		System.out.print("initial parameters:");
//		for (int i = 0; i < initialParameterValues.length; i++) {
//			System.out.print(" " + initialParameterValues[i]);
//		}
//		System.out.println("");
//		System.out.print("parStdDevs:");
//		for (int i = 0; i < standardDeviations.length; i++) {
//			System.out.print(" " + standardDeviations[i]);
//		}
//		System.out.println("");
	}

	static void setlastModelResults(double[] modelResults) {
		stochModelInstance.setModelResults(modelResults);
	}

	public IStochModelInstance getInstance(OutputLevel outputLevel) {
		if (stochModelInstance == null) {
			stochModelInstance = new CalLibStochModelInstance(initialParameterValues, standardDeviations);
		}
		stochModelInstance.setAlgorithmDoneFlag(CalLibStochModelInstance.ExitStatus.RUNNING);
		return stochModelInstance;
	}

	public IStochModelPostProcessor getPostprocessorInstance(File instanceDir) {
		throw new RuntimeException("org.openda.geolab.CalLibStochModelFactory.getPostprocessorInstance() not implemented yet");
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
