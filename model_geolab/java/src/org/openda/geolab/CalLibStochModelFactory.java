package org.openda.geolab;

import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IStochModelPostProcessor;

import java.io.File;

public class CalLibStochModelFactory implements IStochModelFactory {

	private final double[] initialParameterValues;
	private final double[] standardDeviations;
	private CalibrationCommunicator calibrationCommunicator;

	CalLibStochModelFactory(double[] initialParameterValues, double[] standardDeviations, CalibrationCommunicator calibrationCommunicator) {
		this.initialParameterValues = initialParameterValues;
		this.standardDeviations = standardDeviations;
		this.calibrationCommunicator = calibrationCommunicator;
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

	public IStochModelInstance getInstance(OutputLevel outputLevel) {
		CalLibStochModelInstance stochModelInstance = new CalLibStochModelInstance(initialParameterValues, standardDeviations, calibrationCommunicator);
		stochModelInstance.setAlgorithmDoneFlag(ExitStatus.RUNNING);
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
}
