package org.openda.geolab;

import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.IStochModelPostProcessor;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class CalibrationLibraryStochModelFactory implements IStochModelFactory {

	private static List<double[]> modelResults = null;
	private final double[] initialParameterValues;
	private final double[] standardDeviations;

	CalibrationLibraryStochModelFactory(double[] initialParameterValues, double[] standardDeviations) {
		this.initialParameterValues = initialParameterValues;
		this.standardDeviations = standardDeviations;
	}

	static void setlastModelResults(double[] lastModelResults) {
		if (modelResults == null) {
			modelResults = new ArrayList<>();
			// add two dummu set of results (because getInstance will be called twice for params/uncert., with computing
			modelResults.add(new double[1]);
			modelResults.add(new double[1]);
		}
		modelResults.add(lastModelResults);
	}

	public IStochModelInstance getInstance(OutputLevel outputLevel) {
		if (modelResults == null) {
			throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelFactory.getInstance(): no model result have been set yet");
		}
		if (modelResults.size() == 0) {
			throw new RuntimeException("org.openda.geolab.CalibrationLibraryStochModelFactory.getInstance(): More models instances requested than number of available results sets");
		}
		double[] resultsForModelInstance = CalibrationLibraryStochModelFactory.modelResults.get(0);
		modelResults.remove(0);
		return new CalibrationLibraryStochModelInstance(initialParameterValues, standardDeviations, resultsForModelInstance);
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
