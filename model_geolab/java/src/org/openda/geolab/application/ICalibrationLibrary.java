package org.openda.geolab.application;

import java.io.File;

public interface ICalibrationLibrary {

	/*
		(step 2) Start OpenDA, providing the configuration
		odaFilePath: full path to main oda configuration file
	*/
	int initialize(File workingDir);

	/*
		(step 3) Feed the parameter definitions to the OpenDA internal model
		initialParameterValues: initial values of the parameters that have to be calibrated
		standardDeviations: standard deviations of the parameters ('band with' voor calibration)
		The algorithm will
	*/
	int observerSetObsAndStdDevs(double[] observations, double[] standardDeviations);

	/*
		(step 3) Feed the parameter definitions to the OpenDA internal model
		initialParameterValues: initial values of the parameters that have to be calibrated
		standardDeviations: standard deviations of the parameters ('band with' voor calibration)
		The algorithm will
	*/
	int modelSetParameterDefinitions(double[] initialParameterValues, double[] standardDeviations);

	/*
		(step 6) Feed the model results to the OpenDA internal model
		modelResults: the results of the soil model computation for the last set of parameters
	*/
	int modelSetResults(double[] modelResults);

	/*
		(step 7) Check if the algoritm desires more evaluations
		returns: list of new parameter values. null if no evaluations needed any more
	*/
	double[] algorithmGetNextParameterValues();

	/*
		(step 8) Get the final result
		returns: list of optimal parameter values.
	*/
	double[] algorithmGetOptimalParameterValues();

	String getExceptionMessage();

	String getExceptionStackTrace();

}
