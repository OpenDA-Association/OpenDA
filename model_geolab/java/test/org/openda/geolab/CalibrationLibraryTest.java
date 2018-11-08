package org.openda.geolab;

import junit.framework.TestCase;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.util.Arrays;

public class CalibrationLibraryTest extends TestCase {

	private File testRunDataDir;

	protected void setUp() {
		OpenDaTestSupport testData = new OpenDaTestSupport(CalibrationLibraryTest.class, "model_geolab");
		this.testRunDataDir = testData.getTestRunDataDir();
	}

	public void testSinus() {
		CalibrationLibrary calibrationLibrary = new CalibrationLibrary();

		double amplitudeObs = 2;
		double periodObs = 0.5;
		double phaseObs = 0;
		double offsetObs = 0;

		calibrationLibrary.initialize(testRunDataDir);

		double[] obsParams = {amplitudeObs, periodObs, phaseObs, offsetObs};
		double[] observations = evaluateSinus(obsParams);
		double[] observationStandardDeviations = new double[100];
		Arrays.fill(observationStandardDeviations, 0.2);
		calibrationLibrary.observerSetObsAndStdDevs(observations, observationStandardDeviations);

		double amplitudeInitial = 2.1;
		double periodInitial = 0.55;
		double phaseInitial = 0.1;
		double offsetInitial = 0.1;

		double[] initialParams = {amplitudeInitial, periodInitial, phaseInitial, offsetInitial};
		double[] parameterStandardDeviations = new double[]{0.2, 0.2, 0.2, 40};
		calibrationLibrary.modelSetParameterDefinitions(initialParams, parameterStandardDeviations);

		double[] modelResults = evaluateSinus(initialParams);
		calibrationLibrary.modelSetResults(modelResults);
		for (int i = 0; i < initialParams.length; i++) {
			double[] params = Arrays.copyOf(initialParams, 4);
			params[i] += parameterStandardDeviations[i];
			modelResults = evaluateSinus(params);
			calibrationLibrary.modelSetResults(modelResults);
		}

		double[] nextParameterValues = calibrationLibrary.algorithmGetNextParameterValues();
		while (nextParameterValues != null && nextParameterValues.length == 4) {
			modelResults = evaluateSinus(nextParameterValues);
			calibrationLibrary.modelSetResults(modelResults);
			nextParameterValues = calibrationLibrary.algorithmGetNextParameterValues();
		}
		double[] optimalParameterValues = calibrationLibrary.algorithmGetOptimalParameterValues();
		for (int i = 0; i < optimalParameterValues.length; i++) {
			assertEquals(obsParams[i], optimalParameterValues[i]);
		}
	}

	private double[] evaluateSinus(double[] doubles) {
		int size = 100;
		double[] observations = new double[size];
		for (int i = 0; i < size; i++) {
			double x = i * 0.1;
			observations[i] = doubles[0] * Math.sin(x / doubles[1] + doubles[2]) + doubles[3];
		}
		return observations;
	}
}
