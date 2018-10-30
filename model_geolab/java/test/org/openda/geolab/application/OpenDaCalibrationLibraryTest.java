package org.openda.geolab.application;

import junit.framework.TestCase;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;

public class OpenDaCalibrationLibraryTest extends TestCase {
	private File testRunDataDir;

	protected void setUp() throws IOException {
		OpenDaTestSupport testData = new OpenDaTestSupport(OpenDaCalibrationLibraryTest.class, "model_geolab");
		this.testRunDataDir = testData.getTestRunDataDir();
	}

	public void testSinus() {
		OpenDaCalibrationLibrary openDaCalibrationLibrary = new OpenDaCalibrationLibrary();

		double amplitudeObs = 2;
		double periodObs = 0.5;
		double phaseObs = 0;
		double offsetObs = 0;

		double[] obsParams = {amplitudeObs, periodObs, phaseObs, offsetObs};
		double[] observations = evaluateSinus(obsParams);
		double[] observationStandardDeviations = new double[100];
		Arrays.fill(observationStandardDeviations, 0.2);
		openDaCalibrationLibrary.observerSetObsAndStdDevs(observations, observationStandardDeviations);

		double amplitudeInitial = 2.1;
		double periodInitial = 0.55;
		double phaseInitial = 0.1;
		double offsetInitial = 0.1;

		double[] initialParams = {amplitudeInitial, periodInitial, phaseInitial, offsetInitial};
		double[] parameterStandardDeviations = new double[]{0.2, 0.2, 0.2, 40};
		openDaCalibrationLibrary.modelSetParameterDefinitions(initialParams, parameterStandardDeviations);
		double[] modelResults = evaluateSinus(initialParams);
		openDaCalibrationLibrary.modelSetResults(modelResults);

		double[] nextParameterValues = openDaCalibrationLibrary.algorithmGetNextParameterValues();
		while (nextParameterValues != null && nextParameterValues.length == 4) {
			modelResults = evaluateSinus(nextParameterValues);
			openDaCalibrationLibrary.modelSetResults(modelResults);
			nextParameterValues = openDaCalibrationLibrary.algorithmGetNextParameterValues();
		}
		double[] optimalParameterValues = openDaCalibrationLibrary.algorithmGetOptimalParameterValues();
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
