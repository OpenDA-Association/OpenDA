package org.openda.geolab;

import junit.framework.TestCase;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.util.Arrays;
import java.util.List;

public class CalibrationLibraryTest extends TestCase {

	private File testRunDataDir;

	protected void setUp() throws Exception {
		super.setUp();
		OpenDaTestSupport testData = new OpenDaTestSupport(CalibrationLibraryTest.class, "model_geolab");
		this.testRunDataDir = testData.getTestRunDataDir();
	}

	public void testAlgorithmSettings() {
		CalibrationLibrary calibrationLibrary = new CalibrationLibrary();
		calibrationLibrary.initialize(testRunDataDir);
		List<String> algorithmNames = calibrationLibrary.getAlgorithmNames();
		assertEquals(1, algorithmNames.size());
		String algorithmName = algorithmNames.get(0);
		assertEquals("Dud", algorithmName);
		List<String> settingNames = calibrationLibrary.getAlgorithmSettingNames(algorithmName);
		assertEquals(8, settingNames.size());
		String settingName = settingNames.get(5);
		assertEquals("lineSearch@maxRelStepSize", settingName);
		assertEquals("3", calibrationLibrary.getAlgorithmSettingDefault(algorithmName, settingName));
		assertEquals("3", calibrationLibrary.getAlgorithmSettingValue(algorithmName, settingName));
		try {
			calibrationLibrary.setAlgorithmSettingValue(algorithmName, settingName, "9.9");
		} catch (Exception e) {
			assertTrue(e.getMessage().contains("setting double value for integer settings-attribute"));
		}
		assertEquals("3", calibrationLibrary.getAlgorithmSettingDefault(algorithmName, settingName));
	}

	public void testSinus() {
		CalibrationLibrary calibrationLibrary = new CalibrationLibrary();
		calibrateSinusParams(calibrationLibrary, false);
	}

	public void testSinusTwice() {
		CalibrationLibrary calibrationLibrary = new CalibrationLibrary();
		System.out.print("\nStart first calibration\n\n");
		calibrateSinusParams(calibrationLibrary, false);
		System.out.print("\nFirst calibration done\n\n");
		System.out.print("\nStart second calibration\n\n");
		calibrateSinusParams(calibrationLibrary, false);
		System.out.print("\nSecond calibration done\n\n");
	}

	public void testIfOptimumIsReturned() {
		CalibrationLibrary calibrationLibrary = new CalibrationLibrary();
		calibrateSinusParams(calibrationLibrary, true);
	}

	private void calibrateSinusParams(CalibrationLibrary calibrationLibrary, boolean useKnownValues) {
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

		double amplitudeInitial;
		double periodInitial;
		double phaseInitial;
		double offsetInitial;
		if (useKnownValues) {
			amplitudeInitial = 2;
			periodInitial = 0.5;
			phaseInitial = 0;
			offsetInitial = 0;
		} else {
			amplitudeInitial = 2.1;
			periodInitial = 0.55;
			phaseInitial = 0.1;
			offsetInitial = 0.1;
		}

		double[] initialParams = {amplitudeInitial, periodInitial, phaseInitial, offsetInitial};
		double[] parameterStandardDeviations = new double[]{0.2, 0.05, 0.02, 0.02};
		calibrationLibrary.modelSetParameterDefinitions(initialParams, parameterStandardDeviations);

		double[] nextParameterValues = calibrationLibrary.algorithmGetNextParameterValues();
		while (nextParameterValues != null && nextParameterValues.length == 4) {
			double[] modelResults = evaluateSinus(nextParameterValues);
			calibrationLibrary.modelSetResults(modelResults);
			nextParameterValues = calibrationLibrary.algorithmGetNextParameterValues();
			while (calibrationLibrary.getMessageCount() > 0) {
				System.out.println(calibrationLibrary.getNextMessage());
			}
		}
		double[] optimalParameterValues = calibrationLibrary.algorithmGetOptimalParameterValues();
		double[] expectedValuesCalib = {1.594726, 0.591106, 0.085015, 0.075891};
		double[] expectedValuesKnown = {2, 0.5, 0, 0};
		double[] expectedValues = useKnownValues ? expectedValuesKnown : expectedValuesCalib;
		for (int i = 0; i < optimalParameterValues.length; i++) {
			assertEquals(expectedValues[i], optimalParameterValues[i], 1.e-6);
		}
	}

	private double[] evaluateSinus(double[] doubles) {
		double amplitude = doubles[0];
		double period = doubles[1];
		double phase = doubles[2];
		double offset = doubles[3];
		int size = 100;
		double[] observations = new double[size];
		for (int i = 0; i < size; i++) {
			observations[i] = amplitude * Math.sin(phase + 1.0d * i / period) + offset;
		}
		return observations;
	}
}
