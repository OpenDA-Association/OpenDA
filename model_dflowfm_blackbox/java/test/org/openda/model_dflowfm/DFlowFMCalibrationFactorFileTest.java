package org.openda.model_dflowfm;

import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.io.AsciiFileUtils;

import java.io.File;

/**
 * Created by pelgrim on 21-Jun-17.
 */
public class DFlowFMCalibrationFactorFileTest extends TestCase {
	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() {
		testData = new OpenDaTestSupport(DFlowFMCalibrationFactorFileTest.class, "public", "model_dflowfm_blackbox");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testReadAndWriteTargetFile() {
		DFlowFMCalibrationFactorFile calibrationFactorFile = new DFlowFMCalibrationFactorFile();
		calibrationFactorFile.initialize(testRunDataDir, new String[]{"CalibrationFactor/calib-factors.cld", "CalibrationFactor/calib-factors-changed.cld"});
		String[] exchangeItemIDs = calibrationFactorFile.getExchangeItemIDs();
		double[] multiplicationFactors = {1.5};
		for (String exchangeItemID : exchangeItemIDs) {
			IExchangeItem item = calibrationFactorFile.getDataObjectExchangeItem(exchangeItemID);
			double[] doubles = item.getValuesAsDoubles();
			assert doubles != null;
			assert doubles.length == 1;
			assert doubles[0] == 1.0;
			item.multiplyValues(multiplicationFactors);
			assert doubles[0] == 1.5;
		}
		calibrationFactorFile.finish();
		File fileChanged = new File(testRunDataDir, "CalibrationFactor/calib-factors-changed.cld");
		File fileExpected = new File(testRunDataDir, "CalibrationFactor/calib-factors-expected.cld");
		assertEquals(AsciiFileUtils.readText(fileExpected), AsciiFileUtils.readText(fileChanged));
	}

	public void testReadAndWriteOverwriteSource() {
		DFlowFMCalibrationFactorFile calibrationFactorFile = new DFlowFMCalibrationFactorFile();
		calibrationFactorFile.initialize(testRunDataDir, new String[]{"CalibrationFactor/calib-factors.cld"});
		String[] exchangeItemIDs = calibrationFactorFile.getExchangeItemIDs();
		double[] multiplicationFactors = {1.5};
		for (String exchangeItemID : exchangeItemIDs) {
			IExchangeItem item = calibrationFactorFile.getDataObjectExchangeItem(exchangeItemID);
			double[] doubles = item.getValuesAsDoubles();
			assert doubles != null;
			assert doubles.length == 1;
			assert doubles[0] == 1.0;
			item.multiplyValues(multiplicationFactors);
			assert doubles[0] == 1.5;
		}
		calibrationFactorFile.finish();
		File fileChanged = new File(testRunDataDir, "CalibrationFactor/calib-factors.cld");
		File fileExpected = new File(testRunDataDir, "CalibrationFactor/calib-factors-expected.cld");
		assertEquals(AsciiFileUtils.readText(fileExpected), AsciiFileUtils.readText(fileChanged));
	}

}
