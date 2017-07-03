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
		String[] expectedIds = {"CalFactor-34", "CalFactor-28-q100.20", "CalFactor-28-q150", "CalFactor-28-q205.30", "CalFactor-61-q650", "CalFactor-61-q960", "CalFactor-61-q1120.75", "CalFactor-61-q1405.11", "CalFactor-17-h10.53", "CalFactor-17-h15.546", "CalFactor-17-h18.30", "CalFactor-3-h8.15", "CalFactor-3-h9", "CalFactor-3-h10.01", "CalFactor-3-h13"};
		for (int i = 0; i < exchangeItemIDs.length; i++) {
			String exchangeItemID = exchangeItemIDs[i];
			assert exchangeItemID.equals(expectedIds[i]);
			IExchangeItem item = calibrationFactorFile.getDataObjectExchangeItem(exchangeItemID);
			double[] doubles = item.getValuesAsDoubles();
			assert doubles != null;
			assert doubles.length == 1;
			assert doubles[0] == 1.0;
			double factor = 1 + 0.1 * i;
			double[] multiplicationFactors = {factor};
			item.multiplyValues(multiplicationFactors);
			assert item.getValuesAsDoubles()[0] == factor;
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
		String[] expectedIds = {"CalFactor-34","CalFactor-28-q100.20","CalFactor-28-q150","CalFactor-28-q205.30","CalFactor-61-q650","CalFactor-61-q960","CalFactor-61-q1120.75","CalFactor-61-q1405.11","CalFactor-17-h10.53", "CalFactor-17-h15.546","CalFactor-17-h18.30","CalFactor-3-h8.15","CalFactor-3-h9","CalFactor-3-h10.01","CalFactor-3-h13"};
		for (int i = 0; i < exchangeItemIDs.length; i++) {
			String exchangeItemID = exchangeItemIDs[i];
			assert exchangeItemID.equals(expectedIds[i]);
			IExchangeItem item = calibrationFactorFile.getDataObjectExchangeItem(exchangeItemID);
			double[] doubles = item.getValuesAsDoubles();
			assert doubles != null;
			assert doubles.length == 1;
			assert doubles[0] == 1.0;
			double factor = 1 + 0.1 * i;
			double[] multiplicationFactors = {factor};
			item.multiplyValues(multiplicationFactors);
			assert item.getValuesAsDoubles()[0] == factor;
		}
		calibrationFactorFile.finish();
		File fileChanged = new File(testRunDataDir, "CalibrationFactor/calib-factors.cld");
		File fileExpected = new File(testRunDataDir, "CalibrationFactor/calib-factors-expected.cld");
		assertEquals(AsciiFileUtils.readText(fileExpected), AsciiFileUtils.readText(fileChanged));
	}

}
