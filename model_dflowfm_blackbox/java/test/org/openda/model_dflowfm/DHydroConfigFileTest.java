package org.openda.model_dflowfm;

import junit.framework.TestCase;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;

public class DHydroConfigFileTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() {
		testData = new OpenDaTestSupport(DHydroConfigFileTest.class, "public", "model_dflowfm_blackbox");
		testRunDataDir = new File(testData.getTestRunDataDir(), "DHydroFile");
	}

	public void testReadAndWrite() {
		final String inputFileName = "d_hydro_config.xml";
		final String resultFileName = "d_hydro_config_out.xml";
		final String expectedResultFileName = "d_hydro_config_expected.xml";

		IDataObject dHydroConfigFile = new DHydroConfigFile();
		dHydroConfigFile.initialize(testRunDataDir, new String[]{inputFileName, resultFileName});
		String[] exchangeItemIDs = dHydroConfigFile.getExchangeItemIDs();
		assertEquals("#exchange items", 2, exchangeItemIDs.length);
		assertEquals("first exchange item id", "startTime", exchangeItemIDs[0]);
		assertEquals("second exchange item id", "endTime", exchangeItemIDs[1]);
		IExchangeItem startTimeEI = dHydroConfigFile.getDataObjectExchangeItem("startTime");
		IExchangeItem endTimeEI = dHydroConfigFile.getDataObjectExchangeItem("endTime");
		assertEquals("startTime value", 0.0, startTimeEI.getValuesAsDoubles()[0]);
		assertEquals("endTime value", 86400.0, endTimeEI.getValuesAsDoubles()[0]);

		startTimeEI.axpyOnValues(1, new double[] { 555 });
		startTimeEI.axpyOnValues(1, new double[] { 377 });

		dHydroConfigFile.finish();

		File resultFile = new File(testRunDataDir, resultFileName);
		File expectedResultFile = new File(testRunDataDir, expectedResultFileName);
		assertTrue("expected results", testData.FilesAreIdentical(resultFile, expectedResultFile));
	}
}