package org.openda.model_hec_hms;

import junit.framework.TestCase;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;

public class DssFileTest extends TestCase {
	OpenDaTestSupport testData = null;
	private File testRunDataRestartFileDir;


	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(DssFileTest.class, "model_hec_hms");
		testRunDataRestartFileDir = testData.getTestRunDataDir();
	}

	public void testRead() {
		File dssFile = new File(testRunDataRestartFileDir, "Hec.dss");
		DssFile dssDataObject = new DssFile();
		dssDataObject.initialize(testRunDataRestartFileDir, new String[]{dssFile.getName()});
		String[] exchangeItemIDs = dssDataObject.getExchangeItemIDs();
		assertEquals(1, exchangeItemIDs.length);
	}

	public void testReadReal() {
		File dssFile = new File(testRunDataRestartFileDir, "Run_1.dss");
		DssFile dssDataObject = new DssFile();
		dssDataObject.initialize(testRunDataRestartFileDir, new String[]{dssFile.getName()});
		String[] exchangeItemIDs = dssDataObject.getExchangeItemIDs();
		Arrays.sort(exchangeItemIDs);
		for (String exchangeItemID : exchangeItemIDs) {
			dssDataObject.getDataObjectExchangeItem(exchangeItemID).getValuesAsDoubles();
		}
		assertEquals(250, exchangeItemIDs.length);
	}
}
