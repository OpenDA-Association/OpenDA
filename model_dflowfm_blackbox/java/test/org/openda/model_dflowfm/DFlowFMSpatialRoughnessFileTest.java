package org.openda.model_dflowfm;

import junit.framework.TestCase;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;

public class DFlowFMSpatialRoughnessFileTest extends TestCase {
	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() {
		testData = new OpenDaTestSupport(DFlowFMSpatialRoughnessFileTest.class, "model_dflowfm_blackbox");
		testRunDataDir = new File(testData.getTestRunDataDir(), "SpatialRoughness");
	}

	public void testReadAndWrite() {
		DFlowFMSpatialRoughnessFile dFlowFMSpatialRoughnessFile = new DFlowFMSpatialRoughnessFile();
		dFlowFMSpatialRoughnessFile.initialize(testRunDataDir, new String[]{"roughness-Main.ini", "observationFile=obsFile1D_obs.ini"});
		String[] exchangeItemIDs = dFlowFMSpatialRoughnessFile.getExchangeItemIDs();
		assertEquals(4, exchangeItemIDs.length);
	}
}
