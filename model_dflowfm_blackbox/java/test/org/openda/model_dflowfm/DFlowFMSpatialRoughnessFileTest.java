package org.openda.model_dflowfm;

import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
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
		assertEquals(6, exchangeItemIDs.length);

		String[] expectedIds = {"Main-model_wide-Chezy", "Main-Manning-Channel_1D_1_A-x0-q0", "Main-Manning-Channel_1D_1_A-x0-q1", "Main-Manning-Channel_1D_1_A-x0-q2", "Main-Manning-Channel_1D_1_B-x0", "Main-Manning-Channel_1D_1-x0"};

		for (int i = 0; i < 6; i++) {
			assertEquals(expectedIds[i], exchangeItemIDs[i]);
		}
		checkEI(dFlowFMSpatialRoughnessFile, "Main-model_wide-Chezy", new double[]{45.0});
		checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-Channel_1D_1_B-x0", new double[]{0.03, 0.032});
		checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-Channel_1D_1-x0", new double[]{0.028});
		checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-Channel_1D_1_A-x0-q0", new double[]{0.03, 0.029, 0.029});
		checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-Channel_1D_1_A-x0-q1", new double[]{0.03, 0.025, 0.026});
		checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-Channel_1D_1_A-x0-q2", new double[]{0.025, 0.025, 0.023});

		dFlowFMSpatialRoughnessFile.finish();

		File file = new File(testRunDataDir, "roughness-Main.ini");
		assertTrue(file.exists());
	}

	private void checkEI(DFlowFMSpatialRoughnessFile dFlowFMSpatialRoughnessFile, String exchangeItemID, double[] expectedValues) {
		IExchangeItem modelWideEI = dFlowFMSpatialRoughnessFile.getDataObjectExchangeItem(exchangeItemID);
		double[] modelWideValues = modelWideEI.getValuesAsDoubles();
		assertEquals(expectedValues.length, modelWideValues.length);
		for (int i = 0; i < expectedValues.length; i++) {
			assertEquals(expectedValues[i], modelWideValues[i], 1e-6);
		}
	}
}
