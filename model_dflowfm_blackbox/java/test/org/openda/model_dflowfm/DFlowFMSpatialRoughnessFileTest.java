package org.openda.model_dflowfm;

import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.io.AsciiFileUtils;

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
		assertEquals(10, exchangeItemIDs.length);

		String[] expectedIds = {"Main-model_wide-Chezy",
			"Main-Manning-Channel_1D_1_A-x0-q0",
			"Main-Manning-Channel_1D_1_A-x0-q1",
			"Main-Manning-Channel_1D_1_A-x0-q2",
			"Main-Manning-Channel_1D_1_A-x500-q0",
			"Main-Manning-Channel_1D_1_A-x500-q1",
			"Main-Manning-Channel_1D_1_A-x500-q2",
			"Main-Manning-Channel_1D_1_B-x0",
			"Main-Manning-Channel_1D_1_B-x200",
			"Main-Manning-Channel_1D_1-x0"};

		for (int i = 0; i < expectedIds.length; i++) {
			assertEquals(expectedIds[i], exchangeItemIDs[i]);
		}
		IExchangeItem chezy = checkEI(dFlowFMSpatialRoughnessFile, "Main-model_wide-Chezy", new double[]{45.0});
		chezy.setValuesAsDoubles(new double[]{50.0});
		IExchangeItem b0 = checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-Channel_1D_1_B-x0", new double[]{0.03});
		b0.setValuesAsDoubles(new double[]{0.033});
		IExchangeItem b200 = checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-Channel_1D_1_B-x200", new double[]{0.032});
		b200.setValuesAsDoubles(new double[]{0.035});
		IExchangeItem x0 = checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-Channel_1D_1-x0", new double[]{0.028});
		x0.setValuesAsDoubles(new double[]{0.031});
		IExchangeItem x0q0 = checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-Channel_1D_1_A-x0-q0", new double[]{0.03});
		x0q0.setValuesAsDoubles(new double[]{0.0314});
		IExchangeItem x0q1 = checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-Channel_1D_1_A-x0-q1", new double[]{0.03});
		x0q1.setValuesAsDoubles(new double[]{0.0345});
		IExchangeItem x0q2 = checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-Channel_1D_1_A-x0-q2", new double[]{0.025});
		x0q2.setValuesAsDoubles(new double[]{0.0579});
		IExchangeItem x500q0 = checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-Channel_1D_1_A-x500-q0", new double[]{0.029, 0.029});
		x500q0.setValuesAsDoubles(new double[]{0.02978, 0.02963});
		IExchangeItem x500q1 = checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-Channel_1D_1_A-x500-q1", new double[]{0.025, 0.026});
		x500q1.setValuesAsDoubles(new double[]{0.02576, 0.02634});
		IExchangeItem x500q2 = checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-Channel_1D_1_A-x500-q2", new double[]{0.025, 0.023});
		x500q2.setValuesAsDoubles(new double[]{0.02513, 0.02346});

		dFlowFMSpatialRoughnessFile.finish();

		File resultFile = new File(testRunDataDir, "roughness-Main.ini");
		assertTrue(resultFile.exists());
		assertEquals(AsciiFileUtils.readText(new File(testRunDataDir, "expected_roughness-Main.ini")), AsciiFileUtils.readText(resultFile));
	}

	private IExchangeItem checkEI(DFlowFMSpatialRoughnessFile dFlowFMSpatialRoughnessFile, String exchangeItemID, double[] expectedValues) {
		IExchangeItem exchangeItem = dFlowFMSpatialRoughnessFile.getDataObjectExchangeItem(exchangeItemID);
		double[] modelWideValues = exchangeItem.getValuesAsDoubles();
		assertEquals(expectedValues.length, modelWideValues.length);
		for (int i = 0; i < expectedValues.length; i++) {
			assertEquals(expectedValues[i], modelWideValues[i], 1e-6);
		}
		return exchangeItem;
	}

	public void testReadAndWriteOlof() {
		DFlowFMSpatialRoughnessFile dFlowFMSpatialRoughnessFile = new DFlowFMSpatialRoughnessFile();
		dFlowFMSpatialRoughnessFile.initialize(testRunDataDir, new String[]{"roughness-Main-Olof.ini", "observationFile=ObservationPoints-Olof.ini"});
		String[] exchangeItemIDs = dFlowFMSpatialRoughnessFile.getExchangeItemIDs();
		assertEquals(10, exchangeItemIDs.length);
	}
}
