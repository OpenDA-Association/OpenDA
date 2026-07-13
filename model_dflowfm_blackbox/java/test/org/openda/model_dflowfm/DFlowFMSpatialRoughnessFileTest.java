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
		assertEquals(21, exchangeItemIDs.length);

		String[] expectedIds = {"Main-model_wide-Chezy",
			"Main-Manning-Constant_1_chainage-x0",
			"Main-Manning-Constant_2_chainage-x0",
			"Main-Manning-Constant_2_chainage-x200",
			"Main-Manning-ChainageAndDischargeDependent-x0-q0",
			"Main-Manning-ChainageAndDischargeDependent-x0-q1",
			"Main-Manning-ChainageAndDischargeDependent-x0-q2",
			"Main-Manning-ChainageAndDischargeDependent-x0-q3",
			"Main-Manning-ChainageAndDischargeDependent-x0-q4",
			"Main-Manning-ChainageAndDischargeDependent-x500-q0",
			"Main-Manning-ChainageAndDischargeDependent-x500-q1",
			"Main-Manning-ChainageAndDischargeDependent-x500-q2",
			"Main-Manning-ChainageAndDischargeDependent-x500-q3",
			"Main-Manning-ChainageAndDischargeDependent-x500-q4",
			"Main-Manning-ChainageAndWaterlevelDependent-x0-h0",
			"Main-Manning-ChainageAndWaterlevelDependent-x0-h1",
			"Main-Manning-ChainageAndWaterlevelDependent-x0-h2",
			"Main-Manning-ChainageAndWaterlevelDependent-x0-h3",
			"Main-Manning-ChainageAndWaterlevelDependent-x0-h4",
			"Main-Manning-ChainageAndWaterlevelDependent-x0-h5",
			"Main-Manning-ChainageAndWaterlevelDependent-x0-h6"
		};

		for (int i = 0; i < expectedIds.length; i++) {
			assertEquals(expectedIds[i], exchangeItemIDs[i]);
		}
		IExchangeItem chezy = checkEI(dFlowFMSpatialRoughnessFile, "Main-model_wide-Chezy", new double[]{45.0});
		chezy.setValuesAsDoubles(new double[]{50.0});
		IExchangeItem b0 = checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-Constant_2_chainage-x0", new double[]{0.03});
		b0.setValuesAsDoubles(new double[]{0.033});
		IExchangeItem b200 = checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-Constant_2_chainage-x200", new double[]{0.032});
		b200.setValuesAsDoubles(new double[]{0.035});
		IExchangeItem x0 = checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-Constant_1_chainage-x0", new double[]{0.028});
		x0.setValuesAsDoubles(new double[]{0.031});
		IExchangeItem x0q0 = checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-ChainageAndDischargeDependent-x0-q0", new double[]{0.11});
		x0q0.setValuesAsDoubles(new double[]{0.111});
		IExchangeItem x0q1 = checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-ChainageAndDischargeDependent-x0-q1", new double[]{0.12});
		x0q1.setValuesAsDoubles(new double[]{0.122});
		IExchangeItem x0q2 = checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-ChainageAndDischargeDependent-x0-q2", new double[]{0.13});
		x0q2.setValuesAsDoubles(new double[]{0.133});
		IExchangeItem x0q3 = checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-ChainageAndDischargeDependent-x0-q3", new double[]{0.14});
		x0q3.setValuesAsDoubles(new double[]{0.144});
		IExchangeItem x0q4 = checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-ChainageAndDischargeDependent-x0-q4", new double[]{0.15});
		x0q4.setValuesAsDoubles(new double[]{0.155});
		IExchangeItem x500q0 = checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-ChainageAndDischargeDependent-x500-q0", new double[]{1.11, 2.11});
		x500q0.setValuesAsDoubles(new double[]{1.111, 2.111});
		IExchangeItem x500q1 = checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-ChainageAndDischargeDependent-x500-q1", new double[]{1.12, 2.12});
		x500q1.setValuesAsDoubles(new double[]{1.122, 2.122});
		IExchangeItem x500q2 = checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-ChainageAndDischargeDependent-x500-q2", new double[]{1.13, 2.13});
		x500q2.setValuesAsDoubles(new double[]{1.133, 2.133});
		IExchangeItem x500q3 = checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-ChainageAndDischargeDependent-x500-q3", new double[]{1.14, 2.14});
		x500q3.setValuesAsDoubles(new double[]{1.144, 2.144});
		IExchangeItem x500q4 = checkEI(dFlowFMSpatialRoughnessFile, "Main-Manning-ChainageAndDischargeDependent-x500-q4", new double[]{1.15, 2.15});
		x500q4.setValuesAsDoubles(new double[]{1.155, 2.155});

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

/*	public void testReadAndWriteOlof() {
		DFlowFMSpatialRoughnessFile dFlowFMSpatialRoughnessFile = new DFlowFMSpatialRoughnessFile();
		dFlowFMSpatialRoughnessFile.initialize(testRunDataDir, new String[]{"roughness-Main_Olof.ini", "observationFile=ObservationPoints_Olof.ini"});
		String[] exchangeItemIDs = dFlowFMSpatialRoughnessFile.getExchangeItemIDs();
		assertEquals(10, exchangeItemIDs.length);
	}*/
}
