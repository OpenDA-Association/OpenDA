package org.openda.model_bmi;

import junit.framework.TestCase;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.util.List;

public class BmiModelConfigTest extends TestCase {
	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() {
		testData = new OpenDaTestSupport(BmiModelConfigTest.class, "model_bmi");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testStateIds() {
		File configFile = new File(testRunDataDir, "BmiModelConfigTest/BmiModelFactoryConfigStateIds.xml");
		BmiModelFactoryConfigReader reader = new BmiModelFactoryConfigReader(configFile);
		List<BmiModelFactory.BmiModelStateExchangeItemsInfo> modelStateExchangeItemInfos = reader.getModelStateExchangeItemInfos();
		assertNotNull(modelStateExchangeItemInfos);
		assertEquals(2, modelStateExchangeItemInfos.size());
		BmiModelFactory.BmiModelStateExchangeItemsInfo info0 = modelStateExchangeItemInfos.get(0);
		assertEquals("state1", info0.getStateId());
		String[] ids0 = info0.getModelStateExchangeItemIds();
		Double[] lowerLimits0 = info0.getModelStateExchangeItemLowerLimits();
		Double[] upperLimits0 = info0.getModelStateExchangeItemUpperLimits();
		String[] expectedIds0 = new String[]{"DrySnow", "FreeWater", "SoilMoisture"};
		double[] expectedLowerLimits0 = new double[3];
		double[] expectedUpperLimits0 = {1000, 100, 300};
		for (int i = 0; i < expectedIds0.length; i++) {
			assertEquals(expectedIds0[i], ids0[i]);
			assertEquals(expectedLowerLimits0[i], lowerLimits0[i]);
			assertEquals(expectedUpperLimits0[i], upperLimits0[i]);
		}
		BmiModelFactory.BmiModelStateExchangeItemsInfo info1 = modelStateExchangeItemInfos.get(1);
		assertEquals("state2", info1.getStateId());
		String[] ids1 = info1.getModelStateExchangeItemIds();
		Double[] lowerLimits1 = info1.getModelStateExchangeItemLowerLimits();
		Double[] upperLimits1 = info1.getModelStateExchangeItemUpperLimits();
		String[] expectedIds1 = new String[]{"LowerZoneStorage", "UpperZoneStorage", "InterceptionStorage", "SurfaceRunoff", "WaterLevel"};
		double[] expectedLowerLimits1 = new double[5];
		double[] expectedUpperLimits1 = {1000, 1000, 5, 1000, 1000};
		for (int i = 0; i < expectedIds1.length; i++) {
			assertEquals(expectedIds1[i], ids1[i]);
			assertEquals(expectedLowerLimits1[i], lowerLimits1[i]);
			assertEquals(expectedUpperLimits1[i], upperLimits1[i]);
		}

	}

	public void testNoStateIds() {
		File configFile = new File(testRunDataDir, "BmiModelConfigTest/BmiModelFactoryConfigNoStateIds.xml");
		BmiModelFactoryConfigReader reader = new BmiModelFactoryConfigReader(configFile);
		List<BmiModelFactory.BmiModelStateExchangeItemsInfo> modelStateExchangeItemInfos = reader.getModelStateExchangeItemInfos();
		assertNotNull(modelStateExchangeItemInfos);
		assertEquals(1, modelStateExchangeItemInfos.size());
		BmiModelFactory.BmiModelStateExchangeItemsInfo info0 = modelStateExchangeItemInfos.get(0);
		assertEquals("state", info0.getStateId());
		String[] modelStateExchangeItemIds = info0.getModelStateExchangeItemIds();
		Double[] modelStateExchangeItemLowerLimits = info0.getModelStateExchangeItemLowerLimits();
		Double[] modelStateExchangeItemUpperLimits = info0.getModelStateExchangeItemUpperLimits();
		String[] expectedIds = new String[]{"DrySnow", "FreeWater", "SoilMoisture", "LowerZoneStorage", "UpperZoneStorage", "InterceptionStorage", "SurfaceRunoff", "WaterLevel"};
		double[] expectedLowerLimits = new double[8];
		double[] expectedUpperLimits = {1000, 100, 300, 1000, 1000, 5, 1000, 1000};
		for (int i = 0; i < expectedIds.length; i++) {
			assertEquals(expectedIds[i], modelStateExchangeItemIds[i]);
			assertEquals(expectedLowerLimits[i], modelStateExchangeItemLowerLimits[i]);
			assertEquals(expectedUpperLimits[i], modelStateExchangeItemUpperLimits[i]);
		}
	}
}
