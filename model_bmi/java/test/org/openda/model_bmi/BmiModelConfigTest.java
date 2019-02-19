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

	public void testNoStateIds() {
		File configFile = new File(testRunDataDir, "BmiModelConfigTest/BmiModelFactoryConfigStateIds.xml");
		BmiModelFactoryConfigReader reader = new BmiModelFactoryConfigReader(configFile);
		List<BmiModelFactory.BmiModelStateExchangeItemsInfo> modelStateExchangeItemInfos = reader.getModelStateExchangeItemInfos();
		assertNotNull(modelStateExchangeItemInfos);
		assertEquals(2, modelStateExchangeItemInfos.size());
		BmiModelFactory.BmiModelStateExchangeItemsInfo info0 = modelStateExchangeItemInfos.get(0);
		assertEquals("state1", info0.getStateId());
	}

	public void testStateIds() {

	}
}
