package org.openda.model_zero_mq;

import junit.framework.TestCase;

import java.io.File;

public class ZeroMqRealModelTest extends TestCase {
	private static final String TEST_RESOURCES_DIRECTORY = "java/testResources";

	public void testGivenRealModelInstanceAndGoodConfigWhenInitialisedThenOkStatus() {
		ZeroMqModelFactory zeroMqModelFactory = new ZeroMqModelFactory();
		zeroMqModelFactory.initialize(new File(TEST_RESOURCES_DIRECTORY), new String[]{"zeroMqRealModelTest.xml"});
		ZeroMqModelInstance zeroMqModelInstance = (ZeroMqModelInstance) zeroMqModelFactory.getInstance(null, null);

		assertTrue("Initialised instance returned ok", zeroMqModelInstance.initializeModel(new File("D:/OpenDA/wflow_model/sbm_config.toml")));
		assertEquals("Get start time", 9.467712E8D, zeroMqModelInstance.getStartTime());
		assertEquals("Get end time", 9.493632E8D, zeroMqModelInstance.getEndTime());
		assertTrue("Update to mid point", zeroMqModelInstance.updateUntil(9.493632E8D - 9.467712E8D));
		assertTrue("Finalizing model", zeroMqModelInstance.finalizeModel());

		zeroMqModelInstance.finish();
	}
}
