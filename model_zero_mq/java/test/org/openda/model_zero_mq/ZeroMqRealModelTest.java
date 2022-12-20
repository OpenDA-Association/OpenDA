package org.openda.model_zero_mq;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import junit.framework.TestCase;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.concurrent.Executors;

public class ZeroMqRealModelTest extends TestCase {
	private static final String TEST_RESOURCES_DIRECTORY = "java/testResources";
	private final ObjectMapper objectMapper = new ObjectMapper();
	private ZContext context;

	@Override
	protected void setUp() {
		Executors.newSingleThreadExecutor().submit(new Runnable() {
			@Override
			public void run() {

			}
		});
	}

	@Override
	protected void tearDown() {

	}

	public void testGivenRealModelInstanceAndGoodConfigWhenInitialisedThenOkStatus() {
		ZeroMqModelFactory zeroMqModelFactory = new ZeroMqModelFactory();
		zeroMqModelFactory.initialize(new File(TEST_RESOURCES_DIRECTORY), new String[]{"zeroMqRealModelTest.xml"});
		ZeroMqModelInstance zeroMqModelInstance = (ZeroMqModelInstance) zeroMqModelFactory.getInstance(null, null);

		assertTrue("Initialised instance returned ok", zeroMqModelInstance.initializeModel(new File("D:/OpenDA/wflow_model/sbm_config.toml")));
		assertEquals("Get start time", 9.467712E8D, zeroMqModelInstance.getStartTime());
		assertEquals("Get end time", 9.493632E8D, zeroMqModelInstance.getEndTime());
		assertTrue("Update to mid point", zeroMqModelInstance.updateUntil(9.493632E8D - 9.467712E8D));
		assertTrue("Finalizing model", zeroMqModelInstance.finalizeModel());
	}
}
