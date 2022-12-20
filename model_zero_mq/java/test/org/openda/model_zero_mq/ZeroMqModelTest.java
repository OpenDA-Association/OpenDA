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

public class ZeroMqModelTest extends TestCase {
	private static final String TEST_RESOURCES_DIRECTORY = "java/testResources";
	private final ObjectMapper objectMapper = new ObjectMapper();
	private ZContext context;

	@Override
	protected void setUp() {
		Executors.newSingleThreadExecutor().submit(new Runnable() {
			@Override
			public void run() {
				try {
					context = new ZContext();
					ZMQ.Socket socket = context.createSocket(SocketType.REP);
					socket.bind("tcp://localhost:55555");

					boolean isFinalized = false;

					while (!Thread.currentThread().isInterrupted()) {
						byte[] reply = socket.recv(0);

						JsonNode request = objectMapper.readTree(reply);
						ObjectNode response = objectMapper.createObjectNode();

						switch (request.get("fn").asText()) {
							case "initialize":
								switch (request.get("config_file").asText()) {
									case "good-config-file.toml":
										response.put("status", "OK");
										break;
									case "bad-config-file.toml":
										response.put("status", "ERROR");
										response.put("error", "was initialized with \"bad-config-file.toml\"");
										break;
									default:
										response.put("status", "ERROR");
										response.put("error", "was initialized with unexpected configuration file: " + request.get("config_file").asText());
										break;
								}
								break;
							case "get_start_time":
								response.put("start_time", 123456789.0D);
								break;
							case "get_end_time":
								response.put("end_time", 987654321.0D);
								break;
							case "get_time_step":
								response.put("time_step", 12345.0D);
								break;
							case "get_time_units":
								response.put("time_units", "Days");
								break;
							case "get_current_time":
								response.put("current_time", 567894321.0D);
								break;
							case "update_until":
								if (request.get("time").asLong() == 9753197531L) {
									response.put("status", "OK");
								} else {
									response.put("status", "ERROR");
									response.put("error", "was updated until: " + request.get("time").asText());
								}
								break;
							case "finalize":
								if (isFinalized) {
									response.put("status", "ERROR");
									response.put("error", "model already finalized");
								} else {
									isFinalized = true;
									response.put("status", "OK");
								}
								break;
							case "get_input_var_names":
								response.put("input_var_names", "[\"var1\",\"var2\",\"var3\"]");
								break;
							case "get_output_var_names":
								response.put("output_var_names", "[\"var4\",\"var5\",\"var6\"]");
								break;
							case "load_state":
								switch (request.get("path").asText()) {
									case "..\\..\\good-path":
										response.put("status", "OK");
										break;
									case "..\\..\\bad-path":
										response.put("status", "ERROR");
										response.put("error", "was loaded with \"..\\..\\bad-path\"");
										break;
									default:
										response.put("status", "ERROR");
										response.put("error", "was loaded with unexpected path: " + request.get("path").asText());
										break;
								}
								break;
							case "save_state":
								switch (request.get("path").asText()) {
									case "..\\..\\good-path":
										response.put("status", "OK");
										break;
									case "..\\..\\bad-path":
										response.put("status", "ERROR");
										response.put("error", "was saved with \"..\\..\\bad-path\"");
										break;
									default:
										response.put("status", "ERROR");
										response.put("error", "was saved with unexpected path: " + request.get("path").asText());
										break;
								}
								break;
							case "get_var_itemsize":
								switch (request.get("name").asText()) {
									case "abc":
										response.put("var_itemsize", 27L);
										break;
									case "xyz":
										response.put("var_itemsize", 42L);
										break;
									default:
										response.put("var_itemsize", -1L);
										break;
								}
								break;
							case "get_var_type":
								switch (request.get("name").asText()) {
									case "abc":
										response.put("var_type", "<start-letters>");
										break;
									case "xyz":
										response.put("var_type", "<end-letters>");
										break;
									default:
										response.put("var_type", "default");
										break;
								}
								break;
							case "get_value":
								switch (request.get("name").asText()) {
									case "alpha-beta":
										response.put("value", 1369248.09876D);
										break;
									case "gamma-delta":
										response.put("value", 8429631.09876D);
										break;
									default:
										response.put("value", Double.NaN);
										break;
								}
								break;
							case "set_value":
								switch (request.get("name").asText()) {
									case "good-set":
										if ("[1.0,2.3,4.5]".equals(request.get("src").asText())) {
											response.put("status", "OK");
										} else {
											response.put("status", "ERROR");
											response.put("error", "Set value good version but with bad slice");
										}
										break;
									case "bad-set":
										response.put("status", "ERROR");
										response.put("error", "Set value bad version");
										break;
									default:
										response.put("status", "ERROR");
										response.put("error", "Set value with no id");
										break;
								}
								break;
							default:
								response.put("status", "ERROR");
								response.put("error", "reached default");
								break;
						}

						socket.send(objectMapper.writeValueAsString(response).getBytes(ZMQ.CHARSET), 0);
					}
				} catch (IOException e) {
					throw new RuntimeException(e);
				}
			}
		});
	}

	@Override
	protected void tearDown() {
		context.close();
	}

	public void testGivenModelInstanceAndGoodConfigWhenInitialisedThenOkStatus() {
		ZeroMqModelFactory zeroMqModelFactory = new ZeroMqModelFactory();
		zeroMqModelFactory.initialize(new File(TEST_RESOURCES_DIRECTORY), new String[]{"zeroMqModelTest.xml"});
		ZeroMqModelInstance zeroMqModelInstance = (ZeroMqModelInstance) zeroMqModelFactory.getInstance(null, null);

		assertTrue("Initialised instance returned ok", zeroMqModelInstance.initializeModel(new File("good-config-file.toml")));
	}

	public void testGivenModelInstanceAndBadConfigWhenInitialisedThenErrorStatus() {
		ZeroMqModelFactory zeroMqModelFactory = new ZeroMqModelFactory();
		zeroMqModelFactory.initialize(new File(TEST_RESOURCES_DIRECTORY), new String[]{"zeroMqModelTest.xml"});
		ZeroMqModelInstance zeroMqModelInstance = (ZeroMqModelInstance) zeroMqModelFactory.getInstance(null, null);

		String exceptionMessage = "";

		try {
			zeroMqModelInstance.initializeModel(new File("bad-config-file.toml"));
		} catch (RuntimeException runtimeException) {
			exceptionMessage = runtimeException.getMessage();
		}

		assertEquals("Initialised instance returned error", "Initialisation failure: was initialized with \"bad-config-file.toml\"", exceptionMessage);
	}

	public void testGivenModelInstanceWhenRequestsMadeThenCorrectValuesReturned() {
		ZeroMqModelFactory zeroMqModelFactory = new ZeroMqModelFactory();
		zeroMqModelFactory.initialize(new File(TEST_RESOURCES_DIRECTORY), new String[]{"zeroMqModelTest.xml"});
		ZeroMqModelInstance zeroMqModelInstance = (ZeroMqModelInstance) zeroMqModelFactory.getInstance(null, null);

		zeroMqModelInstance.initializeModel(new File("good-config-file.toml"));

		assertEquals("Start time", 123456789.0D, zeroMqModelInstance.getStartTime());
		assertEquals("End time", 987654321.0D, zeroMqModelInstance.getEndTime());
		assertEquals("Time step", 12345D, zeroMqModelInstance.getTimeStep());
		assertEquals("Time units", "Days", zeroMqModelInstance.getTimeUnits());
		assertEquals("Current time", 567894321.0D, zeroMqModelInstance.getCurrentTimeInstant()); // Method name clashed

		List<String> inputVarNames = zeroMqModelInstance.getInputVarNames();

		assertEquals("Input var names size", 3, inputVarNames.size());
		assertTrue("Contains var name 1", inputVarNames.contains("var1"));
		assertTrue("Contains var name 2", inputVarNames.contains("var2"));
		assertTrue("Contains var name 3", inputVarNames.contains("var3"));

		List<String> outputVarNames = zeroMqModelInstance.getOutputVarNames();

		assertEquals("Output var names size", 3, outputVarNames.size());
		assertTrue("Contains var name 4", outputVarNames.contains("var4"));
		assertTrue("Contains var name 5", outputVarNames.contains("var5"));
		assertTrue("Contains var name 6", outputVarNames.contains("var6"));

		assertEquals("Get item size \"abc\"", 27L, zeroMqModelInstance.getVarItemSize("abc"));
		assertEquals("Get item size \"xyz\"", 42L, zeroMqModelInstance.getVarItemSize("xyz"));

		assertEquals("Get item type \"abc\"", "<start-letters>", zeroMqModelInstance.getVarType("abc"));
		assertEquals("Get item type \"xyz\"", "<end-letters>", zeroMqModelInstance.getVarType("xyz"));

		assertEquals("Get value", 1369248.09876D, zeroMqModelInstance.getValue("alpha-beta"));
		assertEquals("Get value", 8429631.09876D, zeroMqModelInstance.getValue("gamma-delta"));
	}

	public void testGivenModelInstanceAndGoodTimeWhenUpdatedUntilThenOkStatus() {
		ZeroMqModelFactory zeroMqModelFactory = new ZeroMqModelFactory();
		zeroMqModelFactory.initialize(new File(TEST_RESOURCES_DIRECTORY), new String[]{"zeroMqModelTest.xml"});
		ZeroMqModelInstance zeroMqModelInstance = (ZeroMqModelInstance) zeroMqModelFactory.getInstance(null, null);

		assertTrue("Update until time returned ok", zeroMqModelInstance.updateUntil(9753197531D));
	}

	public void testGivenModelInstanceAndBadTimeWhenUpdatedUntilThenErrorStatus() {
		ZeroMqModelFactory zeroMqModelFactory = new ZeroMqModelFactory();
		zeroMqModelFactory.initialize(new File(TEST_RESOURCES_DIRECTORY), new String[]{"zeroMqModelTest.xml"});
		ZeroMqModelInstance zeroMqModelInstance = (ZeroMqModelInstance) zeroMqModelFactory.getInstance(null, null);

		String exceptionMessage = "";

		try {
			zeroMqModelInstance.updateUntil(864286428642L);
		} catch (RuntimeException runtimeException) {
			exceptionMessage = runtimeException.getMessage();
		}

		assertEquals("Update until time  returned error", "Update until: was updated until: 8.64286428642E11", exceptionMessage);
	}

	public void testGivenModelInstanceWhenModelFinalizedThenOnlyFinalizedOnce() {
		ZeroMqModelFactory zeroMqModelFactory = new ZeroMqModelFactory();
		zeroMqModelFactory.initialize(new File(TEST_RESOURCES_DIRECTORY), new String[]{"zeroMqModelTest.xml"});
		ZeroMqModelInstance zeroMqModelInstance = (ZeroMqModelInstance) zeroMqModelFactory.getInstance(null, null);

		assertTrue("First model finalization", zeroMqModelInstance.finalizeModel());

		String exceptionMessage = "";

		try {
			zeroMqModelInstance.finalizeModel();
		} catch (RuntimeException runtimeException) {
			exceptionMessage = runtimeException.getMessage();
		}

		assertEquals("Update until time  returned error", "Finalize model: model already finalized", exceptionMessage);
	}

	public void testGivenModelInstanceAndGoodPathWhenSavedAndLoadedThenOkStatus() {
		ZeroMqModelFactory zeroMqModelFactory = new ZeroMqModelFactory();
		zeroMqModelFactory.initialize(new File(TEST_RESOURCES_DIRECTORY), new String[]{"zeroMqModelTest.xml"});
		ZeroMqModelInstance zeroMqModelInstance = (ZeroMqModelInstance) zeroMqModelFactory.getInstance(null, null);

		assertTrue("Save state returned ok", zeroMqModelInstance.saveState(new File("../../good-path")));
		assertTrue("Load state returned ok", zeroMqModelInstance.loadState(new File("../../good-path")));
	}

	public void testGivenModelInstanceAndGoodPathWhenSavedAndLoadedThenErrorStatus() {
		ZeroMqModelFactory zeroMqModelFactory = new ZeroMqModelFactory();
		zeroMqModelFactory.initialize(new File(TEST_RESOURCES_DIRECTORY), new String[]{"zeroMqModelTest.xml"});
		ZeroMqModelInstance zeroMqModelInstance = (ZeroMqModelInstance) zeroMqModelFactory.getInstance(null, null);

		String exceptionMessage = "";

		try {
			zeroMqModelInstance.saveState(new File("../../bad-path"));
		} catch (RuntimeException runtimeException) {
			exceptionMessage = runtimeException.getMessage();
		}

		assertEquals("Update until time returned error", "Save state: was saved with \"..\\..\\bad-path\"", exceptionMessage);

		try {
			zeroMqModelInstance.loadState(new File("../../bad-path"));
		} catch (RuntimeException runtimeException) {
			exceptionMessage = runtimeException.getMessage();
		}

		assertEquals("Update until time  returned error", "Load state: was loaded with \"..\\..\\bad-path\"", exceptionMessage);
	}

	public void testGivenModelInstanceWhenSettingValuesThenOkAndErrorStatus() {
		ZeroMqModelFactory zeroMqModelFactory = new ZeroMqModelFactory();
		zeroMqModelFactory.initialize(new File(TEST_RESOURCES_DIRECTORY), new String[]{"zeroMqModelTest.xml"});
		ZeroMqModelInstance zeroMqModelInstance = (ZeroMqModelInstance) zeroMqModelFactory.getInstance(null, null);

		assertTrue("Set value returned ok", zeroMqModelInstance.setValue("good-set", "[1.0,2.3,4.5]"));

		String exceptionMessage = "";

		try {
			zeroMqModelInstance.setValue("good-set", "[4.5,2.3,1.0]");
		} catch (RuntimeException runtimeException) {
			exceptionMessage = runtimeException.getMessage();
		}

		assertEquals("Set value returned error", "Set value: Set value good version but with bad slice", exceptionMessage);

		try {
			zeroMqModelInstance.setValue("bad-set", "[1.0,2.3,4.5]");
		} catch (RuntimeException runtimeException) {
			exceptionMessage = runtimeException.getMessage();
		}

		assertEquals("Set value returned error", "Set value: Set value bad version", exceptionMessage);
	}
}
