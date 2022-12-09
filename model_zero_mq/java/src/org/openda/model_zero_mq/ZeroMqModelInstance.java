package org.openda.model_zero_mq;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.*;
import org.openda.utils.Instance;
import org.openda.utils.Time;
import org.zeromq.ZMQ;

import java.io.File;
import java.util.Arrays;
import java.util.List;

public class ZeroMqModelInstance extends Instance implements IModelInstance, IModelExtensions, IOutputModeSetter {
	private final ObjectMapper objectMapper = new ObjectMapper();
	private static final String FUNCTION_KEY = "fn";
	private static final String FUNCTION_INITIALISE = "initialize";
	private static final String FUNCTION_GET_START_TIME = "get_start_time";
	private static final String RETURN_START_TIME = "start_time";
	private static final String FUNCTION_GET_END_TIME = "get_end_time";
	private static final String RETURN_END_TIME = "end_time";
	private static final String FUNCTION_GET_TIME_STEP = "get_time_step";
	private static final String RETURN_TIME_STEP = "time_step";
	private static final String FUNCTION_GET_TIME_UNITS = "get_time_units";
	private static final String RETURN_TIME_UNITS = "time_units";
	private static final String FUNCTION_GET_CURRENT_TIME = "get_current_time";
	private static final String RETURN_CURRENT_TIME = "current_time";
	private static final String FUNCTION_UPDATE_UNTIL = "update_until";
	private static final String UPDATE_UNTIL = "time";
	private static final String FUNCTION_FINALIZE_MODEL = "finalize";
	private static final String FUNCTION_GET_INPUT_VAR_NAMES = "get_input_var_names";
	private static final String RETURN_INPUT_VAR_NAMES = "input_var_names";
	private static final String FUNCTION_GET_OUTPUT_VAR_NAMES = "get_output_var_names";
	private static final String RETURN_OUTPUT_VAR_NAMES = "output_var_names";
	private static final String FUNCTION_LOAD_STATE = "load_state";
	private static final String LOAD_STATE_PATH = "path";
	private static final String FUNCTION_SAVE_STATE = "save_state";
	private static final String SAVE_STATE_PATH = "path";
	private static final String FUNCTION_GET_VAR_ITEM_SIZE = "get_var_itemsize";
	private static final String VAR_ITEM_ID = "name";
	private static final String RETURN_ITEM_SIZE = "var_itemsize";
	private static final String FUNCTION_GET_VAR_ITEM_TYPE = "get_var_type";
	private static final String RETURN_ITEM_TYPE = "var_type";
	private static final String FUNCTION_GET_VALUE = "get_value";
	private static final String RETURN_VALUE = "value";
	private static final String FUNCTION_SET_VALUE = "set_value";
	private static final String SLICE = "src";
	private static final String CONFIGURATION_FILE = "config_file";
	private static final String REPLY_STATUS = "status";
	private static final String REPLY_OK = "OK";
	private static final String REPLY_ERROR = "ERROR";
	private static final String REPLY_ERROR_MESSAGE = "error";
	private final ZMQ.Socket socket;

	public ZeroMqModelInstance(ZMQ.Socket socket) {
		// initialize model
		// create exchange items

		//initializeModel(socket, new File("../../test/sbm_config.toml"));
		this.socket = socket;
	}

	public boolean initializeModel(File configFile) {
		ObjectNode request = objectMapper.createObjectNode();
		request.put(FUNCTION_KEY, FUNCTION_INITIALISE);
		request.put(CONFIGURATION_FILE, configFile.getPath());

		JsonNode reply;

		try {
			socket.send(objectMapper.writeValueAsString(request).getBytes(ZMQ.CHARSET), 0);

			reply = objectMapper.readTree(new String(socket.recv(0), ZMQ.CHARSET));

			if (REPLY_ERROR.equals(reply.get(REPLY_STATUS).asText())) {
				throw new RuntimeException("Initialisation failure: " + reply.get(REPLY_ERROR_MESSAGE).asText());
			}
		} catch (JsonProcessingException jsonProcessingException) {
			throw new RuntimeException(jsonProcessingException);
		}

		return REPLY_OK.equals(reply.get(REPLY_STATUS).asText());
	}

	public double getStartTime() {
		return getLongValue(FUNCTION_GET_START_TIME, RETURN_START_TIME);
	}

	public double getEndTime() {
		return getLongValue(FUNCTION_GET_END_TIME, RETURN_END_TIME);
	}

	public long getTimeStep() {
		return getLongValue(FUNCTION_GET_TIME_STEP, RETURN_TIME_STEP);
	}

	public String getTimeUnits() {
		return getReply(FUNCTION_GET_TIME_UNITS, RETURN_TIME_UNITS).asText();
	}

	public long getCurrentTimeInstant() {
		return getLongValue(FUNCTION_GET_CURRENT_TIME, RETURN_CURRENT_TIME);
	}

	public boolean updateUntil(long time) {
		ObjectNode request = objectMapper.createObjectNode();
		request.put(FUNCTION_KEY, FUNCTION_UPDATE_UNTIL);
		request.put(UPDATE_UNTIL, time);

		JsonNode reply;

		try {
			socket.send(objectMapper.writeValueAsString(request).getBytes(ZMQ.CHARSET), 0);

			reply = objectMapper.readTree(new String(socket.recv(0), ZMQ.CHARSET));

			if (REPLY_ERROR.equals(reply.get(REPLY_STATUS).asText())) {
				throw new RuntimeException("Update until: " + reply.get(REPLY_ERROR_MESSAGE).asText());
			}
		} catch (JsonProcessingException jsonProcessingException) {
			throw new RuntimeException(jsonProcessingException);
		}

		return REPLY_OK.equals(reply.get(REPLY_STATUS).asText());
	}

	public boolean finalizeModel() {
		ObjectNode request = objectMapper.createObjectNode();
		request.put(FUNCTION_KEY, FUNCTION_FINALIZE_MODEL);

		JsonNode reply;

		try {
			socket.send(objectMapper.writeValueAsString(request).getBytes(ZMQ.CHARSET), 0);

			reply = objectMapper.readTree(new String(socket.recv(0), ZMQ.CHARSET));

			if (REPLY_ERROR.equals(reply.get(REPLY_STATUS).asText())) {
				throw new RuntimeException("Finalize model: " + reply.get(REPLY_ERROR_MESSAGE).asText());
			}
		} catch (JsonProcessingException jsonProcessingException) {
			throw new RuntimeException(jsonProcessingException);
		}

		return REPLY_OK.equals(reply.get(REPLY_STATUS).asText());
	}

	public List<String> getInputVarNames() {
		try {
			return Arrays.asList(objectMapper.readValue(getReply(FUNCTION_GET_INPUT_VAR_NAMES, RETURN_INPUT_VAR_NAMES).asText(), String[].class));
		} catch (JsonProcessingException jsonProcessingException) {
			throw new RuntimeException("Getting input var names", jsonProcessingException);
		}
	}

	public List<String> getOutputVarNames() {
		try {
			return Arrays.asList(objectMapper.readValue(getReply(FUNCTION_GET_OUTPUT_VAR_NAMES, RETURN_OUTPUT_VAR_NAMES).asText(), String[].class));
		} catch (JsonProcessingException jsonProcessingException) {
			throw new RuntimeException("Getting output var names", jsonProcessingException);
		}
	}

	public boolean loadState(File path) {
		ObjectNode request = objectMapper.createObjectNode();
		request.put(FUNCTION_KEY, FUNCTION_LOAD_STATE);
		request.put(LOAD_STATE_PATH, path.getPath());

		JsonNode reply;

		try {
			socket.send(objectMapper.writeValueAsString(request).getBytes(ZMQ.CHARSET), 0);

			reply = objectMapper.readTree(new String(socket.recv(0), ZMQ.CHARSET));

			if (REPLY_ERROR.equals(reply.get(REPLY_STATUS).asText())) {
				throw new RuntimeException("Load state: " + reply.get(REPLY_ERROR_MESSAGE).asText());
			}
		} catch (JsonProcessingException jsonProcessingException) {
			throw new RuntimeException(jsonProcessingException);
		}

		return REPLY_OK.equals(reply.get(REPLY_STATUS).asText());
	}

	public boolean saveState(File path) {
		ObjectNode request = objectMapper.createObjectNode();
		request.put(FUNCTION_KEY, FUNCTION_SAVE_STATE);
		request.put(SAVE_STATE_PATH, path.getPath());

		JsonNode reply;

		try {
			socket.send(objectMapper.writeValueAsString(request).getBytes(ZMQ.CHARSET), 0);

			reply = objectMapper.readTree(new String(socket.recv(0), ZMQ.CHARSET));

			if (REPLY_ERROR.equals(reply.get(REPLY_STATUS).asText())) {
				throw new RuntimeException("Save state: " + reply.get(REPLY_ERROR_MESSAGE).asText());
			}
		} catch (JsonProcessingException jsonProcessingException) {
			throw new RuntimeException(jsonProcessingException);
		}

		return REPLY_OK.equals(reply.get(REPLY_STATUS).asText());
	}

	public long getVarItemSize(String id) {
		return getReplyById(FUNCTION_GET_VAR_ITEM_SIZE, RETURN_ITEM_SIZE, id).asLong();
	}

	public String getVarType(String id) {
		return getReplyById(FUNCTION_GET_VAR_ITEM_TYPE, RETURN_ITEM_TYPE, id).asText();
	}

	public Double getValue(String id) {
		return getReplyById(FUNCTION_GET_VALUE, RETURN_VALUE, id).asDouble();
	}

	public boolean setValue(String id, String slice) {
		ObjectNode request = objectMapper.createObjectNode();
		request.put(FUNCTION_KEY, FUNCTION_SET_VALUE);
		request.put(VAR_ITEM_ID, id);
		request.put(SLICE, slice);

		JsonNode reply;

		try {
			socket.send(objectMapper.writeValueAsString(request).getBytes(ZMQ.CHARSET), 0);

			reply = objectMapper.readTree(new String(socket.recv(0), ZMQ.CHARSET));

			if (REPLY_ERROR.equals(reply.get(REPLY_STATUS).asText())) {
				throw new RuntimeException("Set value: " + reply.get(REPLY_ERROR_MESSAGE).asText());
			}
		} catch (JsonProcessingException jsonProcessingException) {
			throw new RuntimeException(jsonProcessingException);
		}

		return REPLY_OK.equals(reply.get(REPLY_STATUS).asText());
	}

	private long getLongValue(String functionName, String returnName) {
		return getReply(functionName, returnName).asLong();
	}

	private JsonNode getReply(String functionName, String returnName) {
		ObjectNode request = objectMapper.createObjectNode();
		request.put(FUNCTION_KEY, functionName);

		return getJsonNode(returnName, request);
	}

	private JsonNode getReplyById(String functionName, String returnName, String id) {
		ObjectNode request = objectMapper.createObjectNode();
		request.put(FUNCTION_KEY, functionName);
		request.put(VAR_ITEM_ID, id);

		return getJsonNode(returnName, request);
	}

	private JsonNode getJsonNode(String returnName, ObjectNode request) {
		JsonNode reply;

		try {
			socket.send(objectMapper.writeValueAsString(request).getBytes(ZMQ.CHARSET), 0);

			reply = objectMapper.readTree(new String(socket.recv(0), ZMQ.CHARSET));
		} catch (JsonProcessingException jsonProcessingException) {
			throw new RuntimeException(jsonProcessingException);
		}

		return reply.get(returnName);
	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
		// no need, already done in constructor
	}

	@Override
	public String[] getExchangeItemIDs() {
		// see BmiModelInstance
		return null;
	}

	@Override
	public String[] getExchangeItemIDs(IExchangeItem.Role role) {
		// see BmiModelInstance
		return null;
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		// see BmiModelInstance
		return null;
	}

	@Override
	public void finish() {
		throw new RuntimeException("org.openda.model_wflow.ZeroMQModelInstance.finish() not implemented yet");
	}

	@Override
	public IVector[] getObservedLocalization(String stateExchangeItemID, IObservationDescriptions observationDescriptions, double distance) {
		throw new RuntimeException("org.openda.model_wflow.ZeroMQModelInstance.getObservedLocalization() not implemented yet");
	}

	@Override
	public void announceObservedValues(IObservationDescriptions observationDescriptions) {
		throw new RuntimeException("org.openda.model_wflow.ZeroMQModelInstance.announceObservedValues() not implemented yet");
	}

	@Override
	public IVector getObservedValues(IObservationDescriptions observationDescriptions) {
		throw new RuntimeException("org.openda.model_wflow.ZeroMQModelInstance.getObservedValues() not implemented yet");
	}

	@Override
	public IExchangeItem getExchangeItem(String exchangeItemID) {
		throw new RuntimeException("org.openda.model_wflow.ZeroMQModelInstance.getExchangeItem() not implemented yet");
	}

	@Override
	public ITime getTimeHorizon() {
		double startTime = getStartTime();
		double endTime = getEndTime();
		double timeStepDurationInModelUnits = getTimeStep();
		String timeUnitsString = getTimeUnits();

		double startTimeMjd = TimeUtils.udUnitsTimeToMjd(startTime, timeUnitsString);
		double endTimeMjd = TimeUtils.udUnitsTimeToMjd(endTime, timeUnitsString);
		//convert time step duration from model time units to MJD.
		double timeStepDurationInDays = timeStepDurationInModelUnits * (endTimeMjd - startTimeMjd) / (endTime - startTime);

		return new Time(startTimeMjd, endTimeMjd, timeStepDurationInDays);
	}

	@Override
	public ITime getCurrentTime() {
		return new Time(getCurrentTimeInstant());
	}

	@Override
	public void compute(ITime targetTime) {
		throw new RuntimeException("org.openda.model_wflow.ZeroMQModelInstance.compute() not implemented yet");
	}

	@Override
	public ILocalizationDomains getLocalizationDomains() {
		throw new RuntimeException("org.openda.model_wflow.ZeroMQModelInstance.getLocalizationDomains() not implemented yet");
	}

	@Override
	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance) {
		throw new RuntimeException("org.openda.model_wflow.ZeroMQModelInstance.getObservedLocalization() not implemented yet");
	}

	@Override
	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance, int iDomain) {
		throw new RuntimeException("org.openda.model_wflow.ZeroMQModelInstance.getObservedLocalization() not implemented yet");
	}

	@Override
	public IModelState saveInternalState() {
		throw new RuntimeException("org.openda.model_wflow.ZeroMQModelInstance.saveInternalState() not implemented yet");
	}

	@Override
	public void restoreInternalState(IModelState savedInternalState) {
		throw new RuntimeException("org.openda.model_wflow.ZeroMQModelInstance.restoreInternalState() not implemented yet");
	}

	@Override
	public void releaseInternalState(IModelState savedInternalState) {
		throw new RuntimeException("org.openda.model_wflow.ZeroMQModelInstance.releaseInternalState() not implemented yet");
	}

	@Override
	public IModelState loadPersistentState(File persistentStateFile) {
		throw new RuntimeException("org.openda.model_wflow.ZeroMQModelInstance.loadPersistentState() not implemented yet");
	}

	@Override
	public File getModelRunDir() {
		throw new RuntimeException("org.openda.model_wflow.ZeroMQModelInstance.getModelRunDir() not implemented yet");
	}

	@Override
	public void setInOutputMode(boolean inOutputMode) {
		throw new RuntimeException("org.openda.model_wflow.ZeroMQModelInstance.setInOutputMode() not implemented yet");
	}
}
