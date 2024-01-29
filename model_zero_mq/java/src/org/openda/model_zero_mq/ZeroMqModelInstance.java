package org.openda.model_zero_mq;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.NetcdfGridTimeSeriesExchangeItem;
import org.openda.interfaces.*;
import org.openda.utils.Instance;
import org.openda.utils.Time;
import org.openda.utils.io.AnalysisDataWriter;
import org.zeromq.ZMQ;

import java.io.File;
import java.util.*;

public class ZeroMqModelInstance extends Instance implements IModelInstance, IModelExtensions, IOutputModeSetter {
	private final ObjectMapper objectMapper = new ObjectMapper();
	private static final String FUNCTION_KEY = "fn";
	private static final String FUNCTION_INITIALISE = "initialize";
	private static final String FUNCTION_GET_START_TIME = "get_start_time";
	private static final String FUNCTION_GET_START_UNIX_TIME = "get_start_unix_time";
	private static final String RETURN_START_TIME = "start_time";
	private static final String RETURN_START_UNIX_TIME = "start_unix_time";
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
	private static final String FUNCTION_GET_VAR_N_BYTES = "get_var_nbytes";
	private static final String FUNCTION_GET_VAR_GRID = "get_var_grid";
	private static final String FUNCTION_GET_GRID_SIZE = "get_grid_size";
	private static final String FUNCTION_GET_GRID_X = "get_grid_x";
	private static final String FUNCTION_GET_GRID_Y = "get_grid_y";
	private static final String RETURN_GRID = "var_grid";
	private static final String RETURN_GRID_SIZE = "grid_size";
	private static final String RETURN_GRID_X = "grid_x";
	private static final String RETURN_GRID_Y = "grid_y";
	private static final String VAR_GRID = "grid";
	private static final String VAR_ITEM_ID = "name";
	private static final String RETURN_ITEM_SIZE = "var_itemsize";
	private static final String RETURN_N_BYTES = "var_nbytes";
	private static final String FUNCTION_GET_VAR_ITEM_TYPE = "get_var_type";
	private static final String FUNCTION_GET_VAR_UNITS = "get_var_units";
	private static final String RETURN_ITEM_TYPE = "var_type";
	private static final String RETURN_VAR_UNITS = "var_units";
	private static final String FUNCTION_GET_VALUE = "get_value";
	private static final String RETURN_VALUE = "value";
	private static final String FUNCTION_SET_VALUE = "set_value";
	private static final String SLICE = "src";
	private static final String DEST = "dest";
	private static final String CONFIGURATION_FILE = "config_file";
	private static final String REPLY_STATUS = "status";
	private static final String REPLY_OK = "OK";
	private static final String REPLY_ERROR = "ERROR";
	private static final String REPLY_ERROR_MESSAGE = "error";
	private static final String FUNCTION_SHUTDOWN_MODEL = "shutdown";
	private final ZMQ.Socket socket;
	private final File modelRunDir;
	private final String modelConfigFile;
	private final int modelInstanceNumber;

	private final Map<String, IExchangeItem> exchangeItems;
	//private Map<String, DoublesExchangeItem> bufferedExchangeItems;
	private final Map<String, IExchangeItem> forcingExchangeItems;
	private final Map<String, IExchangeItem> staticLimitExchangeItems;
	private final LinkedHashMap<String, IExchangeItem> modelStateExchangeItems;

	private boolean inOutputMode = false;
	private TimeUnit timeUnit;
	private double startTimeMjd;
	private final AnalysisDataWriter analysisDataWriter;
	private boolean firstTime = true;

	public enum TimeUnit {
		S(86400), MIN(1440), H(24), D(1);

		private final int partsInDay;

		TimeUnit(int partsInDay) {
			this.partsInDay = partsInDay;
		}
	}

	public ZeroMqModelInstance(int modelInstanceNumber, ZMQ.Socket socket, File modelRunDir, String modelConfigFile, double missingValue, ArrayList<ZeroMqModelForcingConfig> forcingConfiguration, ArrayList<ZeroMqModelForcingConfig> staticLimitConfiguration, List<ZeroMqModelFactory.ZeroMqModelStateExchangeItemsInfo> modelStateExchangeItemInfos) {
		this.modelInstanceNumber = modelInstanceNumber;
		this.socket = socket;
		this.modelRunDir = modelRunDir;
		this.modelConfigFile = modelConfigFile;

		initializeModel();

		exchangeItems = createExchangeItems(missingValue);

		staticLimitExchangeItems = createForcingExchangeItems(staticLimitConfiguration);
		modelStateExchangeItems = new LinkedHashMap<>();
		for (ZeroMqModelFactory.ZeroMqModelStateExchangeItemsInfo modelStateExchangeItemInfo : modelStateExchangeItemInfos) {
			String stateId = modelStateExchangeItemInfo.getStateId();

			Double[] upperLimits = modelStateExchangeItemInfo.getModelStateExchangeItemUpperLimits();
			Double[] lowerLimits = modelStateExchangeItemInfo.getModelStateExchangeItemLowerLimits();
			String[] lowerLimitExchangeItemIds = modelStateExchangeItemInfo.getLowerLimitExchangeItemIds();
			String[] upperLimitExchangeItemIds = modelStateExchangeItemInfo.getUpperLimitExchangeItemIds();
			assert upperLimits.length == lowerLimitExchangeItemIds.length;
			assert upperLimits.length == upperLimitExchangeItemIds.length;

			double[][] upperLimits2D = getLimits2D(upperLimits, upperLimitExchangeItemIds);

			double[][] lowerLimits2D = getLimits2D(lowerLimits, lowerLimitExchangeItemIds);

			modelStateExchangeItems.put(stateId, new ZeroMqStateExchangeItem(modelStateExchangeItemInfo.getModelStateExchangeItemIds(), lowerLimits2D, upperLimits2D, this, missingValue));
		}

		forcingExchangeItems = createForcingExchangeItems(forcingConfiguration);

		this.analysisDataWriter = new AnalysisDataWriter(exchangeItems.values(), modelRunDir);
	}
	private Map<String, IExchangeItem> createForcingExchangeItems(ArrayList<ZeroMqModelForcingConfig> bmiModelForcingConfigs) {
		Map<String, IExchangeItem> result = new HashMap<>();

		for (ZeroMqModelForcingConfig forcingConfig : bmiModelForcingConfigs) {
			File forcingFile = new File(this.modelRunDir, forcingConfig.getDataObjectFileName());
			if (!forcingFile.exists()) {
				throw new RuntimeException(getClass().getSimpleName() + ": Cannot find forcing file " + forcingFile.getAbsolutePath() + " configured in bmiModelFactory config xml file.");
			}

			IDataObject dataObject = BBUtils.createDataObject(this.modelRunDir, forcingConfig.getClassName(), forcingConfig.getDataObjectFileName(), forcingConfig.getArguments());
			for (String ExchangeItemId : dataObject.getExchangeItemIDs()) {
				result.put(ExchangeItemId, dataObject.getDataObjectExchangeItem(ExchangeItemId));
			}
			if (dataObject instanceof IEnsembleDataObject) {
				IEnsembleDataObject ensembleDataObject = (IEnsembleDataObject) dataObject;
				String[] ensembleExchangeItemIds = ensembleDataObject.getEnsembleExchangeItemIds();
				for (String exchangeItemId : ensembleExchangeItemIds) {
					IExchangeItem ensembleExchangeItem = ensembleDataObject.getDataObjectExchangeItem(exchangeItemId, this.modelInstanceNumber);
					result.put(ensembleExchangeItem.getId(), ensembleExchangeItem);
				}
			}
		}

		return result;
	}


	private Map<String, IExchangeItem> createExchangeItems(double modelMissingValue) {
		Set<String> inoutVars = new HashSet<>();

		// first fill sets with input and output variables
		Set<String> inputVars = new HashSet<>(getInputVarNames());
		Set<String> outputVars = new HashSet<>(getOutputVarNames());

		// then put duplicates in inout variables.
		// Note: Loop over copy of set to prevent iterator exception
		for (String var : inputVars.toArray(new String[0])) {
			if (outputVars.contains(var)) {
				inputVars.remove(var);
				outputVars.remove(var);
				inoutVars.add(var);
			}
		}

		Map<String, IExchangeItem> result = new HashMap<>();

		for (String variable : inputVars) {
			ZeroMqOutputExchangeItem item = new ZeroMqOutputExchangeItem(variable, IExchangeItem.Role.Input, this, modelMissingValue);
			result.put(variable, item);
		}

		for (String variable : outputVars) {
			ZeroMqOutputExchangeItem item = new ZeroMqOutputExchangeItem(variable, IExchangeItem.Role.Output, this, modelMissingValue);
			result.put(variable, item);
		}

		for (String variable : inoutVars) {
			ZeroMqOutputExchangeItem item = new ZeroMqOutputExchangeItem(variable, IExchangeItem.Role.InOut, this, modelMissingValue);
			result.put(variable, item);
		}
		return result;
	}

	private double[][] getLimits2D(Double[] limits, String[] lowerLimitExchangeItemIds) {
		double[][] lowerLimits2D = new double[limits.length][];
		for (int i = 0; i < lowerLimitExchangeItemIds.length; i++) {
			String lowerLimitExchangeItemId = lowerLimitExchangeItemIds[i];
			if (lowerLimitExchangeItemId == null) {
				lowerLimits2D[i] = new double[]{limits[i]};
				continue;
			}
			IExchangeItem lowerLimitItem = staticLimitExchangeItems.get(lowerLimitExchangeItemId);
			if (lowerLimitItem == null) throw new RuntimeException("Config.Error: No static limit exchange item found with id " + lowerLimitExchangeItemId);
			if (!(lowerLimitItem instanceof NetcdfGridTimeSeriesExchangeItem)) throw new RuntimeException("Config.Error: Only static limit exchange items of NetcdfGridTimeSeries supported.");
			lowerLimits2D[i] = ((NetcdfGridTimeSeriesExchangeItem) lowerLimitItem).getValuesAsDoublesForSingleTimeIndex(0);
		}
		return lowerLimits2D;
	}

	public boolean initializeModel() {
		File modelConfigurationFile = new File(modelRunDir, modelConfigFile);

		if (!modelConfigurationFile.exists()) {
			throw new RuntimeException("Initialisation failure: Model configuration file does not exist: " + modelConfigurationFile);
		}

		return initializeModel(modelConfigurationFile);
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

	public long getStartUnixTime() {
		return getLongValue(FUNCTION_GET_START_UNIX_TIME, RETURN_START_UNIX_TIME);
	}

	public double getEndTime() {
		return getLongValue(FUNCTION_GET_END_TIME, RETURN_END_TIME);
	}

	public double getTimeStep() {
		return getDoubleValue(FUNCTION_GET_TIME_STEP, RETURN_TIME_STEP);
	}

	public String getTimeUnits() {
		return getReply(FUNCTION_GET_TIME_UNITS, RETURN_TIME_UNITS).asText();
	}

	public double getCurrentTimeInstant() {
		return getDoubleValue(FUNCTION_GET_CURRENT_TIME, RETURN_CURRENT_TIME);
	}

	public boolean updateUntil(double time) {
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
		JsonNode reply = getReply(FUNCTION_GET_INPUT_VAR_NAMES, RETURN_INPUT_VAR_NAMES);

		return getVarNamesFromResponse(reply);
	}

	public List<String> getOutputVarNames() {
		JsonNode reply = getReply(FUNCTION_GET_OUTPUT_VAR_NAMES, RETURN_OUTPUT_VAR_NAMES);

		return getVarNamesFromResponse(reply);
	}

	private static List<String> getVarNamesFromResponse(JsonNode reply) {
		List<String> varNames = new ArrayList<>(reply.size());

		for (int varNameIndex = 0; varNameIndex < reply.size(); varNameIndex++) {
			varNames.add(reply.get(varNameIndex).asText());
		}

		return varNames;
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

	public int getVarItemSize(String id) {
		if (id.equals("lateral.land.alpha_pow")) {
			System.out.println("get var itemsize for " + id);
		}
		return getReplyById(FUNCTION_GET_VAR_ITEM_SIZE, RETURN_ITEM_SIZE, id).asInt();
	}

	public int getVarNBytes(String id) {
		return getReplyById(FUNCTION_GET_VAR_N_BYTES, RETURN_N_BYTES, id).asInt();
	}

	public int getGridSize(int id) {
		JsonNode replyById = getReplyForGridSize(FUNCTION_GET_GRID_SIZE, RETURN_GRID_SIZE, id);
		return replyById.asInt();
	}

	public double[] getGridX(int grid, double[] gridArray) {
		return getGridValues(grid, FUNCTION_GET_GRID_X, RETURN_GRID_X, gridArray, "x");
	}

	public double[] getGridY(int grid, double[] gridArray) {
		return getGridValues(grid, FUNCTION_GET_GRID_Y, RETURN_GRID_Y, gridArray, "y");
	}

	private double[] getGridValues(int grid, String getGridDimension, String returnGridDimension, double[] gridArray, String axis) {
		JsonNode replyForGridValues = getReplyForGridValues(getGridDimension, returnGridDimension, grid, gridArray, axis);
		// TODO: prevent by filtering exchange items?
		if (replyForGridValues == null) return new double[0];
		Iterator<JsonNode> elements = replyForGridValues.elements();
		List<Double> values = new ArrayList<>();
		while (elements.hasNext()) {
			values.add(elements.next().asDouble());
		}
		return values.stream().mapToDouble(Double::doubleValue).toArray();
	}

	public int getVarGrid(String id) {
		return getReplyById(FUNCTION_GET_VAR_GRID, RETURN_GRID, id).asInt();
	}

	public String getVarType(String id) {
		return getReplyById(FUNCTION_GET_VAR_ITEM_TYPE, RETURN_ITEM_TYPE, id).asText();
	}

	public String getVarUnits(String id) {
		return getReplyById(FUNCTION_GET_VAR_UNITS, RETURN_VAR_UNITS, id).asText();
	}

	public Double getValue(String id) {
		return getReplyById(FUNCTION_GET_VALUE, RETURN_VALUE, id).asDouble();
	}

	public double[] getValues(String id, double[] dummyValuesArray) {
		JsonNode replyById = getReplyForGetValues(FUNCTION_GET_VALUE, RETURN_VALUE, id, dummyValuesArray);
		Iterator<JsonNode> elements = replyById.elements();
		List<Double> values = new ArrayList<>();
		while (elements.hasNext()) {
			values.add(elements.next().asDouble());
		}
		return values.stream().mapToDouble(Double::doubleValue).toArray();
	}

	public boolean setValue(String id, double[] slice) {
		ObjectNode request = objectMapper.createObjectNode();
		request.put(FUNCTION_KEY, FUNCTION_SET_VALUE);
		request.put(VAR_ITEM_ID, id);
		ArrayNode arrayNode = request.putArray(SLICE);
		for (double value : slice) {
			arrayNode.add(value);
		}

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

	private double getDoubleValue(String functionName, String returnName) {
		return getReply(functionName, returnName).asDouble();
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

	private JsonNode getReplyForGetValues(String functionName, String returnName, String id, double[] gridArray) {
		ObjectNode request = objectMapper.createObjectNode();
		request.put(FUNCTION_KEY, functionName);
		request.put(VAR_ITEM_ID, id);

		ArrayNode arrayNode = request.putArray(DEST);
		for (double index : gridArray) {
			arrayNode.add(index);
		}

		return getJsonNode(returnName, request);
	}

	private JsonNode getReplyForGridValues(String functionName, String returnName, int value, double[] gridArray, String axis) {
		ObjectNode request = objectMapper.createObjectNode();
		request.put(FUNCTION_KEY, functionName);
		request.put(VAR_GRID, value);

		ArrayNode arrayNode = request.putArray(axis);
		for (double index : gridArray) {
			arrayNode.add(index);
		}

		return getJsonNode(returnName, request);
	}

	private JsonNode getReplyForGridSize(String functionName, String returnName, int value) {
		ObjectNode request = objectMapper.createObjectNode();
		request.put(FUNCTION_KEY, functionName);
		request.put(VAR_GRID, value);

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
		Set<String> ids = exchangeItems.keySet();
		return ids.toArray(new String[0]);
	}

	@Override
	public String[] getExchangeItemIDs(IExchangeItem.Role role) {
		List<String> ids = new ArrayList<>();
		for (IExchangeItem exchangeItem : this.exchangeItems.values()) {
			if (exchangeItem.getRole() == role) {
				ids.add(exchangeItem.getId());
			}
		}
		return ids.toArray(new String[0]);
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemId) {
		IExchangeItem exchangeItem = this.modelStateExchangeItems.get(exchangeItemId);
		if (exchangeItem == null && this.forcingExchangeItems != null) {
			exchangeItem = this.forcingExchangeItems.get(exchangeItemId);
		}
		if (exchangeItem == null && this.exchangeItems != null) {
			exchangeItem = this.exchangeItems.get(exchangeItemId);
		}
		if (exchangeItem == null) {
			throw new RuntimeException("Exchange item with id '" + exchangeItemId + "' not found in " + getClass().getSimpleName());
		}
		return exchangeItem;
	}

	@Override
	public void finish() {

		analysisDataWriter.writeDataAfterAnalysis();
		analysisDataWriter.close();

		finalizeModel();

		ObjectNode request = objectMapper.createObjectNode();
		request.put(FUNCTION_KEY, FUNCTION_SHUTDOWN_MODEL);

		try {
			socket.send(objectMapper.writeValueAsString(request).getBytes(ZMQ.CHARSET), 0);
			socket.close();
		} catch (JsonProcessingException jsonProcessingException) {
			throw new RuntimeException(jsonProcessingException);
		}
	}

	@Override
	public IVector[] getObservedLocalization(String stateExchangeItemID, IObservationDescriptions observationDescriptions, double distance) {
		throw new RuntimeException("org.openda.model_wflow.ZeroMQModelInstance.getObservedLocalization() not implemented yet");
	}

	@Override
	public void announceObservedValues(IObservationDescriptions observationDescriptions) {
/*		ITime[] selectedTimes = observationDescriptions.getTimes();
		if (selectedTimes == null || selectedTimes.length == 0) {
			return;
		}
			if (bufferedExchangeItems != null) { bufferedExchangeItems.clear(); }
			bufferedExchangeItems = createBufferedExchangeItems(selectedTimes);*/
	}

	@Override
	public IVector getObservedValues(IObservationDescriptions observationDescriptions) {
		return null;
	}

	@Override
	public IExchangeItem getExchangeItem(String exchangeItemId) {
/*		if (inOutputMode && bufferedExchangeItems !=null && bufferedExchangeItems.containsKey(exchangeItemId)) {
			return bufferedExchangeItems.get(exchangeItemId);
		}*/
		return getDataObjectExchangeItem(exchangeItemId);
	}

	@Override
	public ITime getTimeHorizon() {
		long startUnixTime = getStartUnixTime();
		startTimeMjd = Time.milliesToMjd(startUnixTime * 1000);
		double timeStepDurationInModelUnits = getTimeStep();
		String timeUnits = getTimeUnits();
		this.timeUnit = TimeUnit.valueOf(timeUnits.toUpperCase());
		double endTime = getEndTime();
		double endTimeMjd = (long) (endTime / this.timeUnit.partsInDay) + startTimeMjd;
		//convert time step duration from model time units to MJD.
		double timeStepDurationInDays = timeStepDurationInModelUnits / timeUnit.partsInDay;

		return new Time(startTimeMjd, endTimeMjd, timeStepDurationInDays);
	}

	@Override
	public ITime getCurrentTime() {
		double currentTimeInstant = getCurrentTimeInstant();
		double currentTimeMjd = currentTimeInstant / timeUnit.partsInDay + startTimeMjd;
		return new Time(currentTimeMjd);
	}

	@Override
	public void compute(ITime targetTime) {
		if (firstTime) {
			firstTime = false;
		} else {
			//write model state data after analysis (state update).
			analysisDataWriter.writeDataAfterAnalysis();
		}

		double mjd = targetTime.getMJD() - startTimeMjd;
		double time = mjd * timeUnit.partsInDay;
		updateUntil(time);

		analysisDataWriter.writeDataBeforeAnalysis();
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
		return modelRunDir;
	}

	@Override
	public void setInOutputMode(boolean inOutputMode) {
		this.inOutputMode = inOutputMode;
	}
}
