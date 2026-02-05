package org.openda.model_dflowfm;

import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.generalJavaUtils.StringUtilities;

import java.io.*;
import java.util.*;

public class DFlowFMSpatialRoughnessFile implements IDataObject {

	private static final String GENERAL = "[General]";
	private static final String GLOBAL = "[Global]";
	private static final String BRANCH = "[Branch]";
	private static final String OBSERVATION_POINT = "[ObservationPoint]";

	private static final String FILE_VERSION = "fileVersion";
	private static final String FILE_TYPE = "fileType";

	private static final String FRICTION_ID = "frictionId";
	private static final String FRICTION_TYPE = "frictionType";
	private static final String FRICTION_VALUE = "frictionValue";

	private static final String BRANCH_ID = "branchId";
	private static final String ROUGHNESS_TYPE = "roughnessType";
	private static final String FUNCTION_TYPE = "functionType";
	private static final String NUM_LEVELS = "numLevels";
	private static final String LEVELS = "levels";
	private static final String CHAINAGE = "chainage";
	private static final String VALUES = "values";
	private static final String VALUE = "value";

	private static final String ID = "id";
	private static final String NAME = "name";

	public static final String OBSERVATION_FILE = "observationFile";
	public static final String OBSERVATION_SELECTION_FILE = "observationSelectionFile";
	public static final String REVERSED_POSTFIX = " (Reversed)";

	@SuppressWarnings("WeakerAccess")
	LinkedHashMap<String, IExchangeItem> exchangeItems = new LinkedHashMap<>();
	private String fileVersion;
	private String fileType;
	private String globalFrictionType;
	private double globalFrictionValue;
	private String frictionId;
	private File spatialRoughnessFile;
	private File observationFile;
	private File observationSelectionFile;

	enum RoughnessType {
		Chezy(1), Manning(4), Nikuradse(5), Strickler(6), WhiteColebrook(7), BosBijkerk(9);

		private final int value;

		RoughnessType(int i) {
			value = i;
		}

		int getValue() {
			return value;
		}

		static RoughnessType getInstance(int value) {
			switch (value) {
				case (1):
					return Chezy;
				case (4):
					return Manning;
				case (5):
					return Nikuradse;
				case (6):
					return Strickler;
				case (7):
					return WhiteColebrook;
				case (9):
					return BosBijkerk;
				default:
					throw new RuntimeException("Unknown globalType");
			}
		}
	}

	@Override
	public String[] getExchangeItemIDs() {
		return exchangeItems.keySet().toArray(new String[exchangeItems.keySet().size()]);
	}

	@Override
	public String[] getExchangeItemIDs(IExchangeItem.Role role) {
		return getExchangeItemIDs();
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return exchangeItems.get(exchangeItemID);
	}

	@Override
	public void finish() {
		try (FileOutputStream fileOutputStream = new FileOutputStream(spatialRoughnessFile);
			 OutputStreamWriter outputStreamWriter = new OutputStreamWriter(fileOutputStream);
			 BufferedWriter lineWriter = new BufferedWriter(outputStreamWriter)) {
			StringBuilder builder = new StringBuilder(1000);
			appendGeneral(builder);
			appendContent(builder);

			Set<String> writtenBranchProperties = new HashSet<>();
			for (IExchangeItem item : exchangeItems.values()) {
				DFlowFMSpatialRoughnessExchangeItem exchangeItem = (DFlowFMSpatialRoughnessExchangeItem) item;
				DFlowFMSpatialRoughnessExchangeItem.InitialBranchDefinitions initialBranchDefinitions = exchangeItem.getInitialBranchDefinitions();
				if (initialBranchDefinitions == null) continue;
				String branchId = initialBranchDefinitions.getBranchId();
				if (!writtenBranchProperties.add(branchId)) continue;
				appendBranchProperties(builder, initialBranchDefinitions, branchId);
			}

			lineWriter.write(builder.toString());
		} catch (IOException e) {
			throw new RuntimeException(e.getMessage(), e);
		}
	}

	private void appendBranchProperties(StringBuilder builder, DFlowFMSpatialRoughnessExchangeItem.InitialBranchDefinitions initialBranchDefinitions, String branchId) {
		builder.append(BRANCH + '\n');
		builder.append(BRANCH_ID + '=').append(branchId).append('\n');
		builder.append(ROUGHNESS_TYPE + '=').append(initialBranchDefinitions.getRoughnessType()).append('\n');
		String functionType = initialBranchDefinitions.getFunctionType();
		builder.append(FUNCTION_TYPE + '=').append(functionType).append('\n');
		if (functionType.equals("constant")) {
			builder.append('\n');
			return;
		}
		double[] levels = initialBranchDefinitions.getLevels();
		assert levels != null && levels.length > 0;
		builder.append(NUM_LEVELS + '=').append(levels.length).append('\n');
		builder.append(LEVELS + '=');
		for (double level : levels) {
			builder.append(" ").append(level);
		}
		builder.append('\n');
		builder.append('\n');
	}

	private void appendContent(StringBuilder builder) {
		builder.append(GLOBAL + '\n');
		builder.append(FRICTION_ID + '=').append(frictionId).append('\n');
		builder.append(FRICTION_TYPE + '=').append(globalFrictionType).append('\n');
		builder.append(FRICTION_VALUE + '=').append(globalFrictionValue).append('\n');
		builder.append('\n');
	}

	private void appendGeneral(StringBuilder builder) {
		builder.append(GENERAL + '\n');
		builder.append(FILE_VERSION + '=').append(fileVersion).append('\n');
		builder.append(FILE_TYPE + '=').append(fileType).append('\n');
		builder.append('\n');
	}

	@Override
	public void initialize(File workingDir, String[] arguments) {
		spatialRoughnessFile = new File(workingDir, arguments[0]);

		for (int i = 1; i < arguments.length; i++) {
			String argument = arguments[i];
			String[] keyValue = StringUtilities.getKeyValuePair(argument);
			String key = keyValue[0];
			String value = keyValue[1];
			switch (key) {
				case OBSERVATION_FILE:
					observationFile = new File(workingDir, value);
					continue;
				case OBSERVATION_SELECTION_FILE:
					observationSelectionFile = new File(workingDir, value);
					continue;
				default:
					throw new RuntimeException("Unknown key " + key + ". Please specify only targetFile as key=value pair");
			}
		}

		Set<String> observationPointIdsSelection = null;
		if (observationSelectionFile != null) {
			if (!observationSelectionFile.exists()) throw new RuntimeException("Observation file " + observationSelectionFile.getAbsolutePath() + " does not exist");
			observationPointIdsSelection = new HashSet<>();
			try (FileInputStream fileInputStream = new FileInputStream(observationSelectionFile);
				 InputStreamReader inputStreamReader = new InputStreamReader(fileInputStream);
				 BufferedReader lineReader = new BufferedReader(inputStreamReader)) {
				String line = lineReader.readLine();
				while (line != null) {
					if (!line.startsWith("#")) {
						observationPointIdsSelection.add(line.trim());
					}
					line = lineReader.readLine();

				}
			} catch (IOException e) {
				throw new RuntimeException(e.getMessage(), e);
			}
		}
		//noinspection unchecked
		Map<String, List<ObservationPoint>> observationPoints = observationFile != null ? readObservationFile(observationPointIdsSelection) : Collections.EMPTY_MAP;

		readSpatialDefinitionFile(observationPoints);
	}

	private Map<String, List<ObservationPoint>> readObservationFile(Set<String> observationPointIdsSelection) {
		if (!observationFile.exists()) throw new RuntimeException("Observation file " + observationFile.getAbsolutePath() + " does not exist");
		Map<String, List<ObservationPoint>> observationPointsMap = new LinkedHashMap<>();
		try (FileInputStream fileInputStream = new FileInputStream(observationFile);
			 InputStreamReader inputStreamReader = new InputStreamReader(fileInputStream);
			 BufferedReader lineReader = new BufferedReader(inputStreamReader)) {
			String line = lineReader.readLine();
			if (!line.contains(GENERAL)) throw new RuntimeException("File should start with " + GENERAL);

			line = skipGeneral(lineReader);

			if (!line.contains(OBSERVATION_POINT)) throw new RuntimeException("File should have " + OBSERVATION_POINT + " after " + GENERAL);

			while (line.startsWith(OBSERVATION_POINT)) {

				line = lineReader.readLine();
				String[] keyValue = readKeyValueLine(line);
				String name = keyValue[1];

				line = lineReader.readLine();
				keyValue = readKeyValueLine(line);
				String branchId = keyValue[1];

				line = lineReader.readLine();
				keyValue = readKeyValueLine(line);
				double chainage = Double.parseDouble(keyValue[1]);

				lineReader.readLine();

				if (observationPointIdsSelection == null || observationPointIdsSelection.contains(name)) addObservationPoint(observationPointsMap, name, branchId, chainage, name);

				line = lineReader.readLine();
				if (line == null) return observationPointsMap;
				while (line.trim().isEmpty()) {
					line = lineReader.readLine();
					if (line == null) return observationPointsMap;
				}
			}

		} catch (IOException e) {
			throw new RuntimeException(e.getMessage(), e);
		}
		return observationPointsMap;
	}

	private void addObservationPoint(Map<String, List<ObservationPoint>> observationPointsMap, String id, String branchId, double chainage, String name) {
		ObservationPoint observationPoint = new ObservationPoint(id, branchId, chainage, name);
		List<ObservationPoint> observationPointList = observationPointsMap.get(branchId);
		if (observationPointList == null) {
			observationPointList = new ArrayList<>();
			observationPointList.add(observationPoint);
			observationPointsMap.put(branchId, observationPointList);
		} else {
			observationPointList.add(observationPoint);
		}
	}

	private String skipGeneral(BufferedReader lineReader) throws IOException {
		lineReader.readLine();
		lineReader.readLine();
		lineReader.readLine();
		String line = lineReader.readLine();
		while (line.isEmpty()) {
			line = lineReader.readLine();
		}

		return line;
	}

	private void readSpatialDefinitionFile(Map<String, List<ObservationPoint>> observationPoints) {
		try (FileInputStream fileInputStream = new FileInputStream(spatialRoughnessFile);
			 InputStreamReader inputStreamReader = new InputStreamReader(fileInputStream);
			 BufferedReader lineReader = new BufferedReader(inputStreamReader)) {
			String line = lineReader.readLine();
			if (!line.contains(GENERAL)) throw new RuntimeException("File should start with " + GENERAL);
			readGeneral(lineReader);

			line = lineReader.readLine();
			if (!line.startsWith(GLOBAL)) throw new RuntimeException("File should have " + GLOBAL + " after " + GENERAL);
			DFlowFMSpatialRoughnessExchangeItem globalItem = readContent(lineReader);
			exchangeItems.put(globalItem.getId(), globalItem);

			Map<String, DFlowFMSpatialRoughnessExchangeItem.InitialBranchDefinitions> initialBranchDefinitionsMap = new LinkedHashMap<>();

			line = lineReader.readLine();
			while (line.startsWith(BRANCH)) {
				readBranch(lineReader, initialBranchDefinitionsMap, observationPoints);

				line = lineReader.readLine();
			}
		} catch (IOException e) {
			throw new RuntimeException(e.getMessage(), e);
		}
	}

	private void readGeneral(BufferedReader lineReader) throws IOException {
		String line = lineReader.readLine();

		String[] keyValue = readKeyValueLine(line);
		fileVersion = keyValue[1];

		line = lineReader.readLine();
		keyValue = readKeyValueLine(line);
		fileType = keyValue[1];

		while (!line.isEmpty()) {
			line = lineReader.readLine();
		}
	}

	private void readBranch(BufferedReader lineReader, Map<String, DFlowFMSpatialRoughnessExchangeItem.InitialBranchDefinitions> initialBranchDefinitionsMapExchangeItemsMap, Map<String, List<ObservationPoint>> observationPoints) throws IOException {
		String line = lineReader.readLine();
		String[] keyValue = readKeyValueLine(line);
		String branchId = keyValue[1].trim();

		line = lineReader.readLine();
		keyValue = readKeyValueLine(line);
		String frictionType = keyValue[1].trim();

		line = lineReader.readLine();
		keyValue = readKeyValueLine(line);
		String functionType = keyValue[1].trim();

		line = lineReader.readLine();
		keyValue = readKeyValueLine(line);
		int numLevels = Integer.parseInt(keyValue[1]);

		line = lineReader.readLine();
		keyValue = readKeyValueLine(line);
		String[] splitLevels = keyValue[1].trim().split(" ");
		assert splitLevels.length == numLevels;
		double[] levels = new double[numLevels];
		for (int i = 0; i < numLevels; i++) {
			levels[i] = Double.parseDouble(splitLevels[i]);
		}

		line = lineReader.readLine();
		keyValue = readKeyValueLine(line);
		int numLocations = Integer.parseInt(keyValue[1]);

		line = lineReader.readLine();
		keyValue = readKeyValueLine(line);
		String[] splitChainage = keyValue[1].trim().split(" ");
		assert splitChainage.length == numLocations;
		double[] chainages = new double[numLocations];
		for (int i = 0; i < numLevels; i++) {
			chainages[i] = Double.parseDouble(splitChainage[i]);
		}

		double[][] frictionValues = new double[numLevels][numLocations];
		int index = 0;
		line = lineReader.readLine();
		keyValue = readKeyValueLine(line);
		String value = keyValue[1];
		while (!value.isEmpty()) {
			String[] splitFrictionValues = value.trim().split(" ");
			for (int i = 0; i < splitFrictionValues.length; i++) {
				String splitFrictionValue = splitFrictionValues[i];
				frictionValues[i][index] = Double.parseDouble(splitFrictionValue);
			}
			index++;
			value = lineReader.readLine();
		}

		for (int i = 0; i < numLevels; i++) {
			String id = getIdWithLevel(chainages[0], 0, branchId, functionType, frictionType);
			exchangeItems.put(id, new DFlowFMSpatialRoughnessExchangeItem(id, frictionType, frictionValues[i], 0, numLocations - 1));
		}
	}

	private String getLevelString(String functionType, int levelIndex) {
		if (functionType.equals("absDischarge")) return "q" + levelIndex;
		if (functionType.equals("Constant")) return "";
		throw new RuntimeException("Unknown function type, should be absDischarge, or Constant");
	}

	private String getIdWithLevel(double firstChainage, int levelIndex, String branchId, String functionType, String frictionType) {
		return frictionId +
			'-' +
			frictionType +
			'-' +
			branchId +
			'-' +
			"x" +
			Math.round(firstChainage) +
			'-' +
			getLevelString(functionType, levelIndex);
	}

	private String getIdWithoutLevel(String firstChainage, String branchId, String frictionType) {
		StringBuilder levelIdBuilder = new StringBuilder(30);
		levelIdBuilder.append(branchId);
		levelIdBuilder.append('-');
		levelIdBuilder.append(frictionType);
		levelIdBuilder.append('-');
		levelIdBuilder.append(branchId);
		levelIdBuilder.append('-');
		levelIdBuilder.append(firstChainage);
		return levelIdBuilder.toString();
	}

	private DFlowFMSpatialRoughnessExchangeItem readContent(BufferedReader lineReader) throws IOException {

		String line = lineReader.readLine();
		String[] keyValue = readKeyValueLine(line);
		frictionId = keyValue[1];

		line = lineReader.readLine();
		keyValue = readKeyValueLine(line);
		globalFrictionType = keyValue[1];

		line = lineReader.readLine();
		keyValue = readKeyValueLine(line);
		globalFrictionValue = Double.parseDouble(keyValue[1]);

		String mainExchangeItemId = frictionId + "-model_wide-" + globalFrictionType;

		line = lineReader.readLine();
		while (!line.isEmpty()) {
			line = lineReader.readLine();
		}

		return new DFlowFMSpatialRoughnessExchangeItem(mainExchangeItemId, globalFrictionType, new double[]{globalFrictionValue}, 0, 0);
	}

	private String[] readKeyValueLine(String line) throws IOException {
		String[] split = line.split("=");
		assert split.length == 2;
		split[0] = split[0].trim();
		split[1] = split[1].trim();
		return split;
	}

	static class ObservationPoint implements Comparable<ObservationPoint> {
		private final String id;
		private final String branchId;
		private final double chainage;
		private final String name;

		public ObservationPoint(String id, String branchId, double chainage, String name) {
			this.id = id;
			this.branchId = branchId;
			this.chainage = chainage;
			this.name = name;
		}

		public String getId() {
			return id;
		}

		public String getBranchId() {
			return branchId;
		}

		public double getChainage() {
			return chainage;
		}

		public String getName() {
			return name;
		}

		@Override
		public int compareTo(ObservationPoint other) {
			int compareTo = branchId.compareTo(other.branchId);
			if (compareTo != 0) return compareTo;
			return Double.compare(chainage, other.chainage);
		}
	}
}
