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
	private static final String FUNCTION_TYPE = "functionType";
	private static final String NUM_LEVELS = "numLevels";
	private static final String NUM_LOCATIONS = "numLocations";
	private static final String LEVELS = "levels";
	private static final String CHAINAGE = "chainage";
	private static final String CONSTANT = "Constant";
	private static final String FRICTION_VALUES = "frictionValues";

	public static final String OBSERVATION_FILE = "observationFile";
	public static final String OBSERVATION_SELECTION_FILE = "observationSelectionFile";

	@SuppressWarnings("WeakerAccess")
	LinkedHashMap<String, DFlowFMSpatialRoughnessExchangeItem> exchangeItems = new LinkedHashMap<>();
	private String fileVersion;
	private String fileType;
	private String globalFrictionType;
	private double globalFrictionValue;
	private String frictionId;
	private File spatialRoughnessFile;
	private File observationFile;
	private File observationSelectionFile;

	public static final class BranchDefinition {
		private final String branchId;
		private final String frictionType;
		private final String functionType;

		public BranchDefinition(String branchId, String frictionType, String functionType) {
			this.branchId = branchId;
			this.frictionType = frictionType;
			this.functionType = functionType;
		}

		public String getBranchId() {
			return branchId;
		}

		public String getFrictionType() {
			return frictionType;
		}

		public String getFunctionType() {
			return functionType;
		}
	}

	@Override
	public String[] getExchangeItemIDs() {
		return exchangeItems.keySet().toArray(new String[0]);
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

			List<DFlowFMSpatialRoughnessExchangeItem> loopExchangeItems = new ArrayList<>(exchangeItems.values());
			DFlowFMSpatialRoughnessExchangeItem previousExchangeItem = loopExchangeItems.get(0);
			BranchDefinition currentBranchDefinition = previousExchangeItem.getBranchDefinition();
			double[][] frictionValues = new double[1][1];

			for (int j = 0; j < loopExchangeItems.size(); j++) {
				DFlowFMSpatialRoughnessExchangeItem exchangeItem = loopExchangeItems.get(j);
				BranchDefinition exchangeItemBranchDefinition = exchangeItem.getBranchDefinition();
				String branchId = exchangeItemBranchDefinition.getBranchId();
				double[] exchangeItemLevels = exchangeItem.getLevels();
				if (!branchId.equals(currentBranchDefinition.getBranchId())) {
					if (j != 0) appendText(previousExchangeItem, builder, currentBranchDefinition, frictionValues);
					currentBranchDefinition = exchangeItemBranchDefinition;
					frictionValues = new double[exchangeItemLevels == null ? 1 : exchangeItemLevels.length][exchangeItem.getChainages().length];
					previousExchangeItem = exchangeItem;
				}
				int levelIndex = exchangeItem.getLevelIndex();
				double[] valuesAsDoubles = exchangeItem.getValuesAsDoubles();
				int chainageStartIndex = exchangeItem.getChainageStartIndex();
				int chainageEndIndex = exchangeItem.getChainageEndIndex();
				if (chainageStartIndex == -1) {
					frictionValues[levelIndex][0] = valuesAsDoubles[levelIndex];
					continue;
				}
				if (chainageEndIndex - chainageStartIndex >= 0)
					System.arraycopy(valuesAsDoubles, 0, frictionValues[levelIndex], chainageStartIndex, chainageEndIndex - chainageStartIndex);
			}
			appendText(previousExchangeItem, builder, currentBranchDefinition, frictionValues);

			lineWriter.write(builder.toString());
		} catch (IOException e) {
			throw new RuntimeException(e.getMessage(), e);
		}
	}

	private void appendText(DFlowFMSpatialRoughnessExchangeItem exchangeItem, StringBuilder builder, BranchDefinition branchDefinition, double[][] frictionValues) {
		String functionType = branchDefinition.getFunctionType();
		if (functionType == null) {
			return;
		}
		builder.append(BRANCH + '\n');
		builder.append(BRANCH_ID + '=').append(branchDefinition.getBranchId()).append('\n');
		builder.append(FRICTION_TYPE + '=').append(branchDefinition.getFrictionType()).append('\n');
		builder.append(FUNCTION_TYPE + '=').append(functionType).append('\n');
		double[] exchangeItemLevels = exchangeItem.getLevels();
		if (!functionType.equalsIgnoreCase(CONSTANT)) {
			builder.append(NUM_LEVELS + '=').append(exchangeItemLevels.length).append('\n');
			builder.append(LEVELS + '=');
			for (double level : exchangeItemLevels) {
				builder.append(" ").append(level);
			}
			builder.append('\n');
		}
		double[] chainages = exchangeItem.getChainages();
		builder.append(NUM_LOCATIONS + '=').append(chainages.length).append('\n');
		builder.append(CHAINAGE + '=');
		for (double chainage : chainages) {
			builder.append(" ").append(chainage);
		}
		builder.append('\n');
		builder.append(FRICTION_VALUES + '=');
		if (exchangeItemLevels == null) {
			for (int i = 0; i < chainages.length; i++) {
				builder.append(" ").append(frictionValues[0][i]);
			}
			builder.append('\n');
			builder.append('\n');
			return;
		}

		for (int chainageIndex = 0; chainageIndex < chainages.length; chainageIndex++) {
			for (int levelIndex = 0; levelIndex < exchangeItemLevels.length; levelIndex++) {
				builder.append(" ").append(frictionValues[levelIndex][chainageIndex]);
			}
			builder.append('\n');
		}
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
			if (keyValue == null) throw new RuntimeException("Invalid key = value pair " + argument + ". Please specify only observationFile=<filePath> and observationSelectionFile=<filePath> as key=value pair");
			String key = keyValue[0];
			String value = keyValue[1];
			readOptionalArguments(workingDir, key, value);
		}

		Set<String> observationPointIdsSelection = getObservationSelection();
		//noinspection unchecked
		Map<String, List<ObservationPoint>> observationPoints = observationFile != null ? readObservationFile(observationPointIdsSelection) : Collections.emptyMap();

		readSpatialDefinitionFile(observationPoints);
	}

	private void readOptionalArguments(File workingDir, String key, String value) {
		switch (key) {
			case OBSERVATION_FILE:
				observationFile = new File(workingDir, value);
				return;
			case OBSERVATION_SELECTION_FILE:
				observationSelectionFile = new File(workingDir, value);
				return;
			default:
				throw new RuntimeException("Unknown key " + key + ". Please specify only targetFile as key=value pair");
		}
	}

	private Set<String> getObservationSelection() {
		Set<String> observationPointIdsSelection = null;
		if (observationSelectionFile == null) return observationPointIdsSelection;
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
		return observationPointIdsSelection;
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

				String name = readKeyValueLine(lineReader.readLine())[1];

				String branchId = readKeyValueLine(lineReader.readLine())[1];

				double chainage = Double.parseDouble(readKeyValueLine(lineReader.readLine())[1]);

				if (observationPointIdsSelection == null || observationPointIdsSelection.contains(name)) addObservationPoint(observationPointsMap, branchId, chainage);

				do {
					line = lineReader.readLine();
					if (line == null) return observationPointsMap;
				} while (line.trim().isEmpty());
			}

		} catch (IOException e) {
			throw new RuntimeException(e.getMessage(), e);
		}
		return observationPointsMap;
	}

	private void addObservationPoint(Map<String, List<ObservationPoint>> observationPointsMap, String branchId, double chainage) {
		ObservationPoint observationPoint = new ObservationPoint(branchId, chainage);
		List<ObservationPoint> observationPointList = observationPointsMap.get(branchId);
		if (observationPointList != null) {
			observationPointList.add(observationPoint);
			return;
		}
		observationPointList = new ArrayList<>();
		observationPointList.add(observationPoint);
		observationPointsMap.put(branchId, observationPointList);
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

			line = lineReader.readLine();
			while (line != null) {
				line = line.startsWith(BRANCH) ? readBranch(lineReader, observationPoints) : lineReader.readLine();
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

	private String readBranch(BufferedReader lineReader, Map<String, List<ObservationPoint>> observationPointsMap) throws IOException {
		BranchDefinition branchDefinition = new BranchDefinition(
			readKeyValueLine(lineReader.readLine())[1].trim(),
			readKeyValueLine(lineReader.readLine())[1].trim(),
			readKeyValueLine(lineReader.readLine())[1].trim());

		List<ObservationPoint> observationPointsList = observationPointsMap == null ? null : observationPointsMap.get(branchDefinition.getBranchId());

		if (observationPointsList != null) Collections.sort(observationPointsList);

		if (branchDefinition.getFunctionType().equalsIgnoreCase("constant")) {
			return createConstantExchangeItems(lineReader, branchDefinition, observationPointsList);
		}

		double[] levels = getLevels(lineReader);

		double[] chainages = getChainages(lineReader);

		double[][] frictionValues = new double[chainages.length][levels.length];
		String line = fillFrictionValues(lineReader, frictionValues);

		if (observationPointsList == null) {
			for (int levelIndex = 0; levelIndex < levels.length; levelIndex++) {
				String id = getIdWithLevel(chainages[0], levelIndex, branchDefinition);

				double[] levelFrictionValues = new double[chainages.length];
				for (int chainageIndex = 0; chainageIndex < chainages.length; chainageIndex++) {
					levelFrictionValues[chainageIndex] = frictionValues[chainageIndex][levelIndex];
				}

				exchangeItems.put(id, new DFlowFMSpatialRoughnessExchangeItem(
					id, branchDefinition, levelFrictionValues,
					0, chainages.length, levels, chainages, levelIndex));
			}
			return line;
		}
		createExchangeItemsBasedOnSplit(branchDefinition, observationPointsList, chainages, levels, frictionValues);
		return line;
	}

	private double[] getLevels(BufferedReader lineReader) throws IOException {
		int numLevels = Integer.parseInt(readKeyValueLine(lineReader.readLine())[1]);

		String[] splitLevels = readKeyValueLine(lineReader.readLine())[1].trim().split(" ");
		assert splitLevels.length == numLevels;
		double[] levels = new double[numLevels];
		for (int i = 0; i < numLevels; i++) {
			levels[i] = Double.parseDouble(splitLevels[i]);
		}
		return levels;
	}

	private double[] getChainages(BufferedReader lineReader) throws IOException {
		int numLocations = Integer.parseInt(readKeyValueLine(lineReader.readLine())[1]);

		String[] splitChainage = readKeyValueLine(lineReader.readLine())[1].trim().split(" ");
		assert splitChainage.length == numLocations;
		double[] chainages = new double[numLocations];
		for (int i = 0; i < numLocations; i++) {
			chainages[i] = Double.parseDouble(splitChainage[i]);
		}
		return chainages;
	}

	private String fillFrictionValues(BufferedReader lineReader, double[][] frictionValues) throws IOException {
		int index = 0;
		String value = readKeyValueLine(lineReader.readLine())[1];
		while (value != null && !value.isEmpty() && !value.trim().equalsIgnoreCase(BRANCH)) {
			String[] splitFrictionValues = value.trim().split(" ");
			for (int i = 0; i < splitFrictionValues.length; i++) {
				String splitFrictionValue = splitFrictionValues[i];
				frictionValues[index][i] = Double.parseDouble(splitFrictionValue);
			}
			index++;
			value = lineReader.readLine();
		}
		return value;
	}

	private void createExchangeItemsBasedOnSplit(BranchDefinition branchDefinition, List<ObservationPoint> observationPointsList, double[] branchChainages, double[] levels, double[][] frictionValues) {
		ArrayList<Integer> chainageSplitIndices = getChainageSplitIndices(observationPointsList, branchChainages);
		int previousSplit = 0;
		for (int chainageSplitIndex : chainageSplitIndices) {
			createExchangeItemsForSegment(branchDefinition, branchChainages, levels, frictionValues, previousSplit, chainageSplitIndex, chainageSplitIndex);
			previousSplit = chainageSplitIndex;
		}
		createExchangeItemsForSegment(branchDefinition, branchChainages, levels, frictionValues, previousSplit, branchChainages.length, branchChainages.length);
	}

	private void createExchangeItemsForSegment(BranchDefinition branchDefinition, double[] branchChainages, double[] levels, double[][] frictionValues, int startIndex, int copyToIndex, int exchangeItemEndIndex) {
		for (int levelIndex = 0; levelIndex < levels.length; levelIndex++) {
			String id = getIdWithLevel(branchChainages[startIndex], levelIndex, branchDefinition);

			double[] segmentFrictionValues = new double[copyToIndex - startIndex];
			for (int chainageIndex = startIndex; chainageIndex < copyToIndex; chainageIndex++) {
				segmentFrictionValues[chainageIndex - startIndex] = frictionValues[chainageIndex][levelIndex];
			}

			exchangeItems.put(id, new DFlowFMSpatialRoughnessExchangeItem(
				id, branchDefinition, segmentFrictionValues,
				startIndex, exchangeItemEndIndex, levels, branchChainages, levelIndex));
		}
	}

	private String createConstantExchangeItems(BufferedReader lineReader, BranchDefinition branchDefinition, List<ObservationPoint> observationPointsList) throws IOException {
		String[] keyValue;
		String line;
		line = lineReader.readLine();
		keyValue = readKeyValueLine(line);
		int numLocations = Integer.parseInt(keyValue[1]);

		line = lineReader.readLine();
		keyValue = readKeyValueLine(line);
		String[] splitChainage = keyValue[1].trim().split(" ");
		assert splitChainage.length == numLocations;
		double[] branchChainages = new double[numLocations];
		for (int i = 0; i < numLocations; i++) {
			branchChainages[i] = Double.parseDouble(splitChainage[i]);
		}

		line = lineReader.readLine();
		keyValue = readKeyValueLine(line);

		double[] frictionValues = new double[numLocations];
		String[] splitFrictionValues = keyValue[1].trim().split(" ");
		for (int i = 0; i < splitFrictionValues.length; i++) {
			String splitFrictionValue = splitFrictionValues[i];
			frictionValues[i] = Double.parseDouble(splitFrictionValue);
		}

		if (observationPointsList == null) {
			String id = getIdWithoutLevel(branchChainages[0], branchDefinition);
			exchangeItems.put(id, new DFlowFMSpatialRoughnessExchangeItem(id, branchDefinition, frictionValues, 0, numLocations, null, branchChainages, 0));
			return line;
		}
		createConstantExchangeItemsBasedOnSplit(branchDefinition, observationPointsList, branchChainages, frictionValues);
		return line;
	}

	private void createConstantExchangeItemsBasedOnSplit(BranchDefinition branchDefinition, List<ObservationPoint> observationPointsList, double[] branchChainages, double[] frictionValues) {
		ArrayList<Integer> chainageSplitIndices = getChainageSplitIndices(observationPointsList, branchChainages);
		int previousSplit = 0;
		for (int chainageSplitIndex : chainageSplitIndices) {
			String id = getIdWithoutLevel(branchChainages[previousSplit], branchDefinition);
			double[] segmentFrictionValues = Arrays.copyOfRange(frictionValues, previousSplit, chainageSplitIndex);

			exchangeItems.put(id, new DFlowFMSpatialRoughnessExchangeItem(id, branchDefinition, segmentFrictionValues, previousSplit, chainageSplitIndex, null, branchChainages, 0));
			previousSplit = chainageSplitIndex;
		}
		String id = getIdWithoutLevel(branchChainages[previousSplit], branchDefinition);
		double[] segmentFrictionValues = Arrays.copyOfRange(frictionValues, previousSplit, branchChainages.length);
		exchangeItems.put(id, new DFlowFMSpatialRoughnessExchangeItem(id, branchDefinition, segmentFrictionValues, previousSplit, branchChainages.length, null, branchChainages, 0));
	}

	private ArrayList<Integer> getChainageSplitIndices(List<ObservationPoint> observationPointsList, double[] branchChainages) {
		ArrayList<Integer> chainageSplitIndices = new ArrayList<>();
		for (ObservationPoint observationPoint : observationPointsList) {
			int chainageSplit = findChainageSplit(branchChainages, observationPoint.getChainage());
			if (chainageSplit == -1) continue;
			chainageSplitIndices.add(chainageSplit);
		}
		return chainageSplitIndices;
	}

	private int findChainageSplit(double[] branchChainages, double observationChainage) {
		int size = branchChainages.length;
		for (int i = 0; i < size; i++) {
			double chainage = branchChainages[i];
			if (chainage == observationChainage) return i;
			if (chainage > observationChainage) return i - 1;
		}
		return size - 1;
	}

	private String getLevelString(String functionType, int levelIndex) {
		switch (functionType.toLowerCase()) {
			case "absdischarge":
				return "q" + levelIndex;
			case "waterlevel":
				return "h" + levelIndex;
			case "constant":
				return "";
			default:
				throw new RuntimeException(String.format("Unknown function type %s, should be absDischarge, waterLevel, or Constant", functionType));
		}
	}

	private String getIdWithLevel(double firstChainage, int levelIndex, BranchDefinition branchDefinition) {
		return frictionId +
			'-' +
			branchDefinition.getFrictionType() +
			'-' +
			branchDefinition.getBranchId() +
			'-' +
			"x" +
			Math.round(firstChainage) +
			'-' +
			getLevelString(branchDefinition.getFunctionType(), levelIndex);
	}

	private String getIdWithoutLevel(double firstChainage, BranchDefinition branchDefinition) {
		return frictionId +
			'-' +
			branchDefinition.getFrictionType() +
			'-' +
			branchDefinition.getBranchId() +
			'-' +
			"x" +
			Math.round(firstChainage);
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

		return new DFlowFMSpatialRoughnessExchangeItem(mainExchangeItemId, new BranchDefinition(frictionId, globalFrictionType, null), new double[]{globalFrictionValue}, -1, -1, null, null, 0);
	}

	private String[] readKeyValueLine(String line) {
		String[] split = line.split("=");
		assert split.length == 2;
		split[0] = split[0].trim();
		split[1] = split[1].trim();
		return split;
	}

	static class ObservationPoint implements Comparable<ObservationPoint> {
		private final String branchId;
		private final double chainage;

		public ObservationPoint(String branchId, double chainage) {
			this.branchId = branchId;
			this.chainage = chainage;
		}

		public String getBranchId() {
			return branchId;
		}

		public double getChainage() {
			return chainage;
		}

		@Override
		public int compareTo(ObservationPoint other) {
			int compareTo = branchId.compareTo(other.branchId);
			if (compareTo != 0) return compareTo;
			return Double.compare(chainage, other.chainage);
		}
	}
}
