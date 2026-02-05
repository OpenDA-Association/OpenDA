package org.openda.model_dflowfm;

import org.openda.exchange.QuantityInfo;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;

import java.util.*;

import static org.openda.model_dflowfm.DFlowFMSpatialRoughnessFile.REVERSED_POSTFIX;

public class DFlowFMSpatialRoughnessExchangeItem implements IExchangeItem {

	private String id;
	private String type;
	private double[] values;
	private final int chainageStartIndex;
	private final int chainageEndIndex;
	private InitialBranchDefinitions initialBranchDefinitions;

	public DFlowFMSpatialRoughnessExchangeItem(String id, String type, double[] values, int chainageStartIndex, int chainageEndIndex) {
		this.id = id;
		this.type = type;
		this.values = values;
		this.chainageStartIndex = chainageStartIndex;
		this.chainageEndIndex = chainageEndIndex;
	}

	@Override
	public String getId() {
		return id;
	}

	@Override
	public String getDescription() {
		return null;
	}

	@Override
	public void copyValuesFromItem(IExchangeItem sourceItem) {
		throw new RuntimeException("Method not implemented");
	}

	@Override
	public ITimeInfo getTimeInfo() {
		return null;
	}

	@Override
	public IQuantityInfo getQuantityInfo() {
		return new QuantityInfo("Roughness-" + type, "");
	}

	@Override
	public IGeometryInfo getGeometryInfo() {
		return null;
	}

	@Override
	public ValueType getValuesType() {
		return ValueType.doublesType;
	}

	@Override
	public Role getRole() {
		return null;
	}

	@Override
	public Object getValues() {
		return null;
	}

	@Override
	public double[] getValuesAsDoubles() {
		return values;
	}

	@Override
	public void axpyOnValues(double alpha, double[] axpyValues) {
		if (this.values != null) {
			for (int i = 0; i < values.length; i++) {
				values[i] += alpha * axpyValues[i];
			}
		}
	}

	@Override
	public void multiplyValues(double[] multiplicationFactors) {
		if (this.values != null) {
			for (int i = 0; i < values.length; i++) {
				values[i] *= multiplicationFactors[i];
			}
		}
	}

	@Override
	public void setValues(Object values) {

	}

	@Override
	public void setValuesAsDoubles(double[] values) {
		this.values = values;
	}

	public double[] getTimes() {
		return null;
	}

	public void setTimes(double[] times) {
		throw new RuntimeException(this.getClass().getName() + "setTimes(): time stamps can not be set");
	}

	public void setInitialBranchDefinitions(InitialBranchDefinitions initialBranchDefinitions) {
		this.initialBranchDefinitions = initialBranchDefinitions;
	}

	public InitialBranchDefinitions getInitialBranchDefinitions() {
		return initialBranchDefinitions;
	}

	public int getChainageStartIndex() {
		return chainageStartIndex;
	}

	public int getChainageEndIndex() {
		return chainageEndIndex;
	}


	//Used for collecting data for making exchange items en rewriting .ini file
	@SuppressWarnings("WeakerAccess")
	static class InitialBranchDefinitions {

		private String sectionId;
		private String branchId;
		private String roughnessType;
		private String functionType;
		private double[] levels;
		private List<Double> chainages;
		private List<double[]> listOfValueArrays;
		private Map<Double, double[]> chainageValuesMap = new TreeMap<>();
		private String flowDirection;

		public InitialBranchDefinitions() {}

		public List<DFlowFMSpatialRoughnessExchangeItem> createExchangeItems(Map<String, List<DFlowFMSpatialRoughnessFile.ObservationPoint>> observationPointsMap) {
			List<DFlowFMSpatialRoughnessExchangeItem> exchangeItems = new ArrayList<>();
			List<DFlowFMSpatialRoughnessFile.ObservationPoint> observationPointList = observationPointsMap.get(branchId);
			chainages = new ArrayList<>(chainageValuesMap.keySet());
			listOfValueArrays = new ArrayList<>(chainageValuesMap.values());

			if (observationPointList != null) {
				Collections.sort(observationPointList);
				createExchangeItemsForObservationPoints(exchangeItems, observationPointList);
			} else {
				double minChainage = getMinChainage();
				String firstChainage = "x" + String.valueOf(Math.round(minChainage));
				if (levels == null) {
					createExchangeItemsWithoutLevels(exchangeItems, 0, chainages.size() - 1, firstChainage);
				} else {
					createExchangeItemPerLevel(exchangeItems, 0, chainages.size() - 1, firstChainage);
				}
			}
			return exchangeItems;
		}

		private void createExchangeItemsForObservationPoints(List<DFlowFMSpatialRoughnessExchangeItem> list, List<DFlowFMSpatialRoughnessFile.ObservationPoint> observationPointList) {
			List<Integer> splitIndices = new ArrayList<>();
			splitIndices.add(0);
			for (int k = 0; k < observationPointList.size(); k++) {
				DFlowFMSpatialRoughnessFile.ObservationPoint observationPoint = observationPointList.get(k);
				int chainageSplitIndex = findChainageSplit(observationPoint);
				if (chainageSplitIndex <= 0) continue;
				splitIndices.add(chainageSplitIndex);
			}

			for (int k = 0, n = splitIndices.size(); k < n; k++) {
				int splitStartIndex = splitIndices.get(k);
				int splitEndIndex = k == n - 1 ? chainages.size() - 1 : splitIndices.get(k + 1) - 1;
				double minChainage = chainages.get(splitStartIndex);
				String firstChainage = "x" + String.valueOf(Math.round(minChainage));

				if (levels == null) {
					createExchangeItemsWithoutLevels(list, splitStartIndex, splitEndIndex, firstChainage);
				} else {
					createExchangeItemPerLevel(list, splitStartIndex, splitEndIndex, firstChainage);
				}
			}
		}

		private void createExchangeItemsWithoutLevels(List<DFlowFMSpatialRoughnessExchangeItem> list, int chainageStartIndex, int chainageEndIndex, String firstChainage) {
			String id = getIdWithoutLevel(firstChainage);
			int size = chainageEndIndex + 1 - chainageStartIndex;
			double[] values = new double[size];
			for (int j = chainageStartIndex; j <= chainageEndIndex; j++) {
				double[] listOfValueArray = listOfValueArrays.get(j);
				values[j - chainageStartIndex] = listOfValueArray[0];
			}
			DFlowFMSpatialRoughnessExchangeItem exchangeItem = new DFlowFMSpatialRoughnessExchangeItem(id, roughnessType, values, chainageStartIndex, chainageEndIndex);
			exchangeItem.setInitialBranchDefinitions(this);
			list.add(exchangeItem);
		}

		private String getIdWithoutLevel(String firstChainage) {
			StringBuilder noLevelIdBuilder = new StringBuilder(30);
			noLevelIdBuilder.append(sectionId);
			if (flowDirection.equalsIgnoreCase("true") || flowDirection.equals("1")) noLevelIdBuilder.append(REVERSED_POSTFIX);
			noLevelIdBuilder.append('-');
			noLevelIdBuilder.append(roughnessType);
			noLevelIdBuilder.append('-');
			noLevelIdBuilder.append(branchId);
			noLevelIdBuilder.append('-');
			noLevelIdBuilder.append(firstChainage);
			return noLevelIdBuilder.toString();
		}

		private void createExchangeItemPerLevel(List<DFlowFMSpatialRoughnessExchangeItem> list, int chainageStartIndex, int chainageEndIndex, String firstChainage) {
			for (int i = 0; i < levels.length; i++) {
				double level = levels[i];
				String id = getIdWithLevel(firstChainage, i + 1);
				double[] values = new double[chainageEndIndex + 1 - chainageStartIndex];
				for (int j = chainageStartIndex; j <= chainageEndIndex; j++) {
					double[] listOfValueArray = listOfValueArrays.get(j);
					values[j - chainageStartIndex] = listOfValueArray[i];
				}
				DFlowFMSpatialRoughnessExchangeItem exchangeItem = new DFlowFMSpatialRoughnessExchangeItem(id, roughnessType, values, chainageStartIndex, chainageEndIndex);
				exchangeItem.setInitialBranchDefinitions(this);
				list.add(exchangeItem);
			}
		}

		private String getIdWithLevel(String firstChainage, int levelIndex) {
			StringBuilder levelIdBuilder = new StringBuilder(30);
			levelIdBuilder.append(sectionId);
			if (flowDirection.equalsIgnoreCase("true") || flowDirection.equals("1")) levelIdBuilder.append(REVERSED_POSTFIX);
			levelIdBuilder.append('-');
			levelIdBuilder.append(roughnessType);
			levelIdBuilder.append('-');
			levelIdBuilder.append(branchId);
			levelIdBuilder.append('-');
			levelIdBuilder.append(firstChainage);
			levelIdBuilder.append('-');
			levelIdBuilder.append(functionType);
			levelIdBuilder.append(levelIndex);
			return levelIdBuilder.toString();
		}

		private int findChainageSplit(DFlowFMSpatialRoughnessFile.ObservationPoint observationPoint) {
			double chainageObservation = observationPoint.getChainage();
			int size = chainages.size();
			for (int i = 0; i < size; i++) {
				double chainage = chainages.get(i);
				if (chainage == chainageObservation) return i;
				if (chainage > chainageObservation) return i - 1;
			}
			return size - 1;
		}

		private double getMinChainage() {
			double minChainage = Double.MAX_VALUE;
			for (Double chainage : chainages) {
				if (chainage < minChainage) minChainage = chainage;
			}
			return minChainage;
		}

		public String getBranchId() {
			return branchId;
		}

		public String getRoughnessType() {
			return roughnessType;
		}

		public String getFunctionType() {
			return functionType;
		}

		public double[] getLevels() {
			return levels;
		}

		public List<Double> getChainages() {
			return chainages;
		}

		public void setFrictionId(String sectionId) {
			this.sectionId = sectionId;
		}

		public void setBranchId(String branchId) {
			this.branchId = branchId;
		}

		public void setFrictionType(String frictionType) {
			this.roughnessType = frictionType;
		}

		public void setLevels(double[] levels) {
			this.levels = levels;
		}

		public void setFunctionType(String functionType) {
			this.functionType = functionType;
		}

		public void setFlowDirection(String flowDirection) {
			this.flowDirection = flowDirection;
		}
	}

	//Used for collecting updated values of Definitions that will be written
	@SuppressWarnings("WeakerAccess")
	static class WriteDefinition {

		private String branchId;
		private double chainage;
		private List<Double> values = new ArrayList<>();

		public WriteDefinition(String branchId, Double chainage) {
			this.branchId = branchId;
			this.chainage = chainage;
		}

		public void addValue(double value) {
			this.values.add(value);
		}

		public String getBranchId() {
			return branchId;
		}

		public double getChainage() {
			return chainage;
		}

		public List<Double> getValues() {
			return values;
		}
	}
}
