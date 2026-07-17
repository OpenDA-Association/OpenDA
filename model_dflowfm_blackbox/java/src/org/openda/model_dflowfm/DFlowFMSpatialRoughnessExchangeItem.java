package org.openda.model_dflowfm;

import org.openda.exchange.QuantityInfo;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;

public class DFlowFMSpatialRoughnessExchangeItem implements IExchangeItem {

	private final String id;
	private final DFlowFMSpatialRoughnessFile.BranchDefinition branchDefinition;
	private final double[] chainages;
	private final int levelIndex;
	private final int chainageStartIndex;
	private final int chainageEndIndex;
	private final double[] levels;
	private double[] values;

	public DFlowFMSpatialRoughnessExchangeItem(String id, DFlowFMSpatialRoughnessFile.BranchDefinition branchDefinition, double[] values, int chainageStartIndex, int chainageEndIndex, double[] levels, double[] chainages, int levelIndex) {
		this.id = id;
		this.branchDefinition = branchDefinition;
		this.values = values;
		this.chainageStartIndex = chainageStartIndex;
		this.chainageEndIndex = chainageEndIndex;
		this.levels = levels;
		this.chainages = chainages;
		this.levelIndex = levelIndex;
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
		return new QuantityInfo("Roughness-" + branchDefinition.getFrictionType(), "");
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

	public int getChainageStartIndex() {
		return chainageStartIndex;
	}

	public int getChainageEndIndex() {
		return chainageEndIndex;
	}


	public DFlowFMSpatialRoughnessFile.BranchDefinition getBranchDefinition() {
		return branchDefinition;
	}

	public double[] getLevels() {
		return levels;
	}

	public double[] getChainages() {
		return chainages;
	}

	public int getLevelIndex() {
		return levelIndex;
	}
}
