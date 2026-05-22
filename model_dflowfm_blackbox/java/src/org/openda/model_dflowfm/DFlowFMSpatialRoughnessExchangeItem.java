package org.openda.model_dflowfm;

import org.openda.exchange.QuantityInfo;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;

public class DFlowFMSpatialRoughnessExchangeItem implements IExchangeItem {

	private final String id;
	private final String type;
	private final String branchId;
	private final String functionType;
	private double[] values;
	private final int chainageStartIndex;
	private final int chainageEndIndex;
	private final double level;

	public DFlowFMSpatialRoughnessExchangeItem(String id, String type, String branchId, String functionType, double[] values, int chainageStartIndex, int chainageEndIndex, double level) {
		this.id = id;
		this.type = type;
		this.branchId = branchId;
		this.functionType = functionType;
		this.values = values;
		this.chainageStartIndex = chainageStartIndex;
		this.chainageEndIndex = chainageEndIndex;
		this.level = level;
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

	public int getChainageStartIndex() {
		return chainageStartIndex;
	}

	public int getChainageEndIndex() {
		return chainageEndIndex;
	}


	public String getFunctionType() {
		return functionType;
	}

	public String getBranchId() {
		return branchId;
	}

	public String getRoughnessType() {
		return type;
	}

	public double getLevel() {
		return level;
	}
}
