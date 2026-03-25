package org.openda.model_dflowfm;

import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;

import java.util.List;

public class DFlowFMNetcdfSampleFileExchangeItem implements IExchangeItem {

	private final String id;
	private final String varName;
	private final List<Integer> indices;
	private final double[] eiValues;

	public DFlowFMNetcdfSampleFileExchangeItem(String id, String varName, List<Integer> indices, double[] eiValues) {
		this.id = id;
		this.varName = varName;
		this.indices = indices;
		this.eiValues = eiValues;
	}


	@Override
	public Role getRole() {
		return Role.InOut;
	}

	@Override
	public String getId() {
		return id;
	}

	@Override
	public String getDescription() {
		return "";
	}

	@Override
	public void copyValuesFromItem(IExchangeItem sourceItem) {
		if (sourceItem.getValuesType() != ValueType.doublesType) {
			throw new IllegalArgumentException(String.format("Expected sourceItem to have values of type %s, but got %s", ValueType.doublesType, sourceItem.getValuesType()));
		}
		double[] sourceValues = sourceItem.getValuesAsDoubles();
		System.arraycopy(sourceValues, 0, this.eiValues, 0, sourceValues.length);
	}

	@Override
	public ITimeInfo getTimeInfo() {
		return null;
	}

	@Override
	public IQuantityInfo getQuantityInfo() {
		return null;
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
	public Object getValues() {
		return eiValues;
	}

	@Override
	public double[] getValuesAsDoubles() {
		return eiValues;
	}

	@Override
	public void axpyOnValues(double alpha, double[] axpyValues) {
		for (int i = 0; i < eiValues.length; i++) {
			eiValues[i] += alpha * axpyValues[i];
		}
	}

	@Override
	public void multiplyValues(double[] multiplicationFactors) {
		for (int i = 0; i < eiValues.length; i++) {
			eiValues[i] *= multiplicationFactors[i];
		}
	}

	@Override
	public void setValues(Object values) {

	}

	@Override
	public void setValuesAsDoubles(double[] values) {
		System.arraycopy(values, 0, eiValues, 0, values.length);
	}

	@Override
	public double[] getTimes() {
		return new double[0];
	}

	@Override
	public void setTimes(double[] times) {

	}

	public List<Integer> getIndices() {
		return indices;
	}

	public String getVarName() {
		return varName;
	}
}
