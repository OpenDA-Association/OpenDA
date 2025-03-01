package org.openda.model_delft3d;

import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;

public class D3dBctExchangeItem implements IExchangeItem {
	private final String id;
	private double[] values;
	private final double[] times;

	public D3dBctExchangeItem(String id, double[] values, double[] times) {
		this.id = id;
		this.values = values;
		this.times = times;
	}

	@Override
	public Role getRole() {
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
		this.values = (double[]) values;
	}

	@Override
	public void setValuesAsDoubles(double[] values) {
		this.values = values;
	}

	@Override
	public double[] getTimes() {
		return times;
	}

	@Override
	public void setTimes(double[] times) {

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
		return null;
	}

	@Override
	public Object getValues() {
		return null;
	}

}
