package org.openda.model_hec_hms;

import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;

public class ControlFileExchangeItem implements IExchangeItem {
	@Override
	public Role getRole() {
		return null;
	}

	@Override
	public String getId() {
		return "";
	}

	@Override
	public String getDescription() {
		return "";
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

	@Override
	public double[] getValuesAsDoubles() {
		return new double[0];
	}

	@Override
	public void axpyOnValues(double alpha, double[] axpyValues) {

	}

	@Override
	public void multiplyValues(double[] multiplicationFactors) {

	}

	@Override
	public void setValues(Object values) {

	}

	@Override
	public void setValuesAsDoubles(double[] values) {

	}

	@Override
	public double[] getTimes() {
		return new double[0];
	}

	@Override
	public void setTimes(double[] times) {

	}
}
