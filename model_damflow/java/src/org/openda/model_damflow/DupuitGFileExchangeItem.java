package org.openda.model_damflow;

import org.openda.interfaces.*;

/**
 * Created by IntelliJ IDEA.
 * User: Julius Sumihar
 * Date: 4-7-12
 * Time: 14:04
 * To change this template use File | Settings | File Templates.
 */
public class DupuitGFileExchangeItem implements IExchangeItem {
	private String id;
	private double param;

	public DupuitGFileExchangeItem(String id, double param) {
		this.id = id;
		this.param = param;
	}

	@Override
	public Role getRole() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFileExchangeItem - Method Name : getRole");
	}

	@Override
	public String getId() {
		return this.id;
	}

	@Override
	public String getDescription() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFileExchangeItem - Method Name : getDescription");
	}

	@Override
	public Class getValueType() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFileExchangeItem - Method Name : getValueType");
	}

	@Override
	public void copyValuesFromItem(IExchangeItem sourceItem) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFileExchangeItem - Method Name : copyValuesFromItem");
	}

	@Override
	public ITimeInfo getTimeInfo() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFileExchangeItem - Method Name : getTimeInfo");
	}

	@Override
	public IQuantityInfo getQuantityInfo() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFileExchangeItem - Method Name : getQuantityInfo");
	}

	@Override
	public IGeometryInfo getGeometryInfo() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFileExchangeItem - Method Name : getGeometryInfo");
	}

	@Override
	public ValueType getValuesType() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFileExchangeItem - Method Name : getValuesType");
	}

	@Override
	public Object getValues() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFileExchangeItem - Method Name : getValues");
	}

	@Override
	public double[] getValuesAsDoubles() {
		double[] values = new double[]{this.param};
		return values;
	}

	@Override
	public void axpyOnValues(double alpha, double[] axpyValues) {
		if (axpyValues.length!=1) {
			throw new RuntimeException(this.getClass() + ": vectors of different size, cannot perform axpyOnValues.");
		}
		this.param = alpha * axpyValues[0] + this.param;
	}

	@Override
	public void multiplyValues(double[] multiplicationFactors) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFileExchangeItem - Method Name : multiplyValues");
	}

	@Override
	public void setValues(Object values) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFileExchangeItem - Method Name : setValues");
	}

	@Override
	public void setValuesAsDoubles(double[] values) {
		double[] newParam = new double[1];
		System.arraycopy(values,0,newParam,0,values.length);
		this.param = newParam[0];
		}

	@Override
	public double[] getTimes() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFileExchangeItem - Method Name : getTimes");
	}

	@Override
	public void setTimes(double[] times) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFileExchangeItem - Method Name : setTimes");
	}
}
