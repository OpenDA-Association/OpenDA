package org.openda.model_dflowfm;

import org.openda.interfaces.*;

/**
 * Created by pelgrim on 21-Jun-17.
 */
public class DFlowFMCalibrationFactorExchangeItem implements IExchangeItem {
	private final String id;
	private int lineNumber;
	private double[] values;

	public DFlowFMCalibrationFactorExchangeItem(String id, double value, int lineNumber) {
		this.id = id;
		this.values = new double[]{value};
		this.lineNumber = lineNumber;
	}

	@Override
	public Role getRole() {
		throw new RuntimeException("org.openda.model_dflowfm.DFlowFMCalibrationFactorExchangeItem.getRole() not implemented yet");

	}

	@Override
	public String getId() {
		return id;
	}

	@Override
	public String getDescription() {
		throw new RuntimeException("org.openda.model_dflowfm.DFlowFMCalibrationFactorExchangeItem.getDescription() not implemented yet");

	}

	@Override
	public Class getValueType() {
		throw new RuntimeException("org.openda.model_dflowfm.DFlowFMCalibrationFactorExchangeItem.getValueType() not implemented yet");

	}

	@Override
	public void copyValuesFromItem(IExchangeItem sourceItem) {
		throw new RuntimeException("org.openda.model_dflowfm.DFlowFMCalibrationFactorExchangeItem.copyValuesFromItem() not implemented yet");

	}

	@Override
	public ITimeInfo getTimeInfo() {
		throw new RuntimeException("org.openda.model_dflowfm.DFlowFMCalibrationFactorExchangeItem.getTimeInfo() not implemented yet");

	}

	@Override
	public IQuantityInfo getQuantityInfo() {
		throw new RuntimeException("org.openda.model_dflowfm.DFlowFMCalibrationFactorExchangeItem.getQuantityInfo() not implemented yet");

	}

	@Override
	public IGeometryInfo getGeometryInfo() {
		throw new RuntimeException("org.openda.model_dflowfm.DFlowFMCalibrationFactorExchangeItem.getGeometryInfo() not implemented yet");

	}

	@Override
	public ValueType getValuesType() {
		return ValueType.doubleType;
	}

	@Override
	public Object getValues() {
		return values;
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

	@Override
	public double[] getTimes() {
		throw new RuntimeException("org.openda.model_dflowfm.DFlowFMCalibrationFactorExchangeItem.getTimes() not implemented yet");

	}

	@Override
	public void setTimes(double[] times) {
		throw new RuntimeException("org.openda.model_dflowfm.DFlowFMCalibrationFactorExchangeItem.setTimes() not implemented yet");

	}

	public int getLineNumber() {
		return lineNumber;
	}
}
