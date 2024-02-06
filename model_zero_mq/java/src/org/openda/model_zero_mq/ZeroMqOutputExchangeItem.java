package org.openda.model_zero_mq;

import org.openda.exchange.IrregularGridGeometryInfo;
import org.openda.exchange.QuantityInfo;
import org.openda.exchange.TimeInfo;
import org.openda.interfaces.*;
import org.openda.utils.Vector;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class ZeroMqOutputExchangeItem implements IExchangeItem {
	private final String variableName;
	private final IExchangeItem.Role role;
	private final ZeroMqModelInstance model;
	private final IQuantityInfo quantityInfo;
	private final IGeometryInfo geometryInfo;
	private final double modelMissingValue;
	private final double[] dummyValuesArray;


	public ZeroMqOutputExchangeItem(String variable, Role input, ZeroMqModelInstance model, double modelMissingValue, IrregularGridGeometryInfo irregularGridGeometryInfo, QuantityInfo quantityInfo) {
		this.variableName = variable;
		role = input;
		this.model = model;
		this.modelMissingValue = modelMissingValue;
		this.quantityInfo = quantityInfo;
		this.geometryInfo = irregularGridGeometryInfo;
		int bytesPerItem = this.model.getVarItemSize(variableName);
		int totalBytes = this.model.getVarNBytes(variableName);
		dummyValuesArray = new double[totalBytes / bytesPerItem];
	}

	public String getId() {
		return this.variableName;
	}

	public String getDescription() {
		return null;
	}

	public IExchangeItem.Role getRole() {
		return this.role;
	}

	public ITimeInfo getTimeInfo() {
		return new TimeInfo(getTimes());
	}

	public double[] getTimes() {
		return new double[]{model.getCurrentTime().getMJD()};
	}

	public void setTimes(double[] times) {
		throw new RuntimeException(this.getClass().getName() + ": setting time stamps not supported for BMI model.");
	}

	public IExchangeItem.ValueType getValuesType() {
		return IExchangeItem.ValueType.IVectorType;
	}

	public Object getValues() {
		return new Vector(getValuesAsDoubles());
	}

	/**
	 * Returns only the current values, since the model only stores the current
	 * values in memory.
	 */
	public double[] getValuesAsDoubles() {
		double[] allModelValues = model.getValues(variableName, dummyValuesArray);
		double[] checkedValues = new double[allModelValues.length];
		for (int i = 0; i < allModelValues.length; i++) {
			checkedValues[i] = Double.compare(allModelValues[i], modelMissingValue) == 0 ? Double.NaN : allModelValues[i];
		}
		return checkedValues;
	}

	/**
	 * Only changes the current values, since the model only stores the current
	 * values in memory. Only changes the values of the active grid cells.
	 */
	public void axpyOnValues(double alpha, double[] axpyValues) {
		double[] allValues = getValuesAsDoubles();

		for (int n = 0; n < allValues.length; n++) {
			allValues[n] += alpha * axpyValues[n];
		}
		setValuesAsDoubles(allValues);
	}

	/**
	 * Only changes the current values, since the model only stores the current
	 * values in memory. Only changes the values of the active grid cells.
	 */
	public void multiplyValues(double[] multiplicationFactors) {

		double[] allValues = getValuesAsDoubles();
		for (int n = 0; n < allValues.length; n++) {
			allValues[n] *= multiplicationFactors[n];
		}
		setValuesAsDoubles(allValues);
	}

	public void setValues(Object vector) {
		if (!(vector instanceof IVector)) {
			throw new IllegalArgumentException(this.getClass().getName() + ": supply values as an IVector not as "
				+ vector.getClass().getName());
		}

		setValuesAsDoubles(((IVector) vector).getValues());
	}

	/**
	 * Sets only the current values, since BMI models only store the current
	 * values in memory. Sets the values of all grid cells, also of the
	 * inactive/dry/dead grid cells. This is not a problem, since BMI models
	 * should ignore the values of the inactive grid cells anyway.
	 */
	public void setValuesAsDoubles(double[] values) {
		double[] checkedValues = new double[values.length];
		for (int i = 0; i < values.length; i++) {
			if (Double.isNaN(values[i])) {
				checkedValues[i] = modelMissingValue;
			} else
				checkedValues[i] = values[i];
		}
		model.setValue(variableName, checkedValues);
	}

	public void copyValuesFromItem(IExchangeItem sourceItem) {
		throw new UnsupportedOperationException(getClass().getName() + ".copyValuesFromItem() not implemented");
	}

	public IQuantityInfo getQuantityInfo() {
		return this.quantityInfo;
	}

	public IGeometryInfo getGeometryInfo() {
		return this.geometryInfo;
	}

}
