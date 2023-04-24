package org.openda.model_zero_mq;

import org.openda.exchange.ArrayGeometryInfo;
import org.openda.exchange.QuantityInfo;
import org.openda.exchange.TimeInfo;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.*;
import org.openda.utils.Array;
import org.openda.utils.Vector;

import java.util.Arrays;

public class ZeroMqOutputExchangeItem implements IExchangeItem {
	private final String variableName;
	private final IExchangeItem.Role role;
	private final ZeroMqModelInstance model;
	private final IQuantityInfo quantityInfo;
	private final IGeometryInfo geometryInfo;
	private final double modelMissingValue;


	public ZeroMqOutputExchangeItem(String variable, IExchangeItem.Role input, ZeroMqModelInstance model, double modelMissingValue) {
		this.variableName = variable;
		role = input;
		this.model = model;
		this.modelMissingValue = modelMissingValue;
		this.quantityInfo = new QuantityInfo(variableName, model.getVarUnits(variableName));
		this.geometryInfo = createGeometryInfo();
	}

	private IGeometryInfo createGeometryInfo() {

		int varGrid = model.getVarGrid(variableName);
		// data in grid lower-to-higher latitudes (south to north)
		double[] latitudes = this.model.getGridY(varGrid);

		IArray latitudeArray = new Array(latitudes);

		double[] longitudes = this.model.getGridX(varGrid);

		IArray longitudeArray = new Array(longitudes);
		int[] latitudeValueIndices = new int[]{0};
		int[] longitudeValueIndices = new int[]{1};

		IQuantityInfo latitudeQuantityInfo = new QuantityInfo("y coordinate according to model coordinate system",
			"meter");
		IQuantityInfo longitudeQuantityInfo = new QuantityInfo("x coordinate according to model coordinate system",
			"meter");
		//here create a rectangular grid geometryInfo, otherwise GeometryUtils.getObservedValuesBilinearInterpolation does not work and GeometryUtils.getLocalizationWeights works slow.
		return new ArrayGeometryInfo(latitudeArray, latitudeValueIndices, latitudeQuantityInfo, longitudeArray,
			longitudeValueIndices, longitudeQuantityInfo, null, null, null, null);
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

	public Class<?> getValueType() {
		return IVector.class;
	}

	public Object getValues() {
		// TODO to use native code (to improve performance) for vector and
		// matrix calculations in algorithm, for that need to return a CtaVector
		// here. AK
		return new Vector(getValuesAsDoubles());
	}

	/**
	 * Returns only the current values, since the model only stores the current
	 * values in memory.
	 */
	public double[] getValuesAsDoubles() {
		double[] allModelValues = model.getValues(variableName);
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
			// for all NaNs results in NaN
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
