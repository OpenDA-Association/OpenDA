package org.openda.model_hec_hms;

import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;

public class StateFileExchangeItem implements IExchangeItem {
	private final String gridId;
	private final int x;
	private final int y;
	private final int width;
	private final int height;
	private double[] values;

	public StateFileExchangeItem(String gridId, int x, int y, int width, int height, double[] values) {
		this.gridId = gridId;
		this.x = x;
		this.y = y;
		this.width = width;
		this.height = height;
		this.values = values;
	}

	@Override
	public Role getRole() {
		throw new UnsupportedOperationException("Not implemented yet");
	}

	@Override
	public String getId() {
		return gridId;
	}

	@Override
	public String getDescription() {
		throw new UnsupportedOperationException("Not implemented yet");
	}

	@Override
	public void copyValuesFromItem(IExchangeItem sourceItem) {
		throw new UnsupportedOperationException("Not implemented yet");
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
		return IExchangeItem.ValueType.IVectorType;
	}

	@Override
	public Object getValues() {
		return getValuesAsDoubles();
	}

	@Override
	public double[] getValuesAsDoubles() {
		return values;
	}

	@Override
	public void axpyOnValues(double alpha, double[] axpyValues) {
		throw new UnsupportedOperationException("Not implemented yet");
	}

	@Override
	public void multiplyValues(double[] multiplicationFactors) {
		throw new UnsupportedOperationException("Not implemented yet");
	}

	@Override
	public void setValues(Object values) {
		if (!(values instanceof double[])) {
			throw new IllegalArgumentException("Values must be of type double[]");
		}

		double[] valuesArray = (double[]) values;

		int expectedLength = width * height;

		if (expectedLength != valuesArray.length) {
			throw new IllegalArgumentException("The number of values: " + valuesArray.length + " is not equal to the expected size of the current array: " + expectedLength);
		}

		this.values = valuesArray;
	}

	@Override
	public void setValuesAsDoubles(double[] values) {
		int expectedLength = width * height;

		if (expectedLength != values.length) {
			throw new IllegalArgumentException("The number of values: " + values.length + " is not equal to the expected size of the current array: " + expectedLength);
		}

		this.values = values;
	}

	@Override
	public double[] getTimes() {
		throw new UnsupportedOperationException("Not implemented yet");
	}

	@Override
	public void setTimes(double[] times) {
		throw new UnsupportedOperationException("Not implemented yet");
	}

	public int getX() {
		return x;
	}

	public int getY() {
		return y;
	}

	public int getWidth() {
		return width;
	}

	public int getHeight() {
		return height;
	}
}
