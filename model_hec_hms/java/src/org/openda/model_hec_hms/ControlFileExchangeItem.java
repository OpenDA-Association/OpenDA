package org.openda.model_hec_hms;

import org.openda.exchange.TimeInfo;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;

public class ControlFileExchangeItem implements IExchangeItem {
	private static final String EXPECTED_START_AND_END_TIME_BUT_FOUND_X_TIMES = "Expected start and end time but found %d times";
	private static final int EXPECTED_NUMBER_OF_TIMES = 2;

	private final String id;
	private final TimeInfo timeInfo = new TimeInfo();

	public ControlFileExchangeItem(String id, double[] times) {
		this.id = id;

		if (EXPECTED_NUMBER_OF_TIMES != times.length) {
			String message = String.format(EXPECTED_START_AND_END_TIME_BUT_FOUND_X_TIMES, times.length);
			throw new IllegalArgumentException(message);
		}

		timeInfo.setTimes(times);
	}

	@Override
	public Role getRole() {
		throw new UnsupportedOperationException("Not implemented yet");
	}

	@Override
	public String getId() {
		return id;
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
		return timeInfo;
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
		return ValueType.doubleType;
	}

	@Override
	public Object getValues() {
		return timeInfo.getTimes();
	}

	@Override
	public double[] getValuesAsDoubles() {
		return timeInfo.getTimes();
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
			String message = String.format("Unexpected type for values: %s", values.getClass().getName());
			throw new IllegalArgumentException(message);
		}

		setValuesAsDoubles((double[]) values);
	}

	@Override
	public void setValuesAsDoubles(double[] values) {
		if (EXPECTED_NUMBER_OF_TIMES != values.length) {
			String message = String.format(EXPECTED_START_AND_END_TIME_BUT_FOUND_X_TIMES, values.length);
			throw new IllegalArgumentException(message);
		}

		timeInfo.setTimes(values);
	}

	@Override
	public double[] getTimes() {
		return timeInfo.getTimes();
	}

	@Override
	public void setTimes(double[] times) {
		if (EXPECTED_NUMBER_OF_TIMES != times.length) {
			String message = String.format(EXPECTED_START_AND_END_TIME_BUT_FOUND_X_TIMES, times.length);
			throw new IllegalArgumentException(message);
		}

		timeInfo.setTimes(times);
	}
}
