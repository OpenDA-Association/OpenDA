package org.openda.model_wanda_seawat;

import org.openda.exchange.TimeInfo;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;

import java.util.Arrays;

public class WandaSeawatConstantAsTimeSeriesExchangeItem implements IExchangeItem {
	private final String id;
	private final double initialValue;
	private final TimeInfo timeInfo;
	private double[] values;

	public WandaSeawatConstantAsTimeSeriesExchangeItem(String id, double initialValue, double timeStepInSeconds, double startDateTime, double endDateTime) {
		this.id = id;
		this.initialValue = initialValue;
		int numberOfSteps = (int) (1 + Math.round((endDateTime - startDateTime) * 24 * 3600 / timeStepInSeconds));
		values = new double[numberOfSteps];
		Arrays.fill(values, initialValue);

		double[] times = new double[numberOfSteps];
		for (int i = 0; i < numberOfSteps; i++) {
			times[i] = startDateTime + (i * timeStepInSeconds) / (24 * 3600);
		}
		timeInfo = new TimeInfo(times);
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
		return ValueType.doublesType;
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
		for (int i = 0; i < values.length; i++) {
			values[i] += alpha * axpyValues[i];
		}
	}

	@Override
	public void multiplyValues(double[] multiplicationFactors) {
		for (int i = 0; i < values.length; i++) {
			// TODO EP: make the use of initial values instead of the current values optional
			values[i] = initialValue * multiplicationFactors[i];
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
		return timeInfo.getTimes();
	}

	@Override
	public void setTimes(double[] times) {
		timeInfo.setTimes(times);
	}
}
