package org.openda.model_wflow;

import org.openda.exchange.NetcdfGridTimeSeriesExchangeItem;
import org.openda.interfaces.*;

public class WflowJuliaStateExchangeItem implements IGridTimeSeriesExchangeItem {
	private final NetcdfGridTimeSeriesExchangeItem exchangeItem;

	public WflowJuliaStateExchangeItem(NetcdfGridTimeSeriesExchangeItem exchangeItem) {

		this.exchangeItem = exchangeItem;
	}

	@Override
	public Role getRole() {
		return exchangeItem.getRole();
	}

	@Override
	public String getId() {
		return exchangeItem.getId();
	}

	@Override
	public String getDescription() {
		return exchangeItem.getDescription();
	}

	@Override
	public void copyValuesFromItem(IExchangeItem sourceItem) {
		exchangeItem.copyValuesFromItem(sourceItem);
	}

	@Override
	public ITimeInfo getTimeInfo() {
		return exchangeItem.getTimeInfo();
	}

	@Override
	public IQuantityInfo getQuantityInfo() {
		return exchangeItem.getQuantityInfo();
	}

	@Override
	public IGeometryInfo getGeometryInfo() {
		return exchangeItem.getGeometryInfo();
	}

	@Override
	public ValueType getValuesType() {
		return ValueType.doublesType;
	}

	@Override
	public Object getValues() {
		return exchangeItem.getValuesAsDoublesForSingleTimeIndex(0);
	}

	@Override
	public double[] getValuesAsDoubles() {
		return exchangeItem.getValuesAsDoublesForSingleTimeIndex(0);
	}

	@Override
	public void axpyOnValues(double alpha, double[] axpyValues) {
		exchangeItem.axpyOnValuesForSingleTimeIndex(0, alpha, axpyValues);
	}

	@Override
	public void multiplyValues(double[] multiplicationFactors) {
		exchangeItem.multiplyValues(multiplicationFactors);
	}

	@Override
	public void setValues(Object values) {
		exchangeItem.setValues(values);
	}

	@Override
	public void setValuesAsDoubles(double[] values) {
		exchangeItem.setValuesAsDoubles(values);
	}

	@Override
	public double[] getTimes() {
		return exchangeItem.getTimes();
	}

	@Override
	public void setTimes(double[] times) {
		exchangeItem.setTimes(times);
	}

	@Override
	public IGeometryInfo getGeometryInfoForSingleTimeIndex(int timeIndex) {
		return exchangeItem.getGeometryInfoForSingleTimeIndex(timeIndex);
	}

	@Override
	public double[] getValuesAsDoublesForSingleTimeIndex(int timeIndex) {
		return exchangeItem.getValuesAsDoublesForSingleTimeIndex(timeIndex);
	}

	@Override
	public void setValuesAsDoublesForSingleTimeIndex(int timeIndex, double[] values) {
		exchangeItem.setValuesAsDoublesForSingleTimeIndex(timeIndex, values);
	}

	@Override
	public void axpyOnValuesForSingleTimeIndex(int timeIndex, double alpha, double[] axpyValues) {
		exchangeItem.axpyOnValuesForSingleTimeIndex(timeIndex, alpha, axpyValues);
	}

	@Override
	public void multiplyValuesForSingleTimeIndex(int timeIndex, double[] multiplicationFactors) {
		exchangeItem.multiplyValuesForSingleTimeIndex(timeIndex, multiplicationFactors);
	}
}
