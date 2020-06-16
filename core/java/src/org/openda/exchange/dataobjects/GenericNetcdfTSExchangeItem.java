package org.openda.exchange.dataobjects;

import org.openda.interfaces.*;

public class GenericNetcdfTSExchangeItem implements IExchangeItem {
	private final String id;
	private final IArrayTimeInfo timeInfo;
	private final double[] values;

	public GenericNetcdfTSExchangeItem(String exchangeItemId, IArrayTimeInfo timeInfo, double[] values) {
		this.id = exchangeItemId;
		this.timeInfo = timeInfo;
		this.values = values;
	}

	@Override
	public Role getRole() {
		throw new RuntimeException("org.openda.exchange.dataobjects.GenericNetcdfTSExchangeItem.getRole not implemented yet");
	}

	@Override
	public String getId() {
		return this.id;
	}

	@Override
	public String getDescription() {
		throw new RuntimeException("org.openda.exchange.dataobjects.GenericNetcdfTSExchangeItem.getDescription not implemented yet");
	}

	@Override
	public Class getValueType() {
		throw new RuntimeException("org.openda.exchange.dataobjects.GenericNetcdfTSExchangeItem.getValueType not implemented yet");
	}

	@Override
	public void copyValuesFromItem(IExchangeItem sourceItem) {
		throw new RuntimeException("org.openda.exchange.dataobjects.GenericNetcdfTSExchangeItem.copyValuesFromItem not implemented yet");
	}

	@Override
	public ITimeInfo getTimeInfo() {
		return this.timeInfo;
	}

	@Override
	public IQuantityInfo getQuantityInfo() {
		throw new RuntimeException("org.openda.exchange.dataobjects.GenericNetcdfTSExchangeItem.getQuantityInfo not implemented yet");
	}

	@Override
	public IGeometryInfo getGeometryInfo() {
		throw new RuntimeException("org.openda.exchange.dataobjects.GenericNetcdfTSExchangeItem.getGeometryInfo not implemented yet");
	}

	@Override
	public ValueType getValuesType() {
		throw new RuntimeException("org.openda.exchange.dataobjects.GenericNetcdfTSExchangeItem.getValuesType not implemented yet");
	}

	@Override
	public Object getValues() {
		throw new RuntimeException("org.openda.exchange.dataobjects.GenericNetcdfTSExchangeItem.getValues not implemented yet");
	}

	@Override
	public double[] getValuesAsDoubles() {
		return this.values;
	}

	@Override
	public void axpyOnValues(double alpha, double[] axpyValues) {
		for (int i=0; i<values.length; i++){
			values[i] += alpha*axpyValues[i];
		}
	}

	@Override
	public void multiplyValues(double[] multiplicationFactors) {
		throw new RuntimeException("org.openda.exchange.dataobjects.GenericNetcdfTSExchangeItem.multiplyValues not implemented yet");
	}

	@Override
	public void setValues(Object values) {
		throw new RuntimeException("org.openda.exchange.dataobjects.GenericNetcdfTSExchangeItem.setValues not implemented yet");
	}

	@Override
	public void setValuesAsDoubles(double[] values) {
		throw new RuntimeException("org.openda.exchange.dataobjects.GenericNetcdfTSExchangeItem.setValuesAsDoubles not implemented yet");
	}

	@Override
	public double[] getTimes() {
		return timeInfo.getTimes();
	}

	@Override
	public void setTimes(double[] times) {
		throw new RuntimeException("org.openda.exchange.dataobjects.GenericNetcdfTSExchangeItem.setTimes not implemented yet");
	}
}
