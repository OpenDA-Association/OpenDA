package org.openda.exchange;

import org.openda.exchange.dataobjects.GenericNetcdfDataObject;
import org.openda.interfaces.IExchangeItem;

public class GenericNetcdfArrayExchangeItem extends ExchangeItem implements IExchangeItem {

	private final GenericNetcdfDataObject genericNetcdfDataObject;

	public GenericNetcdfArrayExchangeItem(String exchangeItemId, GenericNetcdfDataObject genericNetcdfDataObject){
		super(exchangeItemId);
		this.genericNetcdfDataObject = genericNetcdfDataObject;
	}

	@Override
	public ValueType getValuesType() {
		throw new RuntimeException("org.openda.exchange.GenericNetcdfArrayExchangeItem.getValuesType not implemented yet");
	}

	@Override
	public Class getValueType() {
		throw new RuntimeException("org.openda.exchange.GenericNetcdfArrayExchangeItem.getValueType not implemented yet");
	}

	@Override
	public Object getValues() {
		throw new RuntimeException("org.openda.exchange.GenericNetcdfArrayExchangeItem.getValues not implemented yet");
	}

	@Override
	public double[] getValuesAsDoubles() {
		return this.genericNetcdfDataObject.getArrayExchangeItemValues(this.getId());
	}

	@Override
	public void axpyOnValues(double alpha, double[] axpyValues) {
		double[] values = getValuesAsDoubles();
		for (int i = 0; i < values.length; i++) {
			if (Double.isNaN(values[i])) {
				System.out.println("OrgVal Nan at " + i);
			}
			values[i] += alpha * axpyValues[i];
			if (Double.isNaN(values[i])) {
				System.out.println("ModVal Nan at " + i);
			}
		}
		setValuesAsDoubles(values);
	}

	@Override
	public void multiplyValues(double[] multiplicationFactors) {
		throw new RuntimeException("org.openda.exchange.GenericNetcdfArrayExchangeItem.multiplyValues not implemented yet");
	}

	@Override
	public void setValues(Object values) {
		throw new RuntimeException("org.openda.exchange.GenericNetcdfArrayExchangeItem.setValues not implemented yet");
	}

	@Override
	public void setValuesAsDoubles(double[] values) {
		this.genericNetcdfDataObject.setArrayExchangeItemValues(this.getId(), values);
	}
}
