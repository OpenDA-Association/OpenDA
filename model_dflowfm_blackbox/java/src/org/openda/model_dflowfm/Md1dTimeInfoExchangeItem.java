package org.openda.model_dflowfm;

import org.openda.interfaces.*;

/**
 * Created by prevel on 30-Nov-15.
 */
public class Md1dTimeInfoExchangeItem implements IExchangeItem
{
	private String id;
	private double value;

	public Md1dTimeInfoExchangeItem(String id, double value)
	{
		this.id = id;
		this.value = value;
	}

	@Override
	public Role getRole() {
		return IPrevExchangeItem.Role.InOut;
	}

	@Override
	public String getId() {
		return id;
	}

	@Override
	public String getDescription() { return id; }

	@Override
	public Class getValueType() { return double.class; }

	@Override
	public void copyValuesFromItem(IExchangeItem sourceItem) { setValues(sourceItem.getValues()); }

	@Override
	public ITimeInfo getTimeInfo() { throw new UnsupportedOperationException("org.openda.model_DFlowFM.Md1dTimeInfoExchangeItem.getTimeInfo(): Not implemented yet."); }

	@Override
	public IQuantityInfo getQuantityInfo() { return null; }

	@Override
	public IGeometryInfo getGeometryInfo() { return null; }

	@Override
	public ValueType getValuesType() { return ValueType.doubleType;	}

	@Override
	public Object getValues() {	return value; }

	@Override
	public double[] getValuesAsDoubles() { return new double[]{(Double)getValues()}; }

	@Override
	public void axpyOnValues(double alpha, double[] axpyValues) { throw new UnsupportedOperationException("org.openda.model_DFlowFM.Md1dTimeInfoExchangeItem.axpyOnValues(): Not implemented yet.");	}

	@Override
	public void multiplyValues(double[] multiplicationFactors) { throw new UnsupportedOperationException("org.openda.model_DFlowFM.Md1dTimeInfoExchangeItem.multiplyValues(): Not implemented yet."); }

	@Override
	public void setValues(Object values)
	{
		if (values instanceof Double) value = (Double)values;
		else throw new RuntimeException("Values must be of type double");
	}

	@Override
	public void setValuesAsDoubles(double[] values) { value = values[0]; }

	@Override
	public double[] getTimes() { throw new UnsupportedOperationException("org.openda.model_DFlowFM.Md1dTimeInfoExchangeItem.getTimes(): Not implemented yet."); }

	@Override
	public void setTimes(double[] times) { throw new UnsupportedOperationException("org.openda.model_DFlowFM.Md1dTimeInfoExchangeItem.setTimes(): Not implemented yet."); }
}
