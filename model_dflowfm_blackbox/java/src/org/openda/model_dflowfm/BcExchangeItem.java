package org.openda.model_dflowfm;

import org.openda.exchange.TimeInfo;
import org.openda.interfaces.*;
import org.openda.utils.Vector;

/**
 * Created by prevel on 20-Nov-15.
 */
public class BcExchangeItem implements IExchangeItem
{
	private String id;
	private double[] valueData;
	private ITimeInfo timeInfo;

	public BcExchangeItem(String id, double[] timeSeriesData, double[] valueData)
	{
		this.id = id;
		this.valueData = valueData;
		timeInfo = new TimeInfo(timeSeriesData);
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
		return id;
	}

	@Override
	public Class getValueType() { return double[].class; }

	@Override
	public void copyValuesFromItem(IExchangeItem sourceItem) { setValues(sourceItem.getValues()); }

	@Override
	public ITimeInfo getTimeInfo() { return timeInfo; }

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
	public Object getValues() {	return valueData; }

	@Override
	public double[] getValuesAsDoubles() { return valueData; }

	@Override
	public void axpyOnValues(double alpha, double[] axpyValues)
	{
		Vector valuesVector = new Vector(valueData);
		valuesVector.axpy(alpha, new Vector(axpyValues));
		valueData = valuesVector.getValues();
	}

	@Override
	public void multiplyValues(double[] multiplicationFactors)
	{
		Vector valuesVector = new Vector(valueData);
		valuesVector.pointwiseMultiply(new Vector(multiplicationFactors));
		valueData = valuesVector.getValues();
	}

	@Override
	public void setValues(Object values)
	{
		if (values instanceof double[]) valueData = (double[])values;
		else throw new RuntimeException("Values must be of type double[]");
	}

	@Override
	public void setValuesAsDoubles(double[] values) { valueData = values; }

	@Override
	public double[] getTimes() { return timeInfo.getTimes(); }

	@Override
	public void setTimes(double[] times) { timeInfo = new TimeInfo(times); }
}
