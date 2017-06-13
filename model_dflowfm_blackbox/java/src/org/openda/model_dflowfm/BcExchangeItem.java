/* OpenDA v2.4 
* Copyright (c) 2017 OpenDA Association 
* All rights reserved.
* 
* This file is part of OpenDA. 
* 
* OpenDA is free software: you can redistribute it and/or modify 
* it under the terms of the GNU Lesser General Public License as 
* published by the Free Software Foundation, either version 3 of 
* the License, or (at your option) any later version. 
* 
* OpenDA is distributed in the hope that it will be useful, 
* but WITHOUT ANY WARRANTY; without even the implied warranty of 
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
* GNU Lesser General Public License for more details. 
* 
* You should have received a copy of the GNU Lesser General Public License
* along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.
*/
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
