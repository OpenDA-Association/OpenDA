/* OpenDA v2.4.1 
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
import org.openda.interfaces.*;

/**
 * Exchange item for time time items in FLOW 1D's md1d-file
 */
public class Flow1DTimeInfoExchangeItem implements IExchangeItem
{
	public enum PropertyId
	{
		StartTime,
		StopTime,
		TimeStep,
		OutputTimeStep
	}

	private PropertyId id;
	private double value;

	public Flow1DTimeInfoExchangeItem(PropertyId id, double value)
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

		return id.name();
	}

	@Override
	public String getDescription() { return id.name(); }

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
		if (values instanceof Double) setValuesAsDoubles(new double[]{(Double)values});
		else throw new RuntimeException(String.format("Values must be of type %s", ValueType.doubleType.name()));
	}

	@Override
	public void setValuesAsDoubles(double[] values)
	{
		if (id == PropertyId.TimeStep) throw new RuntimeException(String.format("Setting of property %s is not allowed", id.name())); // TimeStep may be retrieved but not set
		value = values[0];
	}

	@Override
	public double[] getTimes() { throw new UnsupportedOperationException("org.openda.model_DFlowFM.Md1dTimeInfoExchangeItem.getTimes(): Not implemented yet."); }

	@Override
	public void setTimes(double[] times) { throw new UnsupportedOperationException("org.openda.model_DFlowFM.Md1dTimeInfoExchangeItem.setTimes(): Not implemented yet."); }
}
