/*
* Copyright (c) 2023 OpenDA Association 
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


package org.openda.dotnet;

import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;

/**
 * Java wrapper around .net class for an Exchange Item
 */
public class ExchangeItemN2J implements IExchangeItem {

	private cli.OpenDA.DotNet.Interfaces.IExchangeItem dotNotExchangeItem;

	public ExchangeItemN2J(cli.OpenDA.DotNet.Interfaces.IExchangeItem dotNotExchangeItem) {
		this.dotNotExchangeItem = dotNotExchangeItem;
	}

	public String getId() {
		return dotNotExchangeItem.get_Id();
	}

	public String getDescription() {
		return dotNotExchangeItem.get_Description();
	}

	public IQuantityInfo getQuantityInfo() { throw new UnsupportedOperationException("org.openda.dotnet.ExchangeItemN2J.getQuantityInfo(): Not implemented yet."); }

	public IGeometryInfo getGeometryInfo() { throw new UnsupportedOperationException("org.openda.dotnet.ExchangeItemN2J.getGeometryInfo(): Not implemented yet."); }

	public Class getValueType() {
		// for now, just always return an array of doubles, no matter wat the dotnet object's value type is
		return double[].class;
	}

	public ValueType getValuesType() {
		return ValueType.doublesType;
	}

	public Role getRole() { return Role.InOut; }

	public Object getValues() {
		// for now, just always return an array of doubles, no matter wat the dotnet object's value type is
		return dotNotExchangeItem.get_ValuesAsDoubles();
	}

	public double[] getValuesAsDoubles() {
		return dotNotExchangeItem.get_ValuesAsDoubles();
	}

	public void copyValuesFromItem(IExchangeItem sourceItem) {
		ValueType sourceType=sourceItem.getValuesType();
		if(sourceType != ValueType.doublesType){
			throw new RuntimeException("DummyAstroDataObject.DummyExchangeItem.setValues(): unknown type: " + sourceType );
		}
		this.setValues(sourceItem.getValues());
	}

	public void axpyOnValues(double alpha, double[] axpyValues) {
		dotNotExchangeItem.AxpyOnValues(alpha, axpyValues);
	}

	public void multiplyValues(double[] multiplicationFactors) {
		dotNotExchangeItem.MultiplyValues(multiplicationFactors);
	}

	public void setValues(Object values) {
		dotNotExchangeItem.set_Values(values);
	}

	public void setValuesAsDoubles(double[] values) {
		dotNotExchangeItem.set_ValuesAsDoubles(values);
	}

	public ITimeInfo getTimeInfo() {  throw new UnsupportedOperationException("org.openda.dotnet.ExchangeItemN2J.getTimeInfo(): Not implemented yet."); }

	public double[] getTimes() {
		return dotNotExchangeItem.get_Times();
	}

	public void setTimes(double[] times) {
		dotNotExchangeItem.set_Times(times);
	}
}
