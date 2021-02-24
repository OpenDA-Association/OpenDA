/*
* Copyright (c) 2021 OpenDA Association 
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
package org.openda.model_metaswap;
import org.openda.interfaces.*;

import java.util.Arrays;

public class SwapStateExchangeItem implements IExchangeItem {

	private final String id;
	private double[] values;

	public SwapStateExchangeItem(String id, double[] values) {
		this.id = id;
		this.values = values;
	}

	@Override
	public IExchangeItem.Role getRole() {
		return null;
	}


	@Override
	public double[] getValuesAsDoubles() {
		if (values == null) return null;
		return Arrays.copyOf(values, values.length);
	}

	@Override
	public void axpyOnValues(double alpha, double[] axpyValues) {
		if (this.values != null) {
			for (int i = 0; i < values.length; i++) {
				values[i] += alpha * axpyValues[i];
			}
		}
	}

	@Override
	public void multiplyValues(double[] multiplicationFactors) {
		if (this.values != null) {
			for (int i = 0; i < values.length; i++) {
				values[i] *= multiplicationFactors[i];
			}
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
		return null;
	}

	@Override
	public void setTimes(double[] times) {

	}

	@Override
	public String getId() {
		return id;
	}

	@Override
	public String getDescription() {
		return null;
	}

	@Override
	public void copyValuesFromItem(IExchangeItem sourceItem) {

	}

	@Override
	public ITimeInfo getTimeInfo() {
		return null;
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
	public IExchangeItem.ValueType getValuesType() {
		return null;
	}

	@Override
	public Object getValues() {
		return null;
	}

}
