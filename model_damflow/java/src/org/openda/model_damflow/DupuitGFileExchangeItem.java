/* OpenDA v2.4.3 
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
package org.openda.model_damflow;
import org.openda.interfaces.*;

/**
 * Created by IntelliJ IDEA.
 * User: Julius Sumihar
 * Date: 4-7-12
 * Time: 14:04
 * To change this template use File | Settings | File Templates.
 */
public class DupuitGFileExchangeItem implements IExchangeItem {
	private String id;
	private double param;

	public DupuitGFileExchangeItem(String id, double param) {
		this.id = id;
		this.param = param;
	}

	
	public Role getRole() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFileExchangeItem - Method Name : getRole");
	}

	
	public String getId() {
		return this.id;
	}

	
	public String getDescription() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFileExchangeItem - Method Name : getDescription");
	}

	
	public Class getValueType() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFileExchangeItem - Method Name : getValueType");
	}

	
	public void copyValuesFromItem(IExchangeItem sourceItem) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFileExchangeItem - Method Name : copyValuesFromItem");
	}

	
	public ITimeInfo getTimeInfo() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFileExchangeItem - Method Name : getTimeInfo");
	}

	
	public IQuantityInfo getQuantityInfo() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFileExchangeItem - Method Name : getQuantityInfo");
	}

	
	public IGeometryInfo getGeometryInfo() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFileExchangeItem - Method Name : getGeometryInfo");
	}

	
	public ValueType getValuesType() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFileExchangeItem - Method Name : getValuesType");
	}

	
	public Object getValues() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFileExchangeItem - Method Name : getValues");
	}

	
	public double[] getValuesAsDoubles() {
		double[] values = new double[]{this.param};
		return values;
	}

	
	public void axpyOnValues(double alpha, double[] axpyValues) {
		if (axpyValues.length!=1) {
			throw new RuntimeException(this.getClass() + ": vectors of different size, cannot perform axpyOnValues.");
		}
		this.param = alpha * axpyValues[0] + this.param;
	}

	
	public void multiplyValues(double[] multiplicationFactors) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFileExchangeItem - Method Name : multiplyValues");
	}

	
	public void setValues(Object values) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFileExchangeItem - Method Name : setValues");
	}

	
	public void setValuesAsDoubles(double[] values) {
		double[] newParam = new double[1];
		System.arraycopy(values,0,newParam,0,values.length);
		this.param = newParam[0];
		}

	
	public double[] getTimes() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFileExchangeItem - Method Name : getTimes");
	}

	
	public void setTimes(double[] times) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitGFileExchangeItem - Method Name : setTimes");
	}
}
