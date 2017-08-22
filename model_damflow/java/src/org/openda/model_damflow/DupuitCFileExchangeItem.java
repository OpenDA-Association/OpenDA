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
package org.openda.model_damflow;
import org.openda.interfaces.*;

/**
 * Created by IntelliJ IDEA.
 * User: Julius Sumihar
 * Date: 2-7-12
 * Time: 10:24
 * To change this template use File | Settings | File Templates.
 */
public class DupuitCFileExchangeItem implements IExchangeItem {
	private String id;
	private DupuitCFile dupuitCFile;

	public DupuitCFileExchangeItem(String id, DupuitCFile dupuitCFile) {
		this.id = id;
		this.dupuitCFile = dupuitCFile;
	}

	
	public Role getRole() {
		return IPrevExchangeItem.Role.InOut;
	}

	
	public String getId() {
		return id;
	}

	
	public String getDescription() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitCFileExchangeItem - Method Name : getDescription");
	}

	
	public Class getValueType() {
		return double[].class;
	}

	
	public void copyValuesFromItem(IExchangeItem sourceItem) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitCFileExchangeItem - Method Name : copyValuesFromItem");
	}

	
	public ITimeInfo getTimeInfo() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitCFileExchangeItem - Method Name : getTimeInfo");
	}

	
	public IQuantityInfo getQuantityInfo() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitCFileExchangeItem - Method Name : getQuantityInfo");
	}

	
	public IGeometryInfo getGeometryInfo() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitCFileExchangeItem - Method Name : getGeometryInfo");
	}

	
	public ValueType getValuesType() {
		return ValueType.doublesType;
	}

	
	public Object getValues() {
		return getValuesAsDoubles();
	}

	
	public double[] getValuesAsDoubles() {
		return dupuitCFile.getHydraulicHead();
	}

	
	public void axpyOnValues(double alpha, double[] axpyValues) {
		dupuitCFile.axpyOnHydraulicHead(alpha,axpyValues);
	}

	
	public void multiplyValues(double[] multiplicationFactors) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitCFileExchangeItem - Method Name : multiplyValues");
	}

	
	public void setValues(Object values) {
		if (!(values instanceof double[])) {
			throw new RuntimeException("SetValues for" + this.getId() + ": unexpected object type: " + values.getClass().getName());
		}
		setValuesAsDoubles((double[]) values);
	}

	
	public void setValuesAsDoubles(double[] values) {
		dupuitCFile.setHydraulicHead(values);
	}

	
	public double[] getTimes() {
		return this.dupuitCFile.getTimes();
	}

	
	public void setTimes(double[] times) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitCFileExchangeItem - Method Name : setTimes");
	}
}
