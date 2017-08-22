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
 * Time: 14:47
 * To change this template use File | Settings | File Templates.
 */
public class DupuitPFileExchangeItem implements IExchangeItem {
	private DupuitPFile dupuitPFile;
	private String id;
	private String[] availableIDs = new String[]{"sTime","eTime","sPolder"};

	public DupuitPFileExchangeItem(String id, DupuitPFile dupuitPFile) {
		this.id = id;
		this.dupuitPFile = dupuitPFile;
	}

	
	public Role getRole() {
		return IPrevExchangeItem.Role.InOut;
	}

	
	public String getId() {
		return this.id;
	}

	
	public String getDescription() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitPFileExchangeItem - Method Name : getDescription");
	}

	
	public Class getValueType() {
		return double.class;
	}

	
	public void copyValuesFromItem(IExchangeItem sourceItem) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitPFileExchangeItem - Method Name : copyValuesFromItem");
	}

	
	public ITimeInfo getTimeInfo() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitPFileExchangeItem - Method Name : getTimeInfo");
	}

	
	public IQuantityInfo getQuantityInfo() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitPFileExchangeItem - Method Name : getQuantityInfo");
	}

	
	public IGeometryInfo getGeometryInfo() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitPFileExchangeItem - Method Name : getGeometryInfo");
	}

	
	public ValueType getValuesType() {
		return ValueType.doubleType;
	}

	
	public Object getValues() {
		double value;
		if (this.id.equals(availableIDs[0])) {
			value = dupuitPFile.getSTime();
		} else if (this.id.equals(availableIDs[1])){
			value = dupuitPFile.getETime();
		} else {
			value = dupuitPFile.getSPolder();
		}
		return value;
	}

	
	public double[] getValuesAsDoubles() {
        return new double[]{(Double)getValues()};
	}

	
	public void axpyOnValues(double alpha, double[] axpyValues) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitPFileExchangeItem - Method Name : axpyOnValues");
	}

	
	public void multiplyValues(double[] multiplicationFactors) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitPFileExchangeItem - Method Name : multiplyValues");
	}

	
	public void setValues(Object values) {
		if (this.id.equals(availableIDs[0])){
			this.dupuitPFile.setSTime((Double)values);
		} else if (this.id.equals(availableIDs[1])){
			this.dupuitPFile.setETime((Double)values);
		} else if (this.id.equals(availableIDs[2])){
			this.dupuitPFile.setSPolder((Double)values);
		}
	}

	
	public void setValuesAsDoubles(double[] values) {
		if (this.id.equals(availableIDs[0])){
			this.dupuitPFile.setSTime((Double)values[0]);
		} else if (this.id.equals(availableIDs[1])){
			this.dupuitPFile.setETime((Double)values[0]);
		} else if (this.id.equals(availableIDs[2])){
			this.dupuitPFile.setSPolder((Double)values[0]);
		}
	}

	
	public double[] getTimes() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitPFileExchangeItem - Method Name : getTimes");
	}

	
	public void setTimes(double[] times) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitPFileExchangeItem - Method Name : setTimes");
	}
}
