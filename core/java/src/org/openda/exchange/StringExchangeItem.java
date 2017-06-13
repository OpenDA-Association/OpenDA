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
package org.openda.exchange;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;
import org.openda.utils.MyObservable;

/**
 * Simple exchangeItem for String data. You can add a listener in eg the IDataObject
 * to take action on changes.
 * 
 * @author verlaanm
 *
 */
public class StringExchangeItem extends MyObservable implements IExchangeItem{
	private String value;
	private String id;
	private Role role=Role.InOut;
	
	
	public StringExchangeItem(String id, Role role, String value){
		this.value = value;
		this.role = role;
		this.id = id;
	}
	
	public StringExchangeItem(String id, Role role){
		this(id, role, null);
	}
	
	
	public Role getRole() {
		return this.role;
	}

	
	public String getId() {
		return this.id;
	}

	
	public String getDescription() {
		return this.id;
	}

	
	public void copyValuesFromItem(IExchangeItem sourceItem) {
		ValueType sourceType=sourceItem.getValuesType();
		if(sourceType==ValueType.StringType){
			this.setValue((String)sourceItem.getValues());
			this.notifyObservers();
		}
	}

	
	public ITimeInfo getTimeInfo() {
		return null;
	}

	
	public IQuantityInfo getQuantityInfo() {
		return null;
	}

	
	public IGeometryInfo getGeometryInfo() {
		return null;
	}

	
	public ValueType getValuesType() {
		return ValueType.StringType;
	}

	
	public Object getValues() {
		return this.value;
	}

	public String getValue(){
		return this.value;
	}
	
	public void setValue(String value){
		this.value=value;
		this.notifyObservers();
	}
	/*
	 * =======================================================================
	 * The following methods are due to the old interface and will be removed
	 * later.
	 * =======================================================================
	 */
	
	
	@SuppressWarnings("rawtypes")
	
	@Deprecated public Class getValueType() {
		return String.class;
	}

	
	@Deprecated public double[] getTimes() {
		return null;
	}

	
	@Deprecated public void setTimes(double[] times) {
		throw new UnsupportedOperationException("setTimes method make no sense for a StringExchangeItem");		
	}

	
	@Deprecated public void setValuesAsDoubles(double[] values) {
		throw new UnsupportedOperationException("setValuesAsDoubles method make no sense for a StringExchangeItem");		
	}

	
	@Deprecated public double[] getValuesAsDoubles() {
		throw new UnsupportedOperationException("getValuesAsDoubles method make no sense for a StringExchangeItem");
	}

	
	@Deprecated public void axpyOnValues(double alpha, double[] axpyValues) {
		throw new UnsupportedOperationException("axpy method make no sense for a StringExchangeItem");
	}

	
	@Deprecated public void multiplyValues(double[] multiplicationFactors) {
		throw new UnsupportedOperationException("multiplyValues method make no sense for a StringExchangeItem");
	}

	
	@Deprecated public void setValues(Object values) {
		if(values instanceof String){
			this.value = (String) values;
			this.notifyObservers();
		}else{
			throw new UnsupportedOperationException("Can only set values for a String object");
		}
	}

}
