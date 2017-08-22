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
package org.openda.exchange;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;
import org.openda.utils.Array;
import org.openda.utils.MyObservable;

/**
 * Simple exchangeItem for String data. You can add a listener in eg the IDataObject
 * to take action on changes.
 *
 * @author verlaanm
 *
 */
public class DoubleExchangeItem extends MyObservable implements IExchangeItem{
	private double value;
	private double time=Double.NaN;
	private String id;
	private String description="";
	private Role role=Role.InOut;
	private IQuantityInfo quantityInfo=null;
	private PointGeometryInfo geometryInfo=null;


	public DoubleExchangeItem(String id, Role role, double value){
		this.value = value;
		this.role = role;
		this.id = id;
		this.geometryInfo = new PointGeometryInfo(0.,0., 0.); //fill with defaults
	}

	public DoubleExchangeItem(String id, double value){
		this(id,Role.InOut,value);
	}

	public DoubleExchangeItem(String id, Role role){
		this(id, role, 0);
	}


	public Role getRole() {
		return this.role;
	}


	public String getId() {
		return this.id;
	}


	public String getDescription() {
		return this.description;
	}

	public void setDescription(String description){
		this.description=description;
	}


	public void copyValuesFromItem(IExchangeItem sourceItem) {
		ValueType sourceType=sourceItem.getValuesType();
		if(sourceType==ValueType.doubleType){
			this.value=(Double)sourceItem.getValues();
			this.notifyObservers();

		} else if (sourceType == ValueType.IArrayType) {
			if (sourceItem.getTimeInfo() == null || sourceItem.getTimeInfo().getTimes() == null) {
				throw new RuntimeException(getClass().getSimpleName() + ": cannot copy data from sourceExchangeItem '" + sourceItem.getId()
						+ "' of type " + sourceItem.getClass().getSimpleName() + " because it contains no time info.");
			}

			//get index of wantedTime in sourceItem.
			double[] sourceTimes = sourceItem.getTimeInfo().getTimes();
			double wantedTime = this.time;
			int wantedTimeIndex = TimeUtils.findMatchingTimeIndex(sourceTimes, wantedTime, 1e-5);
			if (wantedTimeIndex == -1) {//if sourceItem does not contain wantedTime.
				//no data to copy.
				return;
			}

			//get value for wanted time.
			//this code assumes that the sourceItem is a scalar time series.
			Array sourceValues = (Array) sourceItem.getValues();
			double valueForWantedTime = sourceValues.getValueAsDouble(wantedTimeIndex);
			setValue(valueForWantedTime);
		}
	}

	public ITimeInfo getTimeInfo() {
		return new TimeInfo(new double[]{this.time});
	}


	public IQuantityInfo getQuantityInfo() {
		return this.quantityInfo;
	}

	public void setQuantityInfo(IQuantityInfo quantityInfo){
		this.quantityInfo=quantityInfo;
		this.notifyObservers();
	}


	public IGeometryInfo getGeometryInfo() {
		return this.geometryInfo;
	}

    public String getLocation(){
    	return this.geometryInfo.getLocation();
    }

    public void setLocation(String location){
    	this.geometryInfo.setLocation(location);
    }

    public double[] getPosition(){
    	return new double[]{ this.geometryInfo.getLongitude() , this.geometryInfo.getLatitude() };
    }

    public void setLatitude(double latitude){
    	String location = this.geometryInfo.getLocation();
    	this.geometryInfo = new PointGeometryInfo(this.geometryInfo.getLongitude(), latitude, this.geometryInfo.getHeight());
    	this.geometryInfo.setLocation(location);
    }

    public void setLongitude(double longitude){
    	String location = this.geometryInfo.getLocation();
    	this.geometryInfo = new PointGeometryInfo(longitude, this.geometryInfo.getLatitude(), this.geometryInfo.getHeight());
    	this.geometryInfo.setLocation(location);
    }

    public void setHeight(double height){
    	String location = this.geometryInfo.getLocation();
    	this.geometryInfo = new PointGeometryInfo(this.geometryInfo.getLongitude(), this.geometryInfo.getLatitude(), height);
    	this.geometryInfo.setLocation(location);
    }

	public ValueType getValuesType() {
		return ValueType.doubleType;
	}


	public Object getValues() {
		return this.value;
	}

	public void setValue(double value){
		this.value=value;
		this.notifyObservers();
	}

	public double getValue(){
		return this.value;
	}

	public double getTime(){
		return this.time;
	}

	public void setTime(double time){
		this.time=time;
		this.notifyObservers();
	}

	/*
	 * =======================================================================
	 * The getTimes/setTimes methods are part of the old IPrevExchangeItem
	 * interface and will be removed later.
	 * =======================================================================
	 */


	@SuppressWarnings("rawtypes")

	public Class getValueType() {
		return double.class;
	}


	@Deprecated public double[] getTimes() {
		return null;
	}


	@Deprecated public void setTimes(double[] times) {
		throw new UnsupportedOperationException("setTimes method make no sense for a DoubleExchangeItem");
	}


	public void setValuesAsDoubles(double[] values) {
		if(values.length==1){
			this.value = values[0];
			this.notifyObservers();
		}else{
			throw new RuntimeException("Number of values can only be 1 for DoubleExchangeItem");
		}
	}


	public double[] getValuesAsDoubles() {
		return new double[]{this.value};
	}


	public void axpyOnValues(double alpha, double[] axpyValues) {
		if(axpyValues.length==1){
			this.value += alpha*axpyValues[0];
			this.notifyObservers();
		}else{
			throw new RuntimeException("Number of values can only be 1 for DoubleExchangeItem");
		}
	}


	public void multiplyValues(double[] multiplicationFactors) {
		if(multiplicationFactors.length==1){
			this.value *= multiplicationFactors[0];
			this.notifyObservers();
		}else{
			throw new RuntimeException("Number of values can only be 1 for DoubleExchangeItem");
		}
	}


	public void setValues(Object values) {
		if(values instanceof Double){
			this.value = (Double) values;
			this.notifyObservers();
		}else{
			throw new UnsupportedOperationException("Can only set values for a Double object");
		}
	}

	public String toString(){
		return "{"+this.id+"="+this.value+"}";
	}

}
