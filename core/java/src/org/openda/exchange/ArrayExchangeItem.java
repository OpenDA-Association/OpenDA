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

package org.openda.exchange;

import org.openda.interfaces.*;
import org.openda.utils.Array;
import org.openda.utils.IMyObservable;
import org.openda.utils.IMyObserver;

import java.util.ArrayList;

/**
 * General ExchangeItem for storing array based data.
 *
 * Example 1: 2D cartesian grid
 *
 *   time      = [55880.0, 55880.1, ..., 55881.0]   (Nov 15 2011)
 *     timeIndex = 0
 *   latitude  = [-180, -179, ..., 178, 179, 180]   (WGS84 degrees east)
 *     latitudeIndex = 1
 *   longitude = [-90, -89, ... ,89 ,90]
 *     longitudeIndex = 2
 *
 *   values is a 3D array with indices[time, latitude, longitude]
 *   which corresponds to values[time][y][x] in c or java and
 *   to values[x,y,time] in fortran.
 *
 *  Example 1b: 2D structured grid
 *
 *   time      = [55880.0, 55880.1, ..., 55881.0]   (Nov 15 2011)
 *     timeIndex = 0
 *   latitude  = 2D array with latitudes   (WGS84 degrees east)
 *     latitudeIndex = 1,2
 *   longitude = 2D array with longitudes
 *     longitudeIndex = 1,2
 *
 * @author Arno Kockx
 */
public class ArrayExchangeItem implements IArrayExchangeItem, IMyObservable {
	
	public int MAXPLOTLENGTH=400;
    private final String id;
    private final Role role;
    private IArrayTimeInfo timeInfo = null;
    protected IArray array = null;
	private IQuantityInfo quantityInfo = null;
	private IArrayGeometryInfo geometryInfo = null;
	private ArrayList<IMyObserver> observers = new ArrayList<IMyObserver>();


    /**
     * @param id for this exchangeItem.
     * @param role
     */
    public ArrayExchangeItem(String id, Role role) {
        if (id == null) {
            throw new IllegalArgumentException(this.getClass().getSimpleName() + ": id is null.");
        }
        if (role == null) {
            throw new IllegalArgumentException(this.getClass().getSimpleName() + ": role is null.");
        }

        this.id = id;
        this.role = role;
        //store empty Array.
        this.array = new Array(new int[]{0});
    }

    
    public Role getRole() {
        return this.role;
    }

    
    public String getId() {
        return id;
    }

    
    public String getDescription() {
        return null;
    }

    
    public void copyValuesFromItem(IExchangeItem sourceItem) {

        //copy time info.
        ITimeInfo sourceTimes = sourceItem.getTimeInfo();
        if(sourceTimes instanceof IArrayTimeInfo){
        	setTimeInfo((IArrayTimeInfo) sourceTimes);
        }else{
        	setTimeInfo(new TimeInfo(sourceTimes.getTimes()));
        }
        //copy quantity info.
        IQuantityInfo quantityInfo = sourceItem.getQuantityInfo();
        if (quantityInfo == null) {
        	setQuantityInfo(null);
        } else {//if quantityInfo != null.
        	setQuantityInfo(new QuantityInfo(quantityInfo.getQuantity(), quantityInfo.getUnit()));
        }
        //copy geometry info.
        IGeometryInfo geometryInfo = sourceItem.getGeometryInfo();
        if (geometryInfo == null) {
        	setGeometryInfo(null);
        } else {//if geometryInfo != null.
	        if (geometryInfo instanceof IArrayGeometryInfo) {
	        	ArrayGeometryInfo geometry = (ArrayGeometryInfo) geometryInfo;
	        	setGeometryInfo(new ArrayGeometryInfo(geometry.getLatitudeArray(), geometry.getLatitudeValueIndices(),
	        			geometry.getLatitudeQuantityInfo(), geometry.getLongitudeArray(),
	        			geometry.getLongitudeValueIndices(), geometry.getLongitudeQuantityInfo(),
	        			geometry.getHeightArray(), geometry.getHeightValueIndices(), geometry.getHeightQuantityInfo()));
	        } else {
	        	//other IGeometryInfo types not supported.
	        }
        }

        Object values = sourceItem.getValues();
        if (!(values instanceof IArray)) {
            throw new IllegalArgumentException("Cannot convert values of type " + sourceItem.getValueType().getSimpleName()
                    + " to type " + getValueType().getSimpleName());
        }

        IArray arrayCopy = new Array((IArray) sourceItem.getValues());
        this.array = arrayCopy;
        this.notifyObservers();
    }

    
    public ITimeInfo getTimeInfo() {
        return this.timeInfo;
    }

    
    public IQuantityInfo getQuantityInfo() {
    	return this.quantityInfo;
    }

    
    public IGeometryInfo getGeometryInfo() {
    	return this.geometryInfo;
    }

    
	//TODO remove when interface IOldExchangeItem is removed. MVL
    public Class<IArray> getValueType() {
        return IArray.class;
    }

    public ValueType getValuesType(){
    	return ValueType.IArrayType;
    }

    
    public IArray getValues() {
        return this.array;
    }

	
	//TODO remove when interface IOldExchangeItem is removed. AK
	public void setValues(Object values) {
		throw new UnsupportedOperationException(this.getClass().getSimpleName() + ": setValues method of old interface IPrevExchangeItem not implemented.");
	}

	
	//TODO remove when interface IOldExchangeItem is removed. AK
	public double[] getValuesAsDoubles() {
		return array.getValuesAsDoubles();
	}

	
	//TODO remove when interface IOldExchangeItem is removed. AK
	public void axpyOnValues(double alpha, double[] axpyValues) {
		array.axpyOnValues(alpha, axpyValues);
		this.notifyObservers();
	}

	
	//TODO remove when interface IOldExchangeItem is removed. AK
	public void multiplyValues(double[] multiplicationFactors) {
		array.multiplyValues(multiplicationFactors);
		this.notifyObservers();
	}

	
	//TODO remove when interface IOldExchangeItem is removed. AK
	public void setValuesAsDoubles(double[] values) {
		array.setValuesAsDoubles(values);
		this.notifyObservers();
	}

	
    //TODO remove when interface IOldExchangeItem is removed. AK
    public double[] getTimes() {
        return (getTimeInfo()!=null) ? getTimeInfo().getTimes() : null;
    }

    
    //TODO remove when interface IOldExchangeItem is removed. AK
    public void setTimes(double[] times) {
        throw new UnsupportedOperationException(this.getClass().getSimpleName() + ": setTimes method of old interface IPrevExchangeItem not implemented.");
    }

    /**
     * Copies the given timeInfo and stores the copy.
     *
     * @param timeInfo
     */
    public void setTimeInfo(IArrayTimeInfo timeInfo) {
        this.timeInfo = new TimeInfo(timeInfo);
		this.notifyObservers();
    }

    public void setQuantityInfo(IQuantityInfo quantityInfo) {
		this.quantityInfo = quantityInfo;
		this.notifyObservers();
	}

	public void setGeometryInfo(IArrayGeometryInfo geometryInfo) {
		this.geometryInfo = geometryInfo;
		this.notifyObservers();
	}

	
	public IArray getArray() {
		return this.array;
	}

    public void setArray(IArray array) {
    	this.array = array;
		this.notifyObservers();
    }
    
    public String toString(){
    	String result = "{ArrayBasedExchangeItem id="+this.id+"\n";
    	if(this.quantityInfo!=null){ result+=" "+this.quantityInfo.toString()+"\n"; }
    	if(this.geometryInfo!=null){ result+=" "+this.geometryInfo.toString()+"\n"; }
    	if(this.timeInfo!=null){     result+=" "+this.timeInfo.toString()+"\n"; }
    	if(this.array!=null){
    		if(this.array.length()<this.MAXPLOTLENGTH){ 
    			result+=" values ="+this.array.toString()+"\n";
    		}else{
    			result+=" values.size="+this.array.getDimensions()+"\n";
    		}
    	}
    	result+="}";
    	return result;
    }

	
	public void addObserver(IMyObserver observer) {
		this.observers.add(observer);
	}

	
	public void notifyObservers() {
		for(IMyObserver o : this.observers){
			o.update(this, null);
		}
	}

}
