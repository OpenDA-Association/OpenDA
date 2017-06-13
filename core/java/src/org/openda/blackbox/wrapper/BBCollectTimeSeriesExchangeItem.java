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

package org.openda.blackbox.wrapper;

import org.openda.interfaces.*;
import java.util.ArrayList;

/**
 * Black box module for wrapping prediction vector
**/

public class BBCollectTimeSeriesExchangeItem implements IExchangeItem	{
    private IPrevExchangeItem modelExchangeItem;
    private ArrayList<Double> concatenatedValues = new ArrayList<Double>();
    private ArrayList<Double> concatenatedTimes = new ArrayList<Double>();
//    private Class valueType;

    public BBCollectTimeSeriesExchangeItem(IPrevExchangeItem modelExchangeItem)
    {
        this.modelExchangeItem = modelExchangeItem;
//        this.valueType = modelExchangeItem.getValueType();
    }


    public void UpdateTimeStepValue(ITime time, double value) {
        concatenatedValues.add(value);
        concatenatedTimes.add(time.getMJD());
    }

    
    public Role getRole() {
        return modelExchangeItem.getRole();
    }

    
    public String getId() {
        return modelExchangeItem.getId();
    }

    
    public String getDescription() {
        throw new UnsupportedOperationException("org.openda.blackbox.wrapper.BBStochModelInstance.BBCollectTimeSeriesExchangeItem.getDescription(): Not implemented yet.");
    }

    
    public Class getValueType() {
//        return this.valueType;
        return double[].class;
    }

    
    public void copyValuesFromItem(IExchangeItem sourceItem) {
        throw new UnsupportedOperationException("org.openda.blackbox.wrapper.BBStochModelInstance.BBCollectTimeSeriesExchangeItem.copyValuesFromItem(): Not implemented yet.");
    }

    
    public ITimeInfo getTimeInfo() {
        throw new UnsupportedOperationException("org.openda.blackbox.wrapper.BBStochModelInstance.BBCollectTimeSeriesExchangeItem.getTimeInfo(): Not implemented yet.");
    }

    
    public IQuantityInfo getQuantityInfo() {
        throw new UnsupportedOperationException("org.openda.blackbox.wrapper.BBStochModelInstance.BBCollectTimeSeriesExchangeItem.getQuantityInfo(): Not implemented yet.");
    }

    
    public IGeometryInfo getGeometryInfo() {
		//TODO all exchangeItems should implement IExchangeItem. AK
		if (this.modelExchangeItem instanceof IExchangeItem) {
			return ((IExchangeItem) this.modelExchangeItem).getGeometryInfo();
		}
		return null;
    }

    
    public ValueType getValuesType() {
        throw new UnsupportedOperationException("org.openda.blackbox.wrapper.BBStochModelInstance.BBCollectTimeSeriesExchangeItem.getValuesType(): Not implemented yet.");
    }

    
    public Object getValues() {
        return getValuesAsDoubles();
    }

    
    public double[] getValuesAsDoubles() {
        double[] dblConcatenatedValues = new double[concatenatedValues.size()];
        for (int i=0; i<concatenatedValues.size(); i++){
            dblConcatenatedValues[i] = concatenatedValues.get(i);
        }
        return dblConcatenatedValues;
    }

    
    public void axpyOnValues(double alpha, double[] axpyValues) {
        throw new UnsupportedOperationException("org.openda.blackbox.wrapper.BBStochModelInstance.BBCollectTimeSeriesExchangeItem.axpyOnValues(): Not implemented yet.");
    }

    
    public void multiplyValues(double[] multiplicationFactors) {
        throw new UnsupportedOperationException("org.openda.blackbox.wrapper.BBStochModelInstance.BBCollectTimeSeriesExchangeItem.multiplyValues(): Not implemented yet.");
    }

    
    public void setValues(Object values) {
        throw new UnsupportedOperationException("org.openda.blackbox.wrapper.BBStochModelInstance.BBCollectTimeSeriesExchangeItem.setValues(): Not implemented yet.");
    }

    
    public void setValuesAsDoubles(double[] values) {
        throw new UnsupportedOperationException("org.openda.blackbox.wrapper.BBStochModelInstance.BBCollectTimeSeriesExchangeItem.setValuesAsDoubles(): Not implemented yet.");
    }

    
    public double[] getTimes() {
        double[] dblConcatenatedTimes = new double[concatenatedTimes.size()];
        for (int i=0; i<concatenatedTimes.size(); i++){
            dblConcatenatedTimes[i] = concatenatedTimes.get(i);
        }
        return dblConcatenatedTimes;
    }

    
    public void setTimes(double[] times) {
        throw new UnsupportedOperationException("org.openda.blackbox.wrapper.BBStochModelInstance.BBCollectTimeSeriesExchangeItem.setTimes(): Not implemented yet.");
    }
}

