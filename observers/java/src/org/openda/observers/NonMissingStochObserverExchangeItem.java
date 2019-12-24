/* MOD_V2.0
* Copyright (c) 2012 OpenDA Association
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
package org.openda.observers;
import org.openda.interfaces.*;

import java.io.Serializable;
import java.util.Hashtable;

/**
 * TODO: korte beschrijving
 * TODO: implement the IExchangeItem methods. Consult Stef.
 */
public class NonMissingStochObserverExchangeItem implements IExchangeItem, Serializable {

    private IExchangeItem ioExchangeItem;
    private double[] values;
    private double[] times;

    public NonMissingStochObserverExchangeItem(IExchangeItem ioExchangeItem, double missingValue){
        this.ioExchangeItem = ioExchangeItem;
        Hashtable<Integer,Double> eiValues = new Hashtable<Integer,Double>();
        Hashtable<Integer,Double> eiTimes = new Hashtable<Integer,Double>();
        boolean isMissing = false;
        int iNonMissing = 0;
        double[] orgValues = ioExchangeItem.getValuesAsDoubles();
        double[] orgTimes = ioExchangeItem.getTimes();
        //TODO: determine the 'most' appropriate value of epsilon
        if (orgTimes.length != orgValues.length) {
            throw new RuntimeException("org.openda.observers.NonMissingStochObserverExchangeItem: Inconsistent #times/#values of original exchange item.");
        }
        if(orgTimes!=null){
        double epsilon = 1.E-5;
        if (orgValues!=null && orgTimes!=null) {
        	for (int iValues=0; iValues<orgValues.length; iValues++){
        		if ((Double.isNaN(orgValues[iValues]) || Math.abs(orgValues[iValues]-missingValue)<epsilon)){
        			isMissing = true;
        		} else {
        			eiValues.put(iNonMissing,orgValues[iValues]);
        			eiTimes.put(iNonMissing,orgTimes[iValues]);
        			iNonMissing++;
        		}
        	}
        }
        }
        if (isMissing){
            double[] newValues = new double[eiValues.size()];
            double[] newTimes = new double[eiValues.size()];
            for (int iNewValues=0; iNewValues<newValues.length; iNewValues++){
                newValues[iNewValues] = eiValues.get(iNewValues);
                newTimes[iNewValues] = eiTimes.get(iNewValues);
            }
            this.values = newValues;
            this.times = newTimes;
        } else {
            values = this.ioExchangeItem.getValuesAsDoubles();
            times = this.ioExchangeItem.getTimes();
        }
    }

    
    public void copyValuesFromItem(IExchangeItem sourceItem) {
        throw new UnsupportedOperationException("org.openda.observers.NonMissingStochObserverExchangeItem.copyValuesFromItem(): Not implemented yet.");
    }

    
    public ITimeInfo getTimeInfo() {
        throw new UnsupportedOperationException("org.openda.observers.NonMissingStochObserverExchangeItem.getTimeInfo(): Not implemented yet.");
    }

    
    public IQuantityInfo getQuantityInfo() {
        throw new UnsupportedOperationException("org.openda.observers.NonMissingStochObserverExchangeItem.getQuantityInfo(): Not implemented yet.");
    }

    
    public IGeometryInfo getGeometryInfo() {
		//TODO all exchangeItems should implement IExchangeItem. AK
		if (this.ioExchangeItem instanceof IExchangeItem) {
			return ((IExchangeItem) this.ioExchangeItem).getGeometryInfo();
		}
		return null;
    }

    
    public String getId() {
        return this.ioExchangeItem.getId();
    }

    
    public String getDescription() {
        return this.ioExchangeItem.getDescription();
    }

    public ValueType getValuesType() {
        return ioExchangeItem.getValuesType();
    }


    
    public Role getRole() {
        return null;
    }

    public PrevRole getPrevRole() { return this.ioExchangeItem.getPrevRole();}
    
    public Object getValues() {
        return this.values;
    }

    
    public double[] getValuesAsDoubles() {
        return this.values;
    }

    
    public void axpyOnValues(double alpha, double[] axpyValues) {
        this.ioExchangeItem.axpyOnValues(alpha,axpyValues);
    }

    
    public void multiplyValues(double[] multiplicationFactors) {
        this.ioExchangeItem.multiplyValues(multiplicationFactors);
    }

    
    public void setValues(Object values) {
        throw new RuntimeException("setValues not allowed for an observer");
    }

    
    public void setValuesAsDoubles(double[] values) {
        throw new RuntimeException("setValuesAsDoubles not allowed for an observer");
    }

    
    public double[] getTimes() {
        return this.times;
    }

    
    public void setTimes(double[] times) {
        throw new RuntimeException("setTimes not allowed for an observer");
    }
}
