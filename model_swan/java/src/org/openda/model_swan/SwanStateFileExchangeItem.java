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

package org.openda.model_swan;

import org.openda.interfaces.*;

import java.io.IOException;

/**
 * Exchange item for reading and writing SWAN state file.
 */
public class SwanStateFileExchangeItem implements IExchangeItem {
    private String id;
    private SwanStateFile swanStateFile;

    public SwanStateFileExchangeItem(String id, SwanStateFile swanStateFile){
        this.id = id;
        this.swanStateFile = swanStateFile;
    }

    public String getId(){
        return id;
    }

    public String getDescription(){
        return null; // no description
    }

    public Class getValueType() {
        return double[].class;
    }

    public ValueType getValuesType() {
        return ValueType.doublesType;
    }

    public Role getRole(){
        return IPrevExchangeItem.Role.InOut;
    }

    public Object getValues() {
        return getValuesAsDoubles();
    }

    public double[] getValuesAsDoubles() {
        double[] stateValues = null; // is this OK???
        try {
            stateValues = swanStateFile.getAllStateValues();
        } catch (IOException e) {
            throw new RuntimeException("org.openda.model_swan.SwanStateFileExchangeItem.getAllStateValues(): not successful.");
        }
        return stateValues;
    }

    public void axpyOnValues(double alpha, double[] axpyValues) {
        try {
            swanStateFile.axpyOnAllState(alpha,axpyValues);
        } catch (IOException e) {
            throw new RuntimeException("org.openda.model_swan.SwanStateFileExchangeItem.axpyOnValues(): not successful.");
        }
    }

    public void multiplyValues(double[] multiplicationFactors) {
        try {
            swanStateFile.multiplyAllState(multiplicationFactors);
        } catch (IOException e) {
            throw new RuntimeException("org.openda.model_swan.SwanStateFileExchangeItem.multiplyValues(): not successful.");
        }
    }

    public void setValues(Object values) {
        if (!(values instanceof double[])) {
            throw new RuntimeException("SetValues for" + this.getId() + ": unexpected object type: " + values.getClass().getName());
        }
        setValuesAsDoubles((double[]) values);
    }

    public void setValuesAsDoubles(double[] values) {
        swanStateFile.setAllStateValues(values);
    }

    public double[] getTimes() {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanStateFileExchangeItem.getTimes(): Not implemented yet.");
    }

    public void setTimes(double[] times) {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanStateFileExchangeItem.setTimes(): Not implemented yet.");
    }

	
	public void copyValuesFromItem(IExchangeItem sourceItem) {
		throw new UnsupportedOperationException("org.openda.model_swan.SwanStateFileExchangeItem.copyValuesFromItem(): Not implemented yet.");
	}

	
	public ITimeInfo getTimeInfo() {
		throw new UnsupportedOperationException("org.openda.model_swan.SwanStateFileExchangeItem.getTimeInfo(): Not implemented yet.");
	}

	
	public IQuantityInfo getQuantityInfo() {
		throw new UnsupportedOperationException("org.openda.model_swan.SwanStateFileExchangeItem.getQuantityInfo(): Not implemented yet.");
	}

	
	public IGeometryInfo getGeometryInfo() {
		return null;
	}
}
