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

package org.openda.model_swan;

import org.openda.interfaces.*;

import java.io.IOException;

/**
 * Exchange item for reading and writing SWAN open boundary in spectral format.
 */
public class SwanOpenBoundarySpectralFileExchangeItem implements IExchangeItem {
    private String id;
    private SwanOpenBoundarySpectralFile swanOBFile;
    private Integer iTime = -1;

    public SwanOpenBoundarySpectralFileExchangeItem(String id, SwanOpenBoundarySpectralFile swanOBFile){
//        public SwanOpenBoundarySpectralFileExchangeItem(String id, SwanOpenBoundarySpectralFile swanOBFile, Integer iTime){
        this.id = id;
        this.swanOBFile = swanOBFile;
//        this.iTime = iTime;
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

    public IPrevExchangeItem.Role getRole(){
        return IPrevExchangeItem.Role.InOut;
    }

    public Object getValues() {
        return getValuesAsDoubles();
    }

    public double[] getValuesAsDoubles() {
        double[] obValues;
        int nTimeMax = swanOBFile.getNTimes();
        try {
            iTime++;
            if (iTime==nTimeMax){
                throw new RuntimeException("Data at iTime>"+nTimeMax+" is not available: org.openda.model_swan.SwanStateFileExchangeItem.getValuesAsDoubles");
            }
            obValues = swanOBFile.getAllStateValues(iTime);
        } catch (IOException e) {
            throw new RuntimeException("org.openda.model_swan.SwanStateFileExchangeItem.getAllStateValues(): not successful.");
        }
        return obValues;
    }

    public void axpyOnValues(double alpha, double[] axpyValues) {
        try {
            swanOBFile.axpyOnAllState(iTime,alpha,axpyValues);
        } catch (IOException e) {
            throw new RuntimeException("org.openda.model_swan.SwanStateFileExchangeItem.axpyOnValues(): not successful.");
        }
    }

    public void multiplyValues(double[] multiplicationFactors) {
        try {
            swanOBFile.multiplyAllState(iTime,multiplicationFactors);
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
        swanOBFile.setAllStateValues(iTime,values);
    }

    public double[] getTimes() {
        return swanOBFile.getTimes();
    }

    public void setTimes(double[] times) {
        throw new RuntimeException("Open boundary times can not be set.");
    }

    public void resetTimeCounter() {
        this.iTime = -1;
    }

	
	public void copyValuesFromItem(IExchangeItem sourceItem) {
		throw new UnsupportedOperationException("org.openda.model_swan.SwanOpenBoundarySpectralFileExchangeItem.copyValuesFromItem(): Not implemented yet.");
	}

	
	public ITimeInfo getTimeInfo() {
		return swanOBFile;
	}

	
	public IQuantityInfo getQuantityInfo() {
		throw new UnsupportedOperationException("org.openda.model_swan.SwanOpenBoundarySpectralFileExchangeItem.getQuantityInfo(): Not implemented yet.");
	}

	
	public IGeometryInfo getGeometryInfo() {
		return null;
	}
}
