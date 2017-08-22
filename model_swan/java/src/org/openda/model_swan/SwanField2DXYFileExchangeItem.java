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

package org.openda.model_swan;

import org.openda.interfaces.*;

/**
 * Exchange item of vector components (XY) input/output SWAN, each in 2D field data format.
 */
public class SwanField2DXYFileExchangeItem implements IExchangeItem {
    private String id;
    private SwanField2DXYFile swanWindFile;
//    private String direction;
    private Integer iTimeX = -1;
    private Integer iTimeY = -1;
    private Integer iTimeXY = -1;

    public SwanField2DXYFileExchangeItem(String id, SwanField2DXYFile swanWindFile){
        this.id = id;
        this.swanWindFile = swanWindFile;
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
        double[] obValues = null;
//        int nTimeMax = swanWindFile.getNTimes();
//        if (id.equalsIgnoreCase("wind.x")) {
//            iTimeX++;
//            if (iTimeX==swanWindFile.getNTimes()){
//                throw new RuntimeException("Data at iTime>"+nTimeMax+" is not available: org.openda.model_swan.SwanWindFileExchangeItem.getValuesAsDoubles");
//            }
//            obValues = swanWindFile.getXComp(iTimeX);
//        } else if (id.equalsIgnoreCase("wind.y")) {
//            iTimeY++;
//            if (iTimeY==swanWindFile.getNTimes()){
//                throw new RuntimeException("Data at iTime>"+nTimeMax+" is not available: org.openda.model_swan.SwanWindFileExchangeItem.getValuesAsDoubles");
//            }
//            obValues = swanWindFile.getYComp(iTimeY);
//        } else if (id.equalsIgnoreCase("wind.xy")) {
//            iTimeXY++;
//            if (iTimeY==swanWindFile.getNTimes()){
//                throw new RuntimeException("Data at iTime>"+nTimeMax+" is not available: org.openda.model_swan.SwanWindFileExchangeItem.getValuesAsDoubles");
//            }
//            obValues = swanWindFile.getXYComp(iTimeXY);
//        }
        return obValues;
    }

    public void axpyOnValues(double alpha, double[] axpyValues) {
//        if (id.equalsIgnoreCase("wind.x")) {
//            swanWindFile.axpyXComp(iTimeX,alpha,axpyValues);
//        } else if (id.equalsIgnoreCase("wind.y")) {
//            swanWindFile.axpyYComp(iTimeY,alpha,axpyValues);
//        } else if (id.equalsIgnoreCase("wind.xy")) {
//            swanWindFile.axpyXYComp(iTimeXY,alpha,axpyValues);
//        }
    }

    public void multiplyValues(double[] multiplicationFactors) {
//        if (id.equalsIgnoreCase("wind.x")) {
//            swanWindFile.multiplyXComp(iTimeX,multiplicationFactors);
//        } else if (id.equalsIgnoreCase("wind.y")) {
//            swanWindFile.multiplyYComp(iTimeY,multiplicationFactors);
//        } else if (id.equalsIgnoreCase("wind.xy")) {
//            swanWindFile.multiplyXYComp(iTimeXY,multiplicationFactors);
//        }
    }

    public void setValues(Object values) {
        if (!(values instanceof double[])) {
            throw new RuntimeException("SetValues for" + this.getId() + ": unexpected object type: " + values.getClass().getName());
        }
        setValuesAsDoubles((double[]) values);
    }

    public void setValuesAsDoubles(double[] values) {
//        if (id.equalsIgnoreCase("wind.x")) {
//            swanWindFile.setXComp(iTimeX,values);
//        } else if (id.equalsIgnoreCase("wind.y")) {
//            swanWindFile.setYComp(iTimeY,values);
//        } else if (id.equalsIgnoreCase("wind.xy")) {
//            swanWindFile.setXYComp(iTimeXY,values);
//        }
    }

    public double[] getTimes() {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanStateFileExchangeItem.getTimes(): Not implemented yet.");
    }

    public void setTimes(double[] times) {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanStateFileExchangeItem.setTimes(): Not implemented yet.");
    }

    public void resetTimeCounter() {
        if (id.equalsIgnoreCase("wind.x")) {
            iTimeX = -1;
        } else if (id.equalsIgnoreCase("wind.y")) {
            iTimeY = -1;
        } else if (id.equalsIgnoreCase("wind.xy")) {
            iTimeXY = -1;
        }
    }

	
	public void copyValuesFromItem(IExchangeItem sourceItem) {
		throw new UnsupportedOperationException("org.openda.model_swan.SwanField2DXYFileExchangeItem.copyValuesFromItem(): Not implemented yet.");
	}

	
	public ITimeInfo getTimeInfo() {
		throw new UnsupportedOperationException("org.openda.model_swan.SwanField2DXYFileExchangeItem.getTimeInfo(): Not implemented yet.");
	}

	
	public IQuantityInfo getQuantityInfo() {
		throw new UnsupportedOperationException("org.openda.model_swan.SwanField2DXYFileExchangeItem.getQuantityInfo(): Not implemented yet.");
	}

	
	public IGeometryInfo getGeometryInfo() {
		return null;
	}
}
