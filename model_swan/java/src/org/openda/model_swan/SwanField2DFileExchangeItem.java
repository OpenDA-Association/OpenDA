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

package org.openda.model_swan;

import org.openda.interfaces.IPrevExchangeItem;

/**
 * Exchange item of SWAN with scalar 2D field data format.
 */
public class SwanField2DFileExchangeItem implements IPrevExchangeItem {
    private String id;
    private SwanField2DFile swanField2DFile;
    private Integer iTime = -1;

    public SwanField2DFileExchangeItem(String id, SwanField2DFile swanField2DFile){
        this.id = id;
        this.swanField2DFile = swanField2DFile;
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

    public IPrevExchangeItem.Role getRole(){
        return IPrevExchangeItem.Role.InOut;
    }

    public Object getValues() {
        return getValuesAsDoubles();
    }

    public double[] getValuesAsDoubles() {
        double[] obValues = null;
        int nTimeMax = swanField2DFile.getNTimes();
//        if (id.equalsIgnoreCase("wind.x")) {
            iTime++;
            if (iTime == swanField2DFile.getNTimes()){
                throw new RuntimeException("Data at iTime>"+nTimeMax+" is not available: org.openda.model_swan.SwanWindFileExchangeItem.getValuesAsDoubles");
            }
            obValues = swanField2DFile.getValueField2D(iTime);
//        } else if (id.equalsIgnoreCase("wind.y")) {
//            iTimeY++;
//            if (iTimeY==swanField2DFile.getNTimes()){
//                throw new RuntimeException("Data at iTime>"+nTimeMax+" is not available: org.openda.model_swan.SwanWindFileExchangeItem.getValuesAsDoubles");
//            }
//            obValues = swanField2DFile.getYComp(iTimeY);
//        } else if (id.equalsIgnoreCase("wind.xy")) {
//            iTimeXY++;
//            if (iTimeY==swanField2DFile.getNTimes()){
//                throw new RuntimeException("Data at iTime>"+nTimeMax+" is not available: org.openda.model_swan.SwanWindFileExchangeItem.getValuesAsDoubles");
//            }
//            obValues = swanField2DFile.getXYComp(iTimeXY);
//        }
        return obValues;
    }

    public void axpyOnValues(double alpha, double[] axpyValues) {
//        if (id.equalsIgnoreCase("wind.x")) {
            swanField2DFile.axpyField2D(iTime,alpha,axpyValues);
//        } else if (id.equalsIgnoreCase("wind.y")) {
//            swanField2DFile.axpyYComp(iTimeY,alpha,axpyValues);
//        } else if (id.equalsIgnoreCase("wind.xy")) {
//            swanField2DFile.axpyXYComp(iTimeXY,alpha,axpyValues);
//        }
    }

    public void multiplyValues(double[] multiplicationFactors) {
//        if (id.equalsIgnoreCase("wind.x")) {
            swanField2DFile.multiplyField2D(iTime,multiplicationFactors);
//        } else if (id.equalsIgnoreCase("wind.y")) {
//            swanField2DFile.multiplyYComp(iTimeY,multiplicationFactors);
//        } else if (id.equalsIgnoreCase("wind.xy")) {
//            swanField2DFile.multiplyXYComp(iTimeXY,multiplicationFactors);
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
            swanField2DFile.setField2D(iTime,values);
//        } else if (id.equalsIgnoreCase("wind.y")) {
//            swanField2DFile.setYComp(iTimeY,values);
//        } else if (id.equalsIgnoreCase("wind.xy")) {
//            swanField2DFile.setXYComp(iTimeXY,values);
//        }
    }

    public double[] getTimes() {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanStateFileExchangeItem.getTimes(): Not implemented yet.");
    }

    public void setTimes(double[] times) {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanStateFileExchangeItem.setTimes(): Not implemented yet.");
    }

    public void resetTimeCounter() {
//        if (id.equalsIgnoreCase("wind.x")) {
            iTime = -1;
//        } else if (id.equalsIgnoreCase("wind.y")) {
//            iTimeY = -1;
//        } else if (id.equalsIgnoreCase("wind.xy")) {
//            iTimeXY = -1;
//        }
    }

}

