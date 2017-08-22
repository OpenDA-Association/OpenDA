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
package org.openda.model_glm;

import org.openda.exchange.timeseries.TimeSeries;

import ucar.nc2.NetcdfFile;

/**
 * ExchangeItem for SimonaNetcdfFile which is used to pass the netcdf result file itself
 */
public class GlmNetcdfFileTimeSeries extends TimeSeries {

    private NetcdfFile ncFile;


    public GlmNetcdfFileTimeSeries(String id, NetcdfFile netcdfFile) {
        this.ncFile = netcdfFile;
    }

    
    public Role getRole() {
        return Role.Output;
    }

    
    public Class getValueType() {
        return NetcdfFile.class;
    }
    
    public ValueType getValuesType() {
        throw new RuntimeException("SimonaNetcdfFileExchangeItem: unsupported ValueType getValuesType()");
    }
    

    
    public Object getValues() {
        return ncFile;
    }

    
    public double[] getValuesAsDoubles() {
        return null;
    }

    
    public void axpyOnValues(double alpha, double[] axpyValues) {
        throw new RuntimeException(this.getClass().getName() + ": axpyOnValues is not implemented");
    }

    
    public void multiplyValues(double[] multiplicationFactors) {
        throw new RuntimeException(this.getClass().getName() + ": multiplyValues is not implemented");
    }

    
    public void setValues(Object values) {
        if (values instanceof NetcdfFile) {
            this.ncFile = (NetcdfFile) values;
            return;
        }

        throw new RuntimeException(this.getClass().getName() + ": cannot digest this type to set values");
    }

    
    public void setValuesAsDoubles(double[] values) {
        //do nothing
    }

    
    public double[] getTimes() {
        return null;
    }

    
    public void setTimes(double[] times) {
        throw new RuntimeException(this.getClass().getName() + ": setTimes is not implemented");
    }

}
