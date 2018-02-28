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

/**
 * Exchange item for reading and writing simulation time information of SWAN.
 */
public class SwanTimeInfoExchangeItem implements IExchangeItem {
    private SwanTimeInfo swanTimeInfo;
    private String id;

    public SwanTimeInfoExchangeItem(String id, SwanTimeInfo swanTimeInfo) {
        this.id = id;
        this.swanTimeInfo = swanTimeInfo;
    }

    public String getId() {
        return id;
    }

    public String getDescription() {
        return null; // no description
    }

    public Class getValueType() {
        return double.class;
    }

    public ValueType getValuesType() {
        return ValueType.doubleType;
    }

    public Role getRole() {
        return IPrevExchangeItem.Role.InOut;
    }

    public Object getValues() {
        double time;
        if (this.id.equals("start_time")) {
            time = swanTimeInfo.getDblTStartSimulation();
        } else if (this.id.equals("end_time")) {
            time = swanTimeInfo.getDblTStopSimulation();
        } else if (this.id.equals("time_step")) {
            time = swanTimeInfo.getDblTStepSimulationMJD();
        } else {
            throw new RuntimeException("SwanTimeInfo exchangeItemId "+this.id+" is not supported. Possible ids are start_time, end_time, and time_step.");
        }
        return time;
    }

    public double[] getValuesAsDoubles() {
        return new double[]{(Double)getValues()};
    }

    public void axpyOnValues(double alpha, double[] axpyValues) {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanTimeInfoExchangeItem.axpyOnValues(): Not implemented yet.");
    }

    public void multiplyValues(double[] multiplicationFactors) {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanTimeInfoExchangeItem.multiplyValues(): Not implemented yet.");
    }

    public void setValues(Object values) {
        if (this.id.equals("start_time")) {
          swanTimeInfo.setDblTStartSimulation((Double)values);
        } else {
          swanTimeInfo.setDblTStopSimulation((Double)values);
        }
    }

    public void setValuesAsDoubles(double[] values) {
        if (this.id.equals("start_time")) {
            swanTimeInfo.setDblTStartSimulation((Double)values[0]);
        } else {
            swanTimeInfo.setDblTStopSimulation((Double)values[0]);
        }
    }

    public double[] getTimes() {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanTimeInfoExchangeItem.getTimes(): Not implemented yet.");
    }

    public void setTimes(double[] times) {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanTimeInfoExchangeItem.setTimes(): Not implemented yet.");
    }

	
	public void copyValuesFromItem(IExchangeItem sourceItem) {
		throw new UnsupportedOperationException("org.openda.model_swan.SwanTimeInfoExchangeItem.copyValuesFromItem(): Not implemented yet.");
	}

	
	public ITimeInfo getTimeInfo() {
		throw new UnsupportedOperationException("org.openda.model_swan.SwanTimeInfoExchangeItem.getTimeInfo(): Not implemented yet.");
	}

	
	public IQuantityInfo getQuantityInfo() {
		throw new UnsupportedOperationException("org.openda.model_swan.SwanTimeInfoExchangeItem.getQuantityInfo(): Not implemented yet.");
	}

	
	public IGeometryInfo getGeometryInfo() {
		return null;
	}
}
