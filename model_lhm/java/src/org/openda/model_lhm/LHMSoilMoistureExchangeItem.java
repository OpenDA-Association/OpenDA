/* OpenDA v2.3.1 
* Copyright (c) 2016 OpenDA Association 
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

package org.openda.model_lhm;

import org.openda.interfaces.*;

import java.io.IOException;
/**
 * Exchange item for reading and writing soil moisture from the grid and to the init_svat.file.
 */
public class LHMSoilMoistureExchangeItem implements IExchangeItem {
    private LHMSoilMoisture lhmSoilMoisture;
    private String id;
    private String[] ids = new String[] {"soilmoisture"};

    public LHMSoilMoistureExchangeItem(String id, LHMSoilMoisture lhmSoilMoisture) {
        this.id = id;
        this.lhmSoilMoisture = lhmSoilMoisture;
    }

    public String getDescription() {
        return null;
    }
    public Class getValueType() {
        throw new UnsupportedOperationException("org.openda.model_lhm.LHMSoilMoistureExchangeItem.getValueType(): Not implemented yet.");
    }
    public ValueType getValuesType() {
        return ValueType.doublesType;
    }
    public Role getRole() {
        throw new UnsupportedOperationException("org.openda.model_lhm.LHMSoilMoistureExchangeItem.getRole(): Not implemented yet.");
    }
    public void multiplyValues(double[] multiplicationFactors) { throw new UnsupportedOperationException("org.openda.model_lhm.LHMSoilMoistureExchangeItem.multiplyValues(): Not implemented yet."); }
    public void setTimes(double[] times) { throw new UnsupportedOperationException("org.openda.model_lhm.LHMSoilMoistureExchangeItem.setTimes(): Not implemented yet.");}
    public IQuantityInfo getQuantityInfo() { throw new UnsupportedOperationException("org.openda.model_lhm.LHMSoilMoistureExchangeItem.getQuantityInfo(): Not implemented yet.");  }
    public ITimeInfo getTimeInfo() { return null; }

    public void copyValuesFromItem(IExchangeItem sourceItem) { throw new UnsupportedOperationException("org.openda.model_swan.SwanOpenBoundaryTPARFileExchangeItem.copyValuesFromItem(): Not implemented yet."); }
    public IGeometryInfo getGeometryInfo() {  return null; }

    public String getId() {
        return id;
    }

	public double[] getValues(){
            double[] avMoist = getValuesAsDoubles();
            return avMoist;
    }

    public double[] getValuesAsDoubles() {
        double avActMoist = lhmSoilMoisture.getMoisture();
		double[] avMoistModel = new double[]{avActMoist};
        return avMoistModel;
    }

    public double[] getTimes() {
        double tStop = lhmSoilMoisture.readStopTimeFromFile();
        double[] stopTime = new double[]{tStop};
        return(stopTime);
    }

    public void setValues(Object values) {
        if (!(values instanceof double[])) {
            throw new RuntimeException("SetValues for" + this.getId() + ": unexpected object type: " + values.getClass().getName());
        }
        setValuesAsDoubles((double[]) values);
    }

    public void setValuesAsDoubles(double[] values) {
        if (id.contentEquals(ids[0])) {
            lhmSoilMoisture.setMoisture(values);
        } else {
            throw new UnsupportedOperationException("org.openda.model_lhm.LHMSoilMoistureExchangeItem.setValuesAsDoubles(): ID is not correct.");
        }
    }

    public void axpyOnValues(double alpha, double[] axpyValues) {
	    //throw new UnsupportedOperationException("org.openda.model_lhm.LHMSoilMoistureExchangeItem.axpyOnValues(): Not implemented yet.");
        System.out.println("axpy with "+alpha+" / "+axpyValues.length);
		/*try {
			lhmSoilMoisture.axpyOnAllState(alpha,axpyValues);
		} catch (IOException e) {
			throw new RuntimeException("org.openda.model_lhm.lhmSoilMoistureExchangeItem.axpyOnValues(): not successful.");
		}*/
    }

}
