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

import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;

/**
 * Exchange item for reading and writing LHM main input (para_sim.inp) file.
 */
public class LHMTimeSettingsExchangeItem implements IExchangeItem {
    private LHMTimeSettings lhmTimeSettings;
    private String id;

    public LHMTimeSettingsExchangeItem(String id, LHMTimeSettings lhmTimeSettings) {
        this.id = id;
        this.lhmTimeSettings = lhmTimeSettings;
    }

    public String getDescription() {
        return null;
    }

    public void axpyOnValues(double alpha, double[] axpyValues) {
        throw new UnsupportedOperationException("org.openda.model_lhm.LHMTimeSettingsExchangeItem.axpyOnValues(): Not implemented yet.");
    }

    public double[] getTimes() {
        throw new UnsupportedOperationException("org.openda.model_lhm.LHMTimeSettingsExchangeItem.getTimes(): Not implemented yet.");
    }

    public void setTimes(double[] times) {
        throw new UnsupportedOperationException("org.openda.model_lhm.LHMTimeSettingsExchangeItem.setTimes(): Not implemented yet.");
    }

    public void multiplyValues(double[] multiplicationFactors) {
        throw new UnsupportedOperationException("org.openda.model_lhm.LHMTimeSettingsExchangeItem.multiplyValues(): Not implemented yet.");
    }

    public IQuantityInfo getQuantityInfo() {
        throw new UnsupportedOperationException("org.openda.model_lhm.LHMTimeSettingsExchangeItem.getQuantityInfo(): Not implemented yet.");
    }

    public void copyValuesFromItem(IExchangeItem sourceItem) {
        throw new UnsupportedOperationException("org.openda.model_lhm.LHMTimeSettingsExchangeItem.copyValuesFromItem(): Not implemented yet.");
    }

    public IGeometryInfo getGeometryInfo() {
        return null;
    }

    public ITimeInfo getTimeInfo() {
        return null;
    }

    public String getId() {
        return id;
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


    /* public double[] getTimes() {
        double[] times = new double[2];
        times[0] = lhmTimeSettings.getStartTime();
        times[1] = lhmTimeSettings.getStopTime(times[0]);
        return times;
    }*/

    public double[] getValuesAsDoubles() {
        return new double[]{(Double) getValues()};
    }

    public Object getValues() {
		System.out.println("Get values...");

        double time = 0.0;
        double[] times = lhmTimeSettings.convertTimesToDouble();
        if (this.id.equals("start_time")) {
            time = times[0];
        } else {
            time = times[1];
        }
        return time;
    }

    public void setValues(Object values) {
		  if (this.id.equals("start_time")) {
			  lhmTimeSettings.convertTimeToString((double) values, this.id );
        //    lhmTimeSettings.setStartTime((Double) values);
        //    lhmTimeSettings.setStartDate((Double) values);
        } else {
			  lhmTimeSettings.convertTimeToString((double) values, this.id );
		  }
         //   lhmTimeSettings.setStopTime((Double) values);
        //}
        lhmTimeSettings.finish();
    }

    //set times
    public void setValuesAsDoubles(double[] values) {
		System.out.println("Setvalues...");
		if (this.id.equals("start_time")) {
			lhmTimeSettings.convertTimeToString(values[0], this.id);
		}else {
			lhmTimeSettings.convertTimeToString(values[0], this.id);
		}
    }
}

