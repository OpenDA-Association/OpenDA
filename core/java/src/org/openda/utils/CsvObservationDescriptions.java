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

package org.openda.utils;

import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.ITime;
import org.openda.interfaces.IVector;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Observation Descriptions
 */
public class CsvObservationDescriptions implements IObservationDescriptions {

	//Class fields
	CsvStochObserver obs = null;

	public CsvObservationDescriptions(){
		//TODO empty ObservationDescriptions
	}

	public CsvObservationDescriptions(CsvStochObserver obs){
       this.obs = obs;
	}

    public List<IPrevExchangeItem> getExchangeItems() {
        List<IPrevExchangeItem> exchangeItems = new ArrayList<IPrevExchangeItem>();
        IVector valueVector = obs.values[obs.valueInd];
        IVector timesVector = obs.values[obs.timeInd];
        IVector locIds = obs.values[obs.locInd];
        String[] obsIds = obs.getIds();
        // sort per Id
        HashMap<String,java.util.Vector<Double>> allValues = new HashMap<String,java.util.Vector<Double>>();          
        HashMap<String,java.util.Vector<Double>> allTimes = new HashMap<String,java.util.Vector<Double>>();          
        for (int i = 0; i < obsIds.length; i++) {
        	allValues.put(obsIds[i], new java.util.Vector<Double>());
        	allTimes.put(obsIds[i], new java.util.Vector<Double>());
        }
        for(int i=0;i<valueVector.getSize();i++){
        	String thisId = String.valueOf(locIds.getValue(i));
        	allValues.get(thisId).add(valueVector.getValue(i));
        	allTimes.get(thisId).add(timesVector.getValue(i));
        }
        for (int i = 0; i < obsIds.length; i++) {
        	java.util.Vector<Double> valuesForSeries = allValues.get(obsIds[i]);
        	java.util.Vector<Double> timesForSeries = allTimes.get(obsIds[i]);
        	TimeSeries series = new TimeSeries(vectorToNative(timesForSeries),vectorToNative(valuesForSeries));
            series.setId(obsIds[i]);
            series.setLocation(obsIds[i]);
            exchangeItems.add(series);
        }
        return exchangeItems;
    }

    /** Get properties (values) that correspond to a given key.
	 *
	 * @param key        I  key for which the value is asked
	 * @return Properties (column of data from observation descriptions)
	 */
	public IVector getValueProperties(String key){
		return this.obs.getValueProperties(key);
	}

	/** Get properties (strings) that correspond to a given key.
	 *
	 * @param key        I  key for which the value is asked
	 * @return Properties (column of data from observation descriptions)
	 */
	public String[] getStringProperties(String key){
        if (key.equalsIgnoreCase("id") ||
                key.equalsIgnoreCase("index") ||
                key.equalsIgnoreCase("loc") ||
                key.equalsIgnoreCase("location") ||
                key.equalsIgnoreCase("station")) {
            return this.obs.getIds();
        }
        throw new RuntimeException("\"" + key + "\" is not a string property");
	}

	/** Get names of all keys.
	 *
	 * @return error status: All keys of the observation descriptions
	 */
	public String[] getPropertyKeys(){
		return this.obs.CTA_ObsDescr_Get_Keys();
	}

	/** Get number of properties/keys.
	 *
	 * @return number of properties
	 */
	public int getPropertyCount(){
		return this.obs.getPropertyCount();
	}

	/** Get number of observations.
	 *
	 *noKeys @return number of observations
	 */
	public int getObservationCount(){
		return this.obs.getObservationCount();
	}

	public ITime[] getTimes() {
		return obs.getTimes();
	}

	/**
     * Give string representation of the data
     * @return StochOsberver as a string
     */
    public String toString(){
    	String result = "ObservationDescriptions{";
    	result += this.obs.toString();
    	result += "\n}\n";
    	return result;
    }

    private static double[] vectorToNative(java.util.Vector<Double> input){
    	int n = input.size();
    	double[] result = new double[n];
    	for(int i=0;i<n;i++){
    		result[i] = input.get(i);
    	}
    	return result;
    }

}

