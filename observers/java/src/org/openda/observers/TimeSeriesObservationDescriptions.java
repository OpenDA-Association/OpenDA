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
import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.ITime;
import org.openda.interfaces.IVector;
import org.openda.interfaces.IPrevExchangeItem;
import java.util.List;

public class TimeSeriesObservationDescriptions implements IObservationDescriptions {

	//Class fields
	public TimeSeriesStochObserver obs = null;

	public TimeSeriesObservationDescriptions(){
		//TODO empty ObservationDescriptions
	}

	public TimeSeriesObservationDescriptions(TimeSeriesStochObserver obs){
       this.obs = obs;
	}

    @Override
    public List<IPrevExchangeItem> getExchangeItems() {
        return obs.getExchangeItems();
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
	 * @param Key        I  key for which the value is asked
	 * @return Properties (column of data from observation descriptions)
	 */
	public String[] getStringProperties( String Key){
		return this.obs.getStringProperties(Key);
	}

	/** Get names of all keys.
	 *
	 * @return error status: All keys of the observation descriptions
	 */
	public String[] getPropertyKeys(){
		return this.obs.getPropertyKeys();
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
		return this.obs.getCount();
	}

	public ITime[] getTimes() {
		return obs.getTimes();
	}

	/**
     * Give string representation of the data
     * @return StochObserver as a string
     */
    public String toString(){
    	String result = "ObservationDescriptions(\n";
    	result += this.obs.toString() + "\n";
    	result += ")\n";
    	return result;
    }


}
