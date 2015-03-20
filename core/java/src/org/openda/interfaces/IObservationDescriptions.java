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

package org.openda.interfaces;

import java.io.Serializable;
import java.util.List;


/**
 * Observation Descriptions.
 */
public interface IObservationDescriptions extends Serializable{

    /** Get the exchange items describing the measures available in the stoch. observer.
     * @return All exchange items in the stoch. observer.
     */
    List<IPrevExchangeItem> getExchangeItems();

	/** Get properties (values) that correspond to a given key.
	 *
	 * @param Key key for which the value is asked
	 * @return Properties (column of data from observation descriptions).
	 */
	IVector getValueProperties( String Key);

	/** Get properties (strings) that correspond to a given key.
	 *
	 * @param Key key for which the value is asked
	 * @return Properties (column of data from observation descriptions).
	 */
	String[] getStringProperties( String Key);

	/** Get names of all keys
     *
	 * @return All keys of the observation descriptions.
	 */
	String[] getPropertyKeys();

	/** Get number of properties/keys.
	 *
	 * @return Number of properties.
	 */
	int getPropertyCount();

	/** Get number of observations.
	 *
	 * @return Number of observations.
	 */
	int getObservationCount();

	/**
	 * Get all different times in increasing order. There is at least one observation for each time.
	 * It is likely that observer.createSelection(time[i]) will be used to walk through the
	 * observations. The implementation of the stochobserver should garantee that al observations are
	 * returned in exactly one batch this way.
	 *
	 * @return Array with all uniquely different times.
	 */
	public ITime[] getTimes();

}

