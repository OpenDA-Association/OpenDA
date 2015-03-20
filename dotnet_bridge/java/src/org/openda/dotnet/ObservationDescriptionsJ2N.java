package org.openda.dotnet;
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

import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.ITime;
import org.openda.interfaces.IVector;

import java.util.List;

/**
 * Author Nils van Velzen
 * Bridge to use C# observationDescriptions in java
 */
public class ObservationDescriptionsJ2N implements IObservationDescriptions {

	private cli.OpenDA.DotNet.Interfaces.IObservationDescriptions _dotNetObsDescr;

    ObservationDescriptionsJ2N(cli.OpenDA.DotNet.Interfaces.IObservationDescriptions dotNetObsDescr){
		_dotNetObsDescr = dotNetObsDescr;
	}

	/**
	 * Get the exchange items describing the measures available in the stoch. observer.
	 *
	 * @return All exchange items in the stoch. observer.
	 */
	public List<IPrevExchangeItem> getExchangeItems() {
		throw new UnsupportedOperationException("Not implemented yet.");
	}

	/**
	 * Get properties (values) that correspond to a given key.
	 *
	 * @param Key key for which the value is asked
	 * @return Properties (column of data from observation descriptions).
	 */
	public IVector getValueProperties(String Key) {
		throw new UnsupportedOperationException("Not implemented yet.");
	}

	/**
	 * Get properties (strings) that correspond to a given key.
	 *
	 * @param Key key for which the value is asked
	 * @return Properties (column of data from observation descriptions).
	 */
	public String[] getStringProperties(String Key) {
		throw new UnsupportedOperationException("Not implemented yet.");
	}

	/**
	 * Get names of all keys
	 *
	 * @return All keys of the observation descriptions.
	 */
	public String[] getPropertyKeys() {
		throw new UnsupportedOperationException("Not implemented yet.");
	}

	/**
	 * Get number of properties/keys.
	 *
	 * @return Number of properties.
	 */
	public int getPropertyCount() {
		throw new UnsupportedOperationException("Not implemented yet.");
	}

	/**
	 * Get number of observations.
	 *
	 * @return Number of observations.
	 */
	public int getObservationCount() {
		throw new UnsupportedOperationException("Not implemented yet.");
	}

	/**
	 * Get all different times in increasing order. There is at least one observation for each time.
	 * It is likely that observer.createSelection(time[i]) will be used to walk through the
	 * observations. The implementation of the stochobserver should garantee that al observations are
	 * returned in exactly one batch this way.
	 *
	 * @return Array with all uniquely different times.
	 */
	public ITime[] getTimes() {
		throw new UnsupportedOperationException("Not implemented yet.");
	}
}
