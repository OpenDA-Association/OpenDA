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

package org.openda.interfaces;

/**
 * Interface for the generic io-object. This interface is needed to give a single interface to
 * various sources of io: ascii-files, model specific binary files, databases, etc..
 */
public interface IDataObject extends IConfigurable {

    /**
     * Get the identifiers of the exchange items that can be retrieved from and set to the model.
     * Should return String[0] if there are no items.
     * @return The array of exchange item identifiers.
     */
    String [] getExchangeItemIDs();

    /**
     * Get the identifiers of the exchange items that can be retrieved from and set to the model,
     * according to the specified role (input, output, both)
     * Should return String[0] if there are no matching items.
     * @param role Input, Output, or InOut (i.e. both)
     * @return The array of exchange item identifiers.
     */
    String [] getExchangeItemIDs(IPrevExchangeItem.Role role);

	/**
	 * Get the exchange item specified by <c>exchangeItemID</c>.
	 * Returns null if no exchangeItem with the given exchangeItemID is found.
	 *
	 * @param exchangeItemID The exchange item identifier.
	 * @return The required exchange item.
	 */
	IExchangeItem getDataObjectExchangeItem(String exchangeItemID);

    /**
     * Shut down this dataObject. Typically this implies flushing output, closing files and network connections.
     * It is also possible that work is done directly after modification of the exchangeItems, so then no work
     * may be left.
     */
    void finish();

}
