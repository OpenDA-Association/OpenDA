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

/**
 * Interface that can be implemented by DataObjects or IoObjects.
 * This interface contains methods to get exchange items for a specific ensemble member.
 *
 * IMPORTANT NOTE:
 * If a DataObject or an IoObject implements this interface, then the following methods
 * should throw an IllegalStateException whenever the method getEnsembleMemberCount() returns a number > 1:
 * getExchangeItemIDs()
 * getExchangeItemIDs(IPrevExchangeItem.Role role)
 * getDataObjectExchangeItem(String exchangeItemID)
 * This IllegalStateException should state that the equivalent method with an added argument "String ensembleMemberId" must be called instead.
 */
public interface IEnsembleDataObject {

	/**
	 * Get the number of ensemble members for which exchange items can be retrieved.
	 * Should return 0 if there are no ensemble members.
	 *
	 * @return number of ensemble members.
	 */
	int getEnsembleMemberCount();

	/**
	 * Get the identifiers of the ensemble members for which exchange items can be retrieved.
	 * Should return String[0] if there are no ensemble members.
	 *
	 * @return array of ensemble member identifiers.
	 */
	String[] getEnsembleMemberIds();

	/**
	 * Get the identifiers of the exchange items that can be retrieved for the given ensembleMemberId.
	 * Should return String[0] if there are no matching items.
	 *
	 * @param ensembleMemberId ensemble member identifier.
	 * @return array of exchange item identifiers.
	 */
	String[] getExchangeItemIds(String ensembleMemberId);

	/**
	 * Get the identifiers of the exchange items that can be retrieved for the given ensembleMemberId
	 * according to the specified role (input, output, both).
	 * Should return String[0] if there are no matching items.
	 *
	 * @param role Input, Output, or InOut (i.e. both)
	 * @param ensembleMemberId ensemble member identifier.
	 * @return The array of exchange item identifiers.
	 */
	String[] getExchangeItemIds(IPrevExchangeItem.Role role, String ensembleMemberId);

	/**
	 * Get the exchange item specified by the given exchangeItemId and ensembleMemberId.
	 * Returns null if no exchangeItem with the given exchangeItemId and ensembleMemberId is found.
	 *
	 * @param exchangeItemId exchange item identifier.
	 * @param ensembleMemberId ensemble member identifier.
	 * @return the requested exchange item.
	 */
	IExchangeItem getDataObjectExchangeItem(String exchangeItemId, String ensembleMemberId);
}
