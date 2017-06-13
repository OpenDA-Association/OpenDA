/* OpenDA v2.4 
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
 * Interface that can be implemented by DataObjects or IoObjects.
 * This interface contains methods to get ensemble exchange items (i.e. exchange items for which there are ensemble members available).
 *
 *
 * IMPORTANT NOTES:
 *
 * If a DataObject implements this interface, then the following methods should ignore any ensemble exchange items:
 * getExchangeItemIDs()
 * getExchangeItemIDs(IPrevExchangeItem.Role role)
 *
 * If a DataObject implements this interface, then the following method should throw an IllegalStateException when it is called for an ensemble exchange item:
 * getDataObjectExchangeItem(String exchangeItemID)
 * This IllegalStateException should state that the method getDataObjectExchangeItem(String exchangeItemId, int ensembleMemberIndex) should be called instead.
 *
 * If an IoObject implements this interface, then the following method should ignore any ensemble exchange items:
 * getExchangeItems()
 *
 * @author Arno Kockx
 */
public interface IEnsembleDataObject {

	/**
	 * Get the ensemble member indices of the ensemble exchange items.
	 * The ensemble member indices must be the same for all ensemble exchange items.
	 * This should ignore any exchange items for which there are no ensemble members available.
	 * Should return int[0] if there are no ensemble members.
	 *
	 * @return array of ensemble member indices.
	 */
	int[] getEnsembleMemberIndices();

	/**
	 * Get the identifiers of the ensemble exchange items.
	 * This should ignore any exchange items for which there are no ensemble members available.
	 * Should return String[0] if there are no matching ensemble items.
	 *
	 * @return array of ensemble exchange item identifiers.
	 */
	String[] getEnsembleExchangeItemIds();

	/**
	 * Get the ensemble exchange item specified by the given exchangeItemId and ensembleMemberIndex.
	 * If the given ensembleMemberIndex does not exist, then this method should throw an IllegalStateException.
	 * If there are no ensemble members available for the given exchangeItem, then it should throw an
	 * IllegalStateException stating that the equivalent method without the argument "int ensembleMemberIndex" must be called instead.
	 * Returns null if no ensemble exchange item with the given exchangeItemId is found.
	 *
	 * @param exchangeItemId ensemble exchange item identifier.
	 * @param ensembleMemberIndex ensemble member index.
	 * @return the requested ensemble exchange item.
	 */
	IExchangeItem getDataObjectExchangeItem(String exchangeItemId, int ensembleMemberIndex);
}
