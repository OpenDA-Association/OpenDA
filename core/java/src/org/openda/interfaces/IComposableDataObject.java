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
public interface IComposableDataObject extends IDataObject {

	/**
	 * Add a <b>copy</b> of exchangeItem to this dataObject. Since a dataObject owns its exchangeItems, it is
	 * possible that the actual work is postponed until the finish method is called, so modification of an
	 * exchangeItem after adding it, but before calling finish, may alter the outcome.
	 * Throws an UnsupportedOperationException when the dataObject does not support the addition of items.
	 * A RuntimeException is thrown if the type of data can not be handled. Note that in general, it is also
	 * possible that the type of echchangeItem can be handled, but with degraded meta data.
	 *
	 * @param item  exchangeItem to be duplicated
	 */
	void addExchangeItem(IExchangeItem item);
}
