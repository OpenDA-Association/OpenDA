/*
* Copyright (c) 2021 OpenDA Association 
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
package org.openda.exchange;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

public abstract class AbstractDataObject implements IDataObject {

	public static final String[] EMPTY_STRING_ARRAY = new String[0];
	protected Map<String, IExchangeItem> exchangeItems = new LinkedHashMap<>();

	@Override
	public String[] getExchangeItemIDs() {
		return this.exchangeItems.keySet().toArray(EMPTY_STRING_ARRAY);
	}

	@Override
	public String[] getExchangeItemIDs(IExchangeItem.Role role) {
		Collection<IExchangeItem> exchangeItems = this.exchangeItems.values();
		ArrayList<String> idsForRole = new ArrayList<>();
		for (IExchangeItem exchangeItem : exchangeItems) {
			IExchangeItem.Role exchangeItemRole = exchangeItem.getRole();
			if (exchangeItemRole.equals(role)) idsForRole.add(exchangeItem.getId());
			// If the role is InOut, then we are done.
			if (role.equals(IExchangeItem.Role.InOut)) continue;
			// For roles In and Out,  we also add the exchangeItemIds for InOut to the result.
			if (exchangeItemRole.equals(IExchangeItem.Role.InOut)) idsForRole.add(exchangeItem.getId());
		}
		return idsForRole.toArray(EMPTY_STRING_ARRAY);
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return this.exchangeItems.get(exchangeItemID);
	}

}
