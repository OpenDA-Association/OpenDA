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
