package org.openda.exchange;

import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;

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
		return getExchangeItemIDs();
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return this.exchangeItems.get(exchangeItemID);
	}

}
