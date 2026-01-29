package org.openda.model_hec_hms;

import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;

import java.io.File;

public class ControlFile implements IDataObject {
	@Override
	public String[] getExchangeItemIDs() {
		return new String[0];
	}

	@Override
	public String[] getExchangeItemIDs(IExchangeItem.Role role) {
		return new String[0];
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return null;
	}

	@Override
	public void finish() {

	}

	@Override
	public void initialize(File workingDir, String[] arguments) {

	}
}
