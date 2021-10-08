package org.openda.model_wflow;

import org.openda.exchange.AbstractDataObject;
import org.openda.exchange.NetcdfGridTimeSeriesExchangeItem;
import org.openda.exchange.dataobjects.NetcdfDataObject;
import org.openda.interfaces.IComposableDataObject;
import org.openda.interfaces.IComposableEnsembleDataObject;
import org.openda.interfaces.IExchangeItem;

import java.io.File;

public class WflowJuliaStateFile extends AbstractDataObject {

	private NetcdfDataObject netcdfDataObject;

	public WflowJuliaStateFile() {}



/*	@Override
	public void addExchangeItem(IExchangeItem item) {
		netcdfDataObject.addExchangeItem(item);
	}

	@Override
	public void addExchangeItem(IExchangeItem exchangeItem, int ensembleMemberIndex) {
		netcdfDataObject.addExchangeItem(exchangeItem, ensembleMemberIndex);
	}*/

	@Override
	public void initialize(File workingDir, String[] arguments) {
		netcdfDataObject = new NetcdfDataObject();
		netcdfDataObject.initialize(workingDir, arguments);
		String[] exchangeItemIDs = netcdfDataObject.getExchangeItemIDs();
		for (String exchangeItemID : exchangeItemIDs) {
			IExchangeItem exchangeItem = netcdfDataObject.getDataObjectExchangeItem(exchangeItemID);
			if (exchangeItem instanceof NetcdfGridTimeSeriesExchangeItem) {
				WflowJuliaStateExchangeItem wflowJuliaStateExchangeItem = new WflowJuliaStateExchangeItem((NetcdfGridTimeSeriesExchangeItem) exchangeItem);
				exchangeItems.put(exchangeItemID, wflowJuliaStateExchangeItem);
				continue;
			}
			exchangeItems.put(exchangeItemID, exchangeItem);
		}

	}

	@Override
	public void finish() {
		netcdfDataObject.finish();
	}

/*	@Override
	public int[] getEnsembleMemberIndices() {
		return netcdfDataObject.getEnsembleMemberIndices();
	}

	@Override
	public String[] getEnsembleExchangeItemIds() {
		return netcdfDataObject.getEnsembleExchangeItemIds();
	}

	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemId, int ensembleMemberIndex) {
		return netcdfDataObject.getDataObjectExchangeItem(exchangeItemId, ensembleMemberIndex);
	}*/
}
