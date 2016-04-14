package org.openda.model_delft3d;

import org.openda.exchange.dataobjects.NetcdfDataObject;
import org.openda.interfaces.IExchangeItem;

import java.io.File;

/**
 * Netcdf Data object for reading time series from Delft3D trih-* files
 */
public class D3dNetcdfHisDataObject extends NetcdfDataObject {

	public D3dNetcdfHisDataObject() {
		stationIdVarName = "NAMST";
		stationDimensionVarName = "NOSTAT";
	}

	public void initialize(File workingDir, String[] arguments) {
		super.initialize(workingDir, arguments);
		// For now, remove the variable that are not scalar (but a vector along the vertical axis)
		// TODO: split in EI per layer?
		// Also remove MNSTAT (time dependent monitoring station location)
		String[] nonScalarVariables = new String[] { "ZCURU", "ZCURV","ZQXK", "ZQYK","MNSTAT"};
		for (String nonScalarVariable : nonScalarVariables) {
			for (IExchangeItem exchangeItem: exchangeItems) {
				if (exchangeItem.getId().equalsIgnoreCase(nonScalarVariable)) {
					exchangeItems.remove(exchangeItem);
					break;
				}
			}
		}
	}
}
