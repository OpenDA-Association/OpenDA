package org.openda.exchange.dataobjects;

/**
 * Netcdf Data object for reading time series from Delft3D trih-* files
 */
public class NetcdfDataObjectDelft3DTrih extends NetcdfDataObject {

	public NetcdfDataObjectDelft3DTrih() {
		stationIdVarName = "NAMST";
		stationDimensionVarName = "NOSTAT";
	}
}
