package org.openda.exchange.dataobjects;

import java.io.File;

/**
 * Netcdf Data object for reading time series from e.g. Sobek3 time series file
 */
public class NetcdfDataScalarTimeSeriesDataObject extends NetcdfDataObject {

	public NetcdfDataScalarTimeSeriesDataObject() {
	}

	public void initialize(File workingDir, String[] arguments) {
		if (arguments.length != 2 && arguments.length != 3) {
			throw new RuntimeException("NetcdfDataScalarTimeSeriesDataObject.initialize expects workingDir and arguments: " +
					"ncfile timeseriesIdVarName [timeseriesSizeVarname]");
		}
		String ncFileName = arguments[0];
		this.stationIdVarName = arguments[1];
		this.stationDimensionVarName = arguments.length == 3 ? arguments[2] : "id";
		super.initialize(workingDir, new String[] {ncFileName, "true"} );
	}
}
