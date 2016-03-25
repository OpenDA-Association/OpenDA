package org.openda.exchange.dataobjects;

import org.openda.exchange.QuantityInfo;
import org.openda.interfaces.*;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Data object voor D3D map file
 */
public class NetcdfD3dMapDataObject implements IDataObject {

	NetcdfFile netcdfFile = null;

	private LinkedHashMap<String, IExchangeItem> exchangeItems = new LinkedHashMap<>();
	private LinkedHashMap<String, Variable> timeDependentVars = new LinkedHashMap<>();

	double[] timesInNetcdfFile = null;

	public void initialize(File workingDir, String[] arguments) {
		if (arguments.length != 1) {
			throw new RuntimeException("NetcdfD3dMapDataObject expects one argument: netcdfFileName (relative to working dir)");
		}
		File file = new File(workingDir, arguments[0]);
		try {
			netcdfFile = new NetcdfFile(file.getAbsolutePath());
		} catch (IOException e) {
			throw new RuntimeException("NetcdfD3dMapDataObject could not open netcdf file " + file.getAbsolutePath());
		}

		// int mmax = netcdfFile.findDimension("MMAX").getLength();
	}

	public void finish() {
		// close file
	}

	public String[] getExchangeItemIDs() {
		return exchangeItems.keySet().toArray(new String[exchangeItems.keySet().size()]);
	}

	public String[] getExchangeItemIDs(IExchangeItem.Role role) {
		if (role == IExchangeItem.Role.Output) {
			// all items
			return getExchangeItemIDs();
		} else {
			// todo: filter onvariables that can be written
			return getExchangeItemIDs();
		}
	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return exchangeItems.get(exchangeItemID);
	}

	private void readNetCdfVariables() {

		//in most netcdfFiles the time and spatial coordinate variables are shared between multiple data variables.
		//Therefore cache timeInfo objects so that time coordinate variables
		//are never read more than once.
		Map<Variable, IArrayTimeInfo> timeInfoCache = new HashMap<Variable, IArrayTimeInfo>();

		for (Variable variable : this.netcdfFile.getVariables()) {
			//skip coordinate variables.
			//a time variable is also a coordinate variable (i.e. has only itself as dimension).
			//do not create exchangeItems for coordinate variables.
			//the coordinate values will be stored in the metadata objects contained in the exchangeItems.
			if (variable.isCoordinateVariable()) {
				continue;
			}

			if (!variable.getDataType().isNumeric()) {
				continue;
			}

			// check if variables that depends on time.
			Variable timeVariable = NetcdfUtils.findTimeVariableForVariable(variable, this.netcdfFile);
			if (timeVariable != null) {

				timeDependentVars.put(variable.getName(), variable);

				// if this is the first time dependent variable: read the times
				if (timesInNetcdfFile == null) {
					ucar.ma2.Array timesArray;
					try {
						timesArray = timeVariable.read();
					} catch (IOException e) {
						throw new RuntimeException("NetcdfD3dMapDataObject could not read time variable " + timeVariable.getName() +
								"from netcdf file " + netcdfFile.getLocation());
					}
					timesInNetcdfFile = (double[]) timesArray.get1DJavaArray(double.class);
				}
			}

			// get the number of spatial dimensions
			int nonTimeDimensions = variable.getDimensions().size();
			if (timeVariable != null) {
				// one of the dimensions was for time
				nonTimeDimensions--;
			}

			int timeDimensionIndex = variable.findDimensionIndex(timeVariable.getName());
			if (timeDimensionIndex < 0) {
				throw new RuntimeException("NetcdfD3dMapDataObject: time dimension variable " + timeVariable.getName() +
						"not present netcdf file " + netcdfFile.getLocation());
			}

			IGeometryInfo geometryInfo = NetcdfUtils.createGeometryInfo(variable, netcdfFile);
			ITimeInfo timeInfo = NetcdfUtils.createTimeInfo(variable, this.netcdfFile, timeInfoCache);

			if (nonTimeDimensions == 3 && geometryInfo != null) {
				//if variable has data for a 2D grid time series.
				String parameterId = variable.getName();

				//create an exchangeItem that can read/write lazily.
				IQuantityInfo quantityInfo = new QuantityInfo(parameterId, variable.getUnitsString());

				int dimensionIndexToFlipForReadData = -1;
				IExchangeItem exchangeItem = new NetcdfD3dGridTimeSeriesExchangeItem(parameterId, IPrevExchangeItem.Role.InOut,
						timeInfo, quantityInfo, geometryInfo, this, timeDimensionIndex);
				this.exchangeItems.put(parameterId, exchangeItem);

				continue;
			}
		}
	}
}
