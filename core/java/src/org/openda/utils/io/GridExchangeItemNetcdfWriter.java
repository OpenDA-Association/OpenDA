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

package org.openda.utils.io;

import org.openda.exchange.ArrayGeometryInfo;
import org.openda.exchange.IrregularGridGeometryInfo;
import org.openda.exchange.PointGeometryInfo;
import org.openda.exchange.dataobjects.GridVariableProperties;
import org.openda.exchange.dataobjects.NetcdfUtils;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IVector;
import org.openda.utils.Time;
import ucar.nc2.Dimension;
import ucar.nc2.NetcdfFileWriteable;
import ucar.nc2.Variable;

import java.io.File;
import java.io.IOException;
import java.util.*;

/**
 * Writes grid data from one or more IExchangeItems to a NetCDF file. The data is written per time step.
 * This can only handle exchangeItems that contain grid data for a single timeStep at a time, e.g. a model state grid exchange item.
 * The written NetCDF files are compliant with the NetCDF CF conventions as much as possible, see http://cfconventions.org/
 *
 * @author Arno Kockx
 */
public class GridExchangeItemNetcdfWriter {
	private final IExchangeItem[] exchangeItems;
	private final NetcdfFileWriteable netcdfFile;

	private final Variable timeVariable;
	private int currentTimeIndex = -1;
	private final List<Double> timesWrittenSoFar = new ArrayList<Double>();

	public GridExchangeItemNetcdfWriter(IExchangeItem[] exchangeItems, File outputFile) {
		if (exchangeItems == null) throw new IllegalArgumentException("exchangeItems == null");
		if (exchangeItems.length < 1) throw new IllegalArgumentException("exchangeItems.length < 1");
		if (outputFile == null) throw new IllegalArgumentException("outputFile == null");

		this.exchangeItems = exchangeItems;

		//create netcdf file.
		try {
			netcdfFile = NetcdfFileWriteable.createNew(outputFile.getAbsolutePath(), false);
		} catch (IOException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": Error while opening netcdf file " + outputFile.getAbsolutePath() + " Message was: " + e.getMessage(), e);
		}
		//always create large file, in case much data needs to be written.
		netcdfFile.setLargeFile(true);

		//create time dimension and variable.
		Dimension timeDimension = NetcdfUtils.createTimeVariable(netcdfFile, NetcdfUtils.TIME_VARIABLE_NAME, -1, NetcdfUtils.createTimeUnitString());
		timeVariable = netcdfFile.findVariable(NetcdfUtils.TIME_VARIABLE_NAME);

		//create grid dimensions and variables.
		//gather geometryInfos.
		List<IGeometryInfo> geometryInfos = new ArrayList<IGeometryInfo>();
		for (IExchangeItem item : exchangeItems) {
			IGeometryInfo geometryInfo = item.getGeometryInfo();
			if (geometryInfo == null || geometryInfo instanceof PointGeometryInfo) {
				throw new IllegalArgumentException(getClass().getSimpleName() + " can only write data for grid exchange items. Exchange item '" + item.getId()
						+ "' of type " + item.getClass().getSimpleName() + " has no grid geometry info.");
			}
			geometryInfos.add(geometryInfo);
		}
		//create spatial coordinate variables, if not present yet.
		//this only adds spatial dimensions, this does not add spatial variables with coordinates,
		//because the coordinates are usually not available in exchangeItems that come from models.
		Map<IGeometryInfo, GridVariableProperties> geometryInfoGridVariablePropertiesMap = NetcdfUtils.createGridVariables(netcdfFile, geometryInfos.toArray(new IGeometryInfo[geometryInfos.size()]));

		//create data variables.
		NetcdfUtils.createDataVariables(netcdfFile, Arrays.asList(exchangeItems), timeDimension, null, null, geometryInfoGridVariablePropertiesMap);

		//add global metadata.
		NetcdfUtils.addGlobalAttributes(netcdfFile);

		try {
			netcdfFile.create();
		} catch (Exception e) {
			throw new RuntimeException(getClass().getSimpleName() + ": Error while creating netcdf file " + netcdfFile.getLocation() + " Message was: " + e.getMessage(), e);
		}

		//write grid variables values.
		try {
			NetcdfUtils.writeGridVariablesValues(netcdfFile, geometryInfoGridVariablePropertiesMap);
		} catch (Exception e) {
			throw new RuntimeException(getClass().getSimpleName() + ": Error while writing grid variable values to netcdf file " + netcdfFile.getLocation() + " Message was: " + e.getMessage(), e);
		}
	}

	/**
	 * Write the current data to the output file.
	 */
	public void writeDataForCurrentTimeStep() {
		currentTimeIndex++;
		writeTime();
		writeData();
	}

	/**
	 * Write time value for current time.
	 */
	private void writeTime() {
		//get current time.
		double currentTime = Double.NaN;
		for (IExchangeItem item : exchangeItems) {
			if (item.getTimeInfo() == null || item.getTimeInfo().getTimes() == null || item.getTimeInfo().getTimes().length != 1 || Double.isNaN(item.getTimeInfo().getTimes()[0])) {
				throw new RuntimeException(getClass().getSimpleName() + ": Cannot write data from exchangeItem '" + item.getId() + "' of type " + item.getClass().getSimpleName()
						+ " because it contains no time info or stores values for more than one time.");
			}

			double time = item.getTimeInfo().getTimes()[0];
			if (Double.isNaN(currentTime)) {
				currentTime = time;
			} else {
				if (time != currentTime) {
					throw new RuntimeException(getClass().getSimpleName() + ": Cannot write data. Exchange items have different times for time index " + currentTimeIndex
							+ " in netcdf file " + netcdfFile.getLocation());
				}
			}
		}

		//write current time.
		if (timesWrittenSoFar.contains(currentTime)) {
			throw new RuntimeException(getClass().getSimpleName() + ": Cannot write data. Output netcdf file " + netcdfFile.getLocation()
					+ " already contains data for current time " + new Date(Time.mjdToMillies(currentTime)));
		}
		try {
			NetcdfUtils.writeTimeVariableSingleValue(netcdfFile, timeVariable, currentTimeIndex, currentTime);
		} catch (Exception e) {
			throw new RuntimeException(getClass().getSimpleName() + ": Error while writing time value for current time " + new Date(Time.mjdToMillies(currentTime))
					+ " to netcdf file " + netcdfFile.getLocation() + " Message was: " + e.getMessage(), e);
		}
		timesWrittenSoFar.add(currentTime);
	}

	/**
	 * Write data values for current time.
	 */
	private void writeData() {
		for (IExchangeItem item : exchangeItems) {
			//get active values for current time.
			double[] values = item.getValuesAsDoubles();

			//add missing values for non-active grid cells.
			IGeometryInfo geometryInfo = item.getGeometryInfo();
			if (geometryInfo == null) {
				throw new RuntimeException(getClass().getSimpleName() + ": Cannot write data from exchangeItem '" + item.getId() + "' of type " + item.getClass().getSimpleName()
						+ " because it contains no geometry info.");
			}
			values = NetcdfUtils.addMissingValuesForNonActiveGridCells(geometryInfo, values);

			//determine dimensions and prepare origin for writing.
			int[] dimensions;
			int[] origin;
			if (geometryInfo instanceof IrregularGridGeometryInfo) {
				int gridCellCount = ((IrregularGridGeometryInfo) geometryInfo).getCellCount();
				//dimensions are (time, node).
				dimensions = new int[2];
				dimensions[0] = 1;
				dimensions[1] = gridCellCount;
				origin = new int[dimensions.length];
				origin[1] = 0;

			} else if (geometryInfo instanceof ArrayGeometryInfo) {
				int rowCount = ((ArrayGeometryInfo) geometryInfo).getLatitudeArray().length();
				int columnCount = ((ArrayGeometryInfo) geometryInfo).getLongitudeArray().length();
				//dimensions are (time, y, x).
				dimensions = new int[3];
				dimensions[0] = 1;
				dimensions[1] = rowCount;
				dimensions[2] = columnCount;
				origin = new int[dimensions.length];
				origin[1] = 0;
				origin[2] = 0;

			} else {
				throw new UnsupportedOperationException(getClass().getSimpleName() + ": Cannot write data from exchangeItem '" + item.getId() + "' of type " + item.getClass().getSimpleName()
						+ " because it has an unknown geometryInfo type: " + geometryInfo.getClass().getSimpleName());
			}

			//write values for current time.
			origin[0] = currentTimeIndex;
			Variable dataVariable = NetcdfUtils.getVariableForExchangeItem(netcdfFile, item);
			NetcdfUtils.writeSelectedData(netcdfFile, dataVariable, origin, dimensions, values);
		}
	}

	public void close() {
		try {
			netcdfFile.close();
		} catch (IOException e) {
			throw new RuntimeException("Error while closing netcdf file " + netcdfFile.getLocation() + " Message was: " + e.getMessage(), e);
		}
	}
}
