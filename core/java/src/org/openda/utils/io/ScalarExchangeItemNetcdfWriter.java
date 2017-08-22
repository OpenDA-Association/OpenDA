/* OpenDA v2.4.1 
* Copyright (c) 2017 OpenDA Association 
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

import org.openda.exchange.PointGeometryInfo;
import org.openda.exchange.dataobjects.NetcdfUtils;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.utils.Time;
import ucar.nc2.Dimension;
import ucar.nc2.NetcdfFileWriter;
import ucar.nc2.Variable;
import ucar.nc2.Attribute;

import java.io.File;
import java.io.IOException;
import java.util.*;

/**
 * Writes scalar data from one or more IExchangeItems to a NetCDF file. The data is written per time step.
 * This can only handle exchangeItems that contain scalar data for a single timeStep at a time, e.g. a model state scalar exchange item.
 * The written NetCDF files are compliant with the NetCDF CF conventions as much as possible, see http://cfconventions.org/
 *
 * @author Arno Kockx
 */
public class ScalarExchangeItemNetcdfWriter {
	private final IExchangeItem[] exchangeItems;
	private final NetcdfFileWriter netcdfFileWriter;
	private final List<String> stationIds;

	private final Variable timeVariable;
	private int currentTimeIndex = -1;
	private final List<Double> timesWrittenSoFar = new ArrayList<Double>();

	public ScalarExchangeItemNetcdfWriter(IExchangeItem[] exchangeItems, File outputFile, String stationIdVarName, String stationDimensionVarName) {
		if (exchangeItems == null) throw new IllegalArgumentException("exchangeItems == null");
		if (exchangeItems.length < 1) throw new IllegalArgumentException("exchangeItems.length < 1");
		if (outputFile == null) throw new IllegalArgumentException("outputFile == null");

		this.exchangeItems = exchangeItems;

		//validate that all exchange items are scalars.
		validateExchangeItems(exchangeItems);

		//create netcdf file.
		try {
			//set fill to true, otherwise missing values will not be written for variables that do not have data for all stations.
			netcdfFileWriter = NetcdfFileWriter.createNew(NetcdfFileWriter.Version.netcdf3, outputFile.getAbsolutePath());
			netcdfFileWriter.setFill(true);
		} catch (IOException e) {
			throw new RuntimeException(getClass().getSimpleName() + ": Error while opening netcdf file " + outputFile.getAbsolutePath() + " Message was: " + e.getMessage(), e);
		}

		//create time dimension and variable.
		Dimension timeDimension = NetcdfUtils.createTimeVariable(netcdfFileWriter, NetcdfUtils.TIME_VARIABLE_NAME, -1, NetcdfUtils.createTimeUnitString());
		timeVariable = netcdfFileWriter.findVariable(NetcdfUtils.TIME_VARIABLE_NAME);

		//create station dimension and variable.
		//this only adds a station id variable, this does not add spatial variables with coordinates,
		//because the coordinates are usually not available in exchangeItems that come from models.
		//gather locationIds.
		stationIds = NetcdfUtils.getStationIds(Arrays.asList(exchangeItems), null);
		Dimension stationDimension = NetcdfUtils.createStationsVariable(netcdfFileWriter, stationIdVarName, stationDimensionVarName, stationIds.size());

		//create data variables.
		NetcdfUtils.createDataVariables(netcdfFileWriter, Arrays.asList(exchangeItems), timeDimension, null, stationDimension, null);

		//add global metadata.
		NetcdfUtils.addGlobalAttributes(netcdfFileWriter);
		netcdfFileWriter.addGroupAttribute(null, new Attribute(NetcdfUtils.FEATURE_TYPE_ATTRIBUTE_NAME, NetcdfUtils.TIME_SERIES_FEATURE_TYPE));

		try {
			netcdfFileWriter.create();
		} catch (Exception e) {
			throw new RuntimeException(getClass().getSimpleName() + ": Error while creating netcdf file " + netcdfFileWriter.getNetcdfFile().getLocation() + " Message was: " + e.getMessage(), e);
		}

		//write station variable values.
		try {
			NetcdfUtils.writeStationIdVariableValues(netcdfFileWriter, stationIdVarName, stationIds);
		} catch (Exception e) {
			throw new RuntimeException(getClass().getSimpleName() + ": Error while writing station variable values to netcdf file " + netcdfFileWriter.getNetcdfFile().getLocation() + " Message was: " + e.getMessage(), e);
		}
	}

	private void validateExchangeItems(IExchangeItem[] exchangeItems) {
		for (IExchangeItem item : exchangeItems) {
			IGeometryInfo geometryInfo = item.getGeometryInfo();
			if (geometryInfo != null && !(geometryInfo instanceof PointGeometryInfo)) {
				throw new IllegalArgumentException(getClass().getSimpleName() + " can only write data for scalar exchange items. Exchange item '" + item.getId()
						+ "' of type " + item.getClass().getSimpleName() + " has a grid geometry info.");
			}
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
							+ " in netcdf file " + netcdfFileWriter.getNetcdfFile().getLocation());
				}
			}
		}

		//write current time.
		if (timesWrittenSoFar.contains(currentTime)) {
			throw new RuntimeException(getClass().getSimpleName() + ": Cannot write data. Output netcdf file " + netcdfFileWriter.getNetcdfFile().getLocation()
					+ " already contains data for current time " + new Date(Time.mjdToMillies(currentTime)));
		}
		try {
			NetcdfUtils.writeTimeVariableSingleValue(netcdfFileWriter, timeVariable, currentTimeIndex, currentTime);
		} catch (Exception e) {
			throw new RuntimeException(getClass().getSimpleName() + ": Error while writing time value for current time " + new Date(Time.mjdToMillies(currentTime))
					+ " to netcdf file " + netcdfFileWriter.getNetcdfFile().getLocation() + " Message was: " + e.getMessage(), e);
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
			if (values == null || values.length != 1) {
				throw new RuntimeException(getClass().getSimpleName() + ": Cannot write data from exchangeItem '" + item.getId() + "' of type " + item.getClass().getSimpleName()
						+ " because it has no value or stores multiple values for time index " + currentTimeIndex);
			}

			int stationIndex = stationIds.indexOf(NetcdfUtils.getStationId(item));
			if (stationIndex == -1) {
				throw new IllegalStateException("stationIndex == -1");
			}

			//determine dimensions and prepare origin for writing.
			//dimensions are (time, station).
			int[] dimensions = new int[2];
			dimensions[0] = 1;
			dimensions[1] = 1;
			int[] origin = new int[dimensions.length];
			origin[0] = currentTimeIndex;
			origin[1] = stationIndex;

			//write values for current time.
			Variable dataVariable = NetcdfUtils.getVariableForExchangeItem(netcdfFileWriter.getNetcdfFile(), item);
			NetcdfUtils.writeSelectedData(netcdfFileWriter, dataVariable, origin, dimensions, values);
		}
	}

	public void close() {
		try {
			netcdfFileWriter.close();
		} catch (IOException e) {
			throw new RuntimeException("Error while closing netcdf file " + netcdfFileWriter.getNetcdfFile().getLocation() + " Message was: " + e.getMessage(), e);
		}
	}
}
