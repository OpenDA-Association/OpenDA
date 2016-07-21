/* MOD_V2.0
 * Copyright (c) 2013 OpenDA Association
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

package org.openda.model_openfoam;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.openda.exchange.timeseries.TimeSeriesFormatter;
import org.openda.exchange.timeseries.TimeSeriesSet;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem.Role;

import java.io.File;
import java.util.Iterator;

import java.util.Set;
import java.util.regex.Pattern;

public final class CsvTimeSeriesDataObject implements IDataObject {

    private static final Logger logger = LoggerFactory.getLogger(CsvTimeSeriesDataObject.class);


    public static final String PROPERTY_PATHNAME = "pathName";
	private TimeSeriesSet timeSeriesSet = null;
	String fileName = null;
	File workingDir = null;
	String idSeparator = ".";

	/**
	 * Initialize the IDataObject
	 *
	 * @param workingDir
	 *           Working directory
	 * @param arguments
	 *           Additional arguments (may be null zero-length)
	 */
	public void initialize(File workingDir, String[] arguments) {
		if (arguments != null) {
			this.fileName = arguments[0];
			if (arguments.length > 1) {
				throw new RuntimeException("DflowFMRestartFile: " + this.fileName + " has too many arguments");
			}
		}
		this.timeSeriesSet = new TimeSeriesSet();
		this.workingDir=workingDir;

		TimeSeriesFormatter csvFormatter = new CsvTimeSeriesFormatter(";");
		File file = new File(this.workingDir, this.fileName);
		logger.debug("Reading file: " + file.getAbsolutePath());
		TimeSeries series = csvFormatter.readFile(file);
		series.setProperty(PROPERTY_PATHNAME, file.getAbsolutePath());
		timeSeriesSet.add(series);
	}


 	public String [] getExchangeItemIDs() {
		String [] result = new String[this.timeSeriesSet.size()];
		Set<String> quantities = this.timeSeriesSet.getQuantities();
		int idx=0;
		for (String quantity: quantities) {
			Set<String> locations = this.timeSeriesSet.getOnQuantity(quantity).getLocations();
			for (String location: locations) {
				String id = location + idSeparator + quantity;
				result[idx]= id;
				idx++;
			}
		}
		return result;
	}


	public String [] getExchangeItemIDs(Role role) {
		return getExchangeItemIDs();
	}


	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		
		String[] parts = Pattern.compile(idSeparator, Pattern.LITERAL).split(exchangeItemID);
		if (parts.length != 2) {
			throw new RuntimeException("Invalid exchangeItemID " + exchangeItemID );
		}
		String location = parts[0];	
		String quantity = parts[1];


		// Get the single time series based on location and quantity
		TimeSeriesSet myTimeSeriesSet = this.timeSeriesSet.getOnQuantity(quantity)
					.getOnLocation(location);
		Iterator<TimeSeries> iterator = myTimeSeriesSet.iterator();
			if (!iterator.hasNext()) {
			    throw new RuntimeException("No time series found for " + exchangeItemID);
			}
			TimeSeries timeSeries = iterator.next();
			if (iterator.hasNext()) {
			    throw new RuntimeException("Time series is not uniquely defined for  " + exchangeItemID);
			}
			return timeSeries;


	}
	
	
	/**
	 * Write all time series in this DataObject that were read from file (with property DflowfmTimTimeSeriesIoObject.PROPERTY_PATHNAME
	 * set). Ignores all other time series, including those obtained from an URL.
	 */
	public void finish() {
		if (this.timeSeriesSet == null) return;
		for (TimeSeries series : this.timeSeriesSet)
			writeTimeSeries(series);
	}

	/**
	 * Write the specified time series to the specified file
	 *
	 * @param series
	 *           The time series to write
	 */
	public static void writeTimeSeries(TimeSeries series) {
		CsvTimeSeriesFormatter csvFormatter = new CsvTimeSeriesFormatter(";");
		File file = new File(series.getProperty(PROPERTY_PATHNAME));
		csvFormatter.writeFile(file, series, false);
	}

	/**
	 * @return Reference to the time series set
	 */
	public TimeSeriesSet getTimeSeriesSet() {
		return this.timeSeriesSet;
	}

	/**
	 * @param set
	 *           The TimeSeriesSet to set in this IoObject
	 */
	public void setTimeSeriesSet(TimeSeriesSet set) {
		this.timeSeriesSet = set;
	}

}
