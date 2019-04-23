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

package org.openda.exchange.dataobjects;

import org.openda.interfaces.IPrevExchangeItem;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem.Role;

import org.openda.exchange.timeseries.TimeSeries;
import org.openda.exchange.timeseries.TimeSeriesFormatter;

import org.openda.utils.ConfigTree;

/**
 * DataObject that uses a configured TimeSeriesFormatter to parse a text file.
 *
 * @author Werner Kramer
 */
public class TimeSeriesFormatterDataObject implements IDataObject {

	private static final Logger logger = LoggerFactory.getLogger(TimeSeriesFormatterDataObject.class);
	private HashMap<String, TimeSeries> seriesMap = new HashMap<>();
	private ConfigTree config;
	private File workingDir;

	/**
	 * Initialize the configurable. Specify what its "working directory" is (usually meaning: the directory
	 * where its configuration file is), and provide its arguments.
	 *
	 * @param workingDir The directory indicating the where the configurable is started (not as 'current
	 *                   working directory', but as the root path for its configuration files etc).
	 * @param arguments  The arguments needed to initialize. Typically the first argument can be a configuration
	 */
	@Override
	public void initialize(File workingDir, String[] arguments) {
		this.config = new ConfigTree(workingDir, arguments[0], true);
		this.workingDir = workingDir;
		// get formatter class from config and instantiate
		String className = this.config.getContentString("formatter@class");
		TimeSeriesFormatter formatter = TimeSeriesFormatter.instantiateFrom(className, this.config.getSubTrees("formatter")[0]);
		ConfigTree seriesTrees[] = this.config.getSubTrees("timeSeries");
		if (seriesTrees.length == 0) {
			logger.warn("No '<timeSeries>' configured in %s", arguments[0]);
		}
		// Loop over all configured timeSeries
		for (ConfigTree seriesTree : seriesTrees) {

			// Get id from config
			String id = seriesTree.getAsString("@id", null);
			if (!"use".equals(seriesTree.getAsString("@status", "use"))) {
				logger.debug("Ignore timeSeries with id '{}'", id);
				continue;
			}

			String seriesFileName = seriesTree.getContentString("").trim();
			File seriesFile = new File(workingDir, seriesFileName);
			logger.debug("Parsing file'{}'", seriesFile.getName());
			if (seriesFile.exists()) {
				TimeSeries series = formatter.readFile(seriesFile.getAbsolutePath());
				// Get time series status from file or config  (if present)
				String keyword = "status";
				String stringValue = series.getStringProperty(keyword, "use");
				stringValue = seriesTree.getAsString("@status", stringValue);
				series.setProperty(keyword, stringValue);
				if (!"use".equals(stringValue)) {
					logger.debug("Skipping file '{}'", seriesFile.getName());
					continue;
				}

				// Get time series standardDeviation from file or config (if present)
				keyword = "standardDeviation";
				double doubleValue = series.getDoubleProperty(keyword, 0.0);
				doubleValue = seriesTree.getAsDouble("@" + keyword, doubleValue);
				series.setProperty(keyword, "" + doubleValue);

				seriesMap.put(id, series);
			}
		}
	}

	/**
	 * Get the identifiers of the exchange items that can be retrieved from and set to the model.
	 * Should return String[0] if there are no items.
	 *
	 * @return The array of exchange item identifiers.
	 */
	@Override
	public String[] getExchangeItemIDs() {
		return seriesMap.keySet().toArray( new String[0]);
	}

	/**
	 * Get the identifiers of the exchange items that can be retrieved from and set to the model,
	 * according to the specified role (input, output, both)
	 * Should return String[0] if there are no matching items.
	 *
	 * @param role Input, Output, or InOut (i.e. both)
	 * @return The array of exchange item identifiers.
	 */
	@Override
	public String[] getExchangeItemIDs(Role role) {
		if (Role.Input.equals(role)) {
			return getExchangeItemIDs();
		}
		return new String[0];
	}

	/**
	 * Get the exchange item specified by <c>exchangeItemID</c>.
	 * Returns null if no exchangeItem with the given exchangeItemID is found.
	 *
	 * @param exchangeItemID The exchange item identifier.
	 * @return The required exchange item.
	 */
	@Override
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return seriesMap.get(exchangeItemID);
	}

	/**
	 * Shut down this dataObject. Typically this implies flushing output, closing files and network connections.
	 * It is also possible that work is done directly after modification of the exchangeItems, so then no work
	 * may be left.
	 */
	@Override
	public void finish() {
		String className = this.config.getContentString("formatter@class");
		TimeSeriesFormatter formatter = TimeSeriesFormatter.instantiateFrom(className, this.config.getSubTrees("formatter")[0]);
		ConfigTree seriesTrees[] = this.config.getSubTrees("timeSeries");
		// Loop over all configured timeSeries
		for (ConfigTree seriesTree : seriesTrees) {
			// Get id from config
			String id = seriesTree.getAsString("@id", null);
			if (!"use".equals(seriesTree.getAsString("@status", "use"))) {
				logger.debug("Ignore timeSeries with id '{}'", id);
				continue;
			}
			String seriesFileName = seriesTree.getContentString("").trim();
			File seriesFile = new File(workingDir, seriesFileName);
			logger.debug("Parsing file'{}'", seriesFile.getName());
			if (seriesFile.exists()) {
				TimeSeries timeSeries = seriesMap.get(id);
				if ( timeSeries.getRole() == IPrevExchangeItem.Role.Input ) {
					continue;
				}
				formatter.writeFile(seriesFile, timeSeries, true);
			}
		}
	}

}
