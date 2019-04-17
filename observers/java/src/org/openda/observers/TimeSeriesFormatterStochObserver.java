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
package org.openda.observers;

import org.openda.exchange.timeseries.TimeSeries;
import org.openda.exchange.timeseries.TimeSeriesFormatter;
import org.openda.interfaces.ISelector;
import org.openda.interfaces.IStochObserver;
import org.openda.utils.ConfigTree;
import org.openda.utils.IndicesSelector;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class TimeSeriesFormatterStochObserver extends TimeSeriesStochObserver {

	private static final Logger logger = LoggerFactory.getLogger(TimeSeriesFormatterStochObserver.class);

	/**
	 * Initialize TimeSeriesFormatterStochObserver from input file.
	 * @param workingDir Working dir. for stoch observer.
	 * @param arguments Arguments for initialization
	 */
	public void initialize(File workingDir, String[] arguments){
		ConfigTree config = new ConfigTree(workingDir, arguments[0], true);
		// get formatter class from config and instantiate
		String className = config.getContentString("formatter@class");
		TimeSeriesFormatter formatter = TimeSeriesFormatter.instantiateFrom(className, config.getSubTrees("formatter")[0]);
		ConfigTree seriesTrees[] = config.getSubTrees("timeSeries");
		if (seriesTrees.length == 0) {
			logger.warn("No '<timeSeries>' configured in %s", arguments[0]);
		}
		// Loop over all configured timeSeries
		TimeSeries seriesArray[] = new TimeSeries[seriesTrees.length];
		for(int i=0;i<seriesTrees.length;i++){
			String seriesFileName = seriesTrees[i].getContentString("").trim();
			File seriesFile = new File(workingDir, seriesFileName);
			logger.debug("Parsing %s", seriesFile.getName());
			if (seriesFile.exists()) {
				TimeSeries series = formatter.readFile(seriesFile.getAbsolutePath());
				series.setId(seriesTrees[i].getContentString("@id"));

				// Get time series status from file or config  (if present)
				String keyword = "status";
				String stringValue = series.getStringProperty(keyword,"use");
				stringValue = seriesTrees[i].getAsString("@status",stringValue);
				series.setProperty(keyword, stringValue);

				// Get time series standardDeviation from file or config (if present)
				keyword = "standardDeviation";
				double doubleValue = series.getDoubleProperty(keyword,0.0);
				doubleValue = seriesTrees[i].getAsDouble("@"+keyword, doubleValue);
				series.setProperty(keyword,""+doubleValue);
				seriesArray[i] = series;
			}
		}
		setSeries(seriesArray);
	}

	public IStochObserver createSelection(Type observationType) {
		List<TimeSeries> selectedSeries = new ArrayList<TimeSeries>();
		for (TimeSeries serie : series) {
			String status = serie.getProperty("status");
			if (status != null) {
				if (observationType.toString().toLowerCase().contentEquals(status.toLowerCase())) {
					selectedSeries.add(serie);
				}
			}
		}

		TimeSeries arrSelectedSeries[] = selectedSeries.toArray(new TimeSeries[0]);
		return new TimeSeriesStochObserver(arrSelectedSeries);
	}

	public ISelector createSelector(Type observationType) {
		IndicesSelector indSelector = new IndicesSelector();
		int indLast=0;
		int indFirst;
		for (TimeSeries serie : series) {
			String status = serie.getProperty("status");
			indFirst = indLast;
			indLast += serie.getSize();
			if (status != null) {
				if (observationType.toString().toLowerCase().contentEquals(status.toLowerCase())) {
					indSelector.addIndexRange(indFirst, indLast);
				}
			}
		}
		return indSelector;
	}

}
