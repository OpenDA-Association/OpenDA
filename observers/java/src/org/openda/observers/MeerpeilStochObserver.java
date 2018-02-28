/* OpenDA v2.4.3 
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
package org.openda.observers;

import org.openda.exchange.timeseries.TimeSeries;
import org.openda.utils.ConfigTree;
import java.io.File;

/**
 * Implementation of IStochObserver to be used in combination with algorithm Meerpeilcorrectie.
 *
 * The input file for this class has the following format (FEWS export TimeSeries)
 * <?xml version="1.0" encoding="UTF-8"?>
 * <TimeSeries xmlns="http://www.wldelft.nl/fews/PI" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.wldelft.nl/fews/PI http://fews.wldelft.nl/schemas/version1.0/pi-schemas/pi_timeseries.xsd" version="1.2">
 *   <timeZone>0.0</timeZone>
 *   <series>
 *     <header>
 *       <type>instantaneous</type>
 *       <locationId>WAQUA_IJsselmeer</locationId>
 *       <parameterId>H.meerpeil.gemiddeld</parameterId>
 *       <timeStep unit="nonequidistant"/>
 *       <startDate date="2012-10-18" time="06:00:00"/>
 *       <endDate date="2012-10-18" time="06:00:00"/>
 *       <missVal>-999.0</missVal>
 *       <stationName>WAQUA IJsselmeer</stationName>
 *       <units>m</units>
 *     </header>
 *     <event date="2012-10-18" time="06:00:00" value="-0.14201607" flag="0"/>
 *   </series>
 * </TimeSeries>
 *
 * @author Alja Vrieling
 *
 */
public class MeerpeilStochObserver extends TimeSeriesStochObserver {

    /**
	 * Construct MeerpeilTimeSeriesStochObserver from input file.
	 * @param workingDir Working dir. for stoch observer.
	 * @param arguments Arguments for initialization
	 */
	public void initialize(File workingDir, String[] arguments){
		String fileName = arguments[0];
		file = new File(workingDir,fileName);
		TimeSeries[] Meerpeil = null;
		TimeSeries Tempseries = null;
		if (!file.exists()) {
			throw new RuntimeException("Could not find file "+file.getAbsolutePath());
		}
		ConfigTree config = new ConfigTree(workingDir,fileName);
		ConfigTree seriesTree[] = config.getSubTrees("series");
		ConfigTree eventTree[] = seriesTree[0].getSubTrees("event");
		ConfigTree headerTree[] = seriesTree[0].getSubTrees("header");
		if (eventTree.length != 1 ) {
			throw new RuntimeException("Incorrect file format in "+file.getAbsolutePath());
		} else {
			Meerpeil = new TimeSeries[seriesTree.length];
			String stringValue = eventTree[0].toString();
			// parse stringValue:
			// <?xml version="1.0"?><event date="2012-10-18" time="06:00:00" value="-0.14201607" flag="0" />
			String[] parts = stringValue.split("\n");
			// parts[0] = <?xml version="1.0"?>
			// parts[1] = <event date="2012-10-18" time="06:00:00" value="-0.14201607" flag="0" />
			for (String part: parts) {
				if (part.startsWith("<event")){
					parts = part.split("\" ");
					for (String key: parts) {
						if (key.startsWith("value")) {
							String[] keyvalue = key.split("=\"");
							double times[] = new double[1];
							times[0] = 1.0;
							double factor[] = new double[1];
							factor[0] = Double.parseDouble(keyvalue[1]);
							Tempseries = new TimeSeries(times, factor);
						}
					}
				}
			}
			// set property Location
			String key = "locationId";
			ConfigTree tmpTree[] = headerTree[0].getSubTrees(key);
	        // parse and strip <key>..</key>
			stringValue = tmpTree[0].toString();
			parts = stringValue.split("\n");
			for (String part: parts) {
				if (part.startsWith("<"+key)){
					stringValue = part.replaceAll("<"+key+">","");
					stringValue = stringValue.replaceAll("</"+key+">","");
				}
			}
			Tempseries.setLocation(stringValue);
    		// set property Quantity
			key = "parameterId";
			tmpTree = headerTree[0].getSubTrees(key);
			// parse and strip <key>..</key>
			stringValue = tmpTree[0].toString();
			parts = stringValue.split("\n");
			for (String part: parts) {
				if (part.startsWith("<"+key)){
					stringValue = part.replaceAll("<"+key+">","");
					stringValue = stringValue.replaceAll("</"+key+">","");
				}
			}
			Tempseries.setQuantity(stringValue);
			Meerpeil[0] = Tempseries;
			setSeries(Meerpeil);
		}
	}
}
