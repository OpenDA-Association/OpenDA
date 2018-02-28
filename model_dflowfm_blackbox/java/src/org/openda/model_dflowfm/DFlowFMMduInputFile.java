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
package org.openda.model_dflowfm;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.ini4j.Ini;
import org.ini4j.InvalidFileFormatException;
import org.openda.exchange.timeseries.TimeUtils;

public class DFlowFMMduInputFile {
	
	/**
	 * Class for reading settings from the mdu input file
	 * *
	 * The file is loosely based on the INI format:
	 *
	 *   [model]
	 *   key = value
	 *   
	 *   [geometry]
	 *   
	 *   [numerics]
	 *   [physics]
	 *   [wind]
	 *   [time]
	 *   [external forcing]
	 *   [output]
	 *
	 */
	private File inputFile = null;
	private Ini ini = new Ini();
	private final static String COMMENT_CHAR = "#";
	private static Map<String, Double> timeMap;

	static {
	    timeMap = new HashMap<String, Double>();
	    timeMap.put("S", 1.0 / (60.0*60.0*24.0) );
	    timeMap.put("M", 1.0 / (60.0*24.0) );
	    timeMap.put("H", 1.0 / 24.0 );
	}

	public DFlowFMMduInputFile(File workingDir, String fileName) {
		
		inputFile = new File(workingDir,fileName);
		parseInputFile();
		// Set<String> keyNames = ini.keySet();
		// logger.debug("Found the following sections: " + keyNames);
	}
	
	private void parseInputFile() {

		/*  Parse .mdu input file as an ini file without sections
		 *  Remove all keys originating from comments lines 
		 */
		//logger.debug("Parsing: " + inputFile);
		try {
			ini.load(inputFile);
		} catch (InvalidFileFormatException e) {
			throw new RuntimeException("Invalid Formatting in '" + inputFile.getAbsolutePath() + "'.", e);
		} catch (IOException e) {
			throw new RuntimeException("No such file'" + inputFile.getAbsolutePath() + "'.", e);
		}
	}
	
	public String get(String sectionName, String key) {
		
		String value = ini.get(sectionName, key);
		if (value == null) {
			String error = String.format("The option '[%s] %s' not specified in'" + inputFile.getAbsolutePath() + "'.", sectionName, key);
			
			throw new RuntimeException(error);
		}
		// remove inline comment from value
		String[] parts = value.split(COMMENT_CHAR);
		value = parts[0].trim();
		//logger.debug("Get [" + sectionName + "] " + key + " = " + value);
		return value;
	}
	
	public Double getReferenceDateInMjd() {
		// get reference date for future use
		String dateString = this.get("time","RefDate");
		Double dateInMjd;
		try {
			dateInMjd = TimeUtils.date2Mjd(dateString + "0000" );
		}
		catch (Exception e)  {
			throw new RuntimeException("Error parsing reference date" + this.get("time","RefDate") );
		}
		return dateInMjd;
	}
	
	public Double getTimeToMjdFactor() {
		String timeUnit = this.get("time","Tunit");
		Double factor = timeMap.get(timeUnit);
		if (factor == null) {
			throw new RuntimeException("Incorrect Tunit in MDU-file: " + timeUnit);
		}
		return factor;
	}
	
	public void put(String sectionName, String key, String value) {
		//logger.debug("Put [" + sectionName + "] " + key + " = " + value);
		ini.put(sectionName, key, value);
	}

	public void store() {
		try {
			ini.store(inputFile);
		} catch (IOException e) {
			throw new RuntimeException("Cannot write to " + ini.getFile(), e);
		}
	}
	
}

