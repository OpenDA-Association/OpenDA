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
package org.openda.exchange.dataobjects;

import java.io.*;
import java.util.Arrays;
import java.util.LinkedHashMap;

import org.openda.exchange.timeseries.NoosTimeSeriesFormatter;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.IComposableDataObject;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem.Role;
import org.openda.utils.Glob;


public class NoosDataObject implements IDataObject, IComposableDataObject{
	private static final String PROPERTY_PATHNAME = "pathName";
	private LinkedHashMap<String, TimeSeries> timeSeriesSet = new LinkedHashMap<String, TimeSeries>();
	private File workingDir = new File(".");


	/**
	 * Initialize the IDataObject
	 *
	 * @param workingDir
	 *           Working directory
	 * @param arguments
	 *           First argument should be a filename his fileName may be null, empty or
	 *           contain wildcard characters.
	 *           Additional arguments (may be null zero-length) These arguments are ignored.
	 */

	public void initialize(File workingDir, String[] arguments) {
		String fileName=arguments[0];
		String newArguments[] = new String[arguments.length-1];
		for(int i=0;i<arguments.length-1;i++){
			newArguments[i]=arguments[i+1];
		}
		this.initialize(workingDir, fileName, newArguments);
	}

	/**
	 * Initialize the IDataObject
	 *
	 * @param workingDir
	 *           Working directory
	 * @param fileName
	 *           The name of the file containing the data (relative to the working dir) This fileName may be null, empty or
	 *           contain wildcard characters.
	 * @param arguments
	 *           Additional arguments (may be null zero-length) These arguments are ignored.
	 */
	public void initialize(File workingDir, String fileName, String[] arguments) {
		this.timeSeriesSet = new LinkedHashMap<String, TimeSeries>();
		if(workingDir==null){
			workingDir=new File(".");
		}
		this.workingDir=workingDir;

		// Check whether the fileName is null or contains * or ?
		// If so, use the multi-file version
		// It it doesn't, call readNoosTimeSeries directly
		if (fileName == null || fileName.isEmpty() || fileName.matches(".*[*?].*")) {
			// Determine which filenames to read
			String[] fileNames;
			if (fileName == null || fileName.isEmpty()) {
				fileNames = new String[]{};
				//fileNames = workingDir.list();
			}
			else {
				final String regexp = Glob.createRegex(fileName);
				fileNames = workingDir.list(new FilenameFilter() {
					public boolean accept(File dir, String name) {
						return name.matches(regexp);
					}
				});
			}

			// Sort names to make the order independent of the OS
			Arrays.sort(fileNames);
			// Then, read each file
			for (String name : fileNames) {
				readNoosTimeSeries(workingDir, name);
			}
		}
		else if(fileName.length()>0){
			// Read the file indicated by fileName directly
			readNoosTimeSeries(workingDir, fileName);
		}
	}

	/**
	 * Initialize the IDataObject without arguments
	 *
	 * @param workingDir
	 *           Working directory
	 * @param fileName
	 *           The name of the file containing the data (relative to the working dir) This fileName may be null, empty or
	 *           contain wildcard characters.
	 */
	public void initialize(File workingDir, String fileName) {
		final String args[] = {};
		this.initialize(workingDir, fileName, args);
	}

	/**
	 * Read a single Noos timeseries from a file.
	 *
	 * @param workingDir
	 *           Working directory
	 * @param fileName
	 *           The name of the file containing the data (relative to the working dir) This fileName may NOT contain wildcard
	 *           characters.
	 */
	public void readNoosTimeSeries(File workingDir, String fileName) {
		File noosFile = new File(workingDir, fileName);
		FileInputStream noosFileInputStream;
		try {
			noosFileInputStream = new FileInputStream(noosFile);
		}
		catch (FileNotFoundException e) {
			throw new RuntimeException("Cannot read noos file: " + fileName + " " + e.getMessage());
		}
		NoosTimeSeriesFormatter noosFormatter = new NoosTimeSeriesFormatter();
		TimeSeries series = noosFormatter.read(noosFileInputStream);
		series.setProperty(PROPERTY_PATHNAME, noosFile.getAbsolutePath());
		this.timeSeriesSet.put(series.getId(),series);
	}


	
	public String[] getExchangeItemIDs() {
		return this.timeSeriesSet.keySet().toArray(new String[this.timeSeriesSet.size()]);
	}

	
	public String[] getExchangeItemIDs(Role role) {
		return this.timeSeriesSet.keySet().toArray(new String[this.timeSeriesSet.size()]);
	}

	
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		return this.timeSeriesSet.get(exchangeItemID);
	}

	/**
	 * Write all time series in this IDataObject that were read from file (with property NoosTimeSeriesIDataObject.PROPERTY_PATHNAME
	 * set). Ignores all other time series, including those obtained from an URL.
	 */
	public void finish() {
		if (this.timeSeriesSet == null) return;
		for (TimeSeries series : this.timeSeriesSet.values())
			if (series.hasProperty(PROPERTY_PATHNAME)) writeNoosTimeSeries(series);
	}

	/**
	 * Write the specified time series to the path name it was read from (using a property).
	 *
	 * @param series
	 *           The time series to write (path name will be determined from its property).
	 */
	public void writeNoosTimeSeries(TimeSeries series) {
		if (!series.hasProperty(PROPERTY_PATHNAME))
			throw new RuntimeException("Cannot write a time series without " + PROPERTY_PATHNAME + " property");
		File noosFile = new File(series.getProperty(PROPERTY_PATHNAME));
		writeNoosTimeSeries(series, noosFile);
	}

	/**
	 * Write the specified time series to the specified file
	 *
	 * @param series
	 *           The time series to write
	 * @param noosFile
	 *           The file to write to
	 */
	public static void writeNoosTimeSeries(TimeSeries series, File noosFile) {
		if (!noosFile.exists()) {
			try {
				noosFile.createNewFile();
			}
			catch (IOException e) {
				throw new RuntimeException("Cannot create noos file " + e.getMessage());
			}
		}

		FileOutputStream noosFileOutputStream;
		try {
			noosFileOutputStream = new FileOutputStream(noosFile);
		}
		catch (FileNotFoundException e) {
			throw new RuntimeException("Cannot find output noos file " + e.getMessage());
		}

		NoosTimeSeriesFormatter noosFormatter = new NoosTimeSeriesFormatter();
		noosFormatter.write(noosFileOutputStream, series);
		try {
			noosFileOutputStream.close();
		} catch (IOException e) {
			throw new RuntimeException("Cannot close output noos file " + e.getMessage());
		}
	}

	
	public void addExchangeItem(IExchangeItem item) {
		// TODO Auto-generated method stub
		String id=item.getId();
		if(!(item instanceof TimeSeries)){
			throw new RuntimeException("NoosDataObject: attempt to add something other than a timeseries.");
		}
		TimeSeries s=(TimeSeries)item;
		if(!s.hasProperty(PROPERTY_PATHNAME)){
			// make up a default filename
			String loc=s.getLocation();
			loc=loc.replaceAll(",", "_");
			loc=loc.replaceAll(" ", "-");
			String fileName=s.getQuantityId()+"_"+loc+".noos";
			fileName.replaceAll(" ", "-");
			File file = new File(this.workingDir,fileName);
			s.setProperty(PROPERTY_PATHNAME, file.getAbsolutePath());
		}
		this.timeSeriesSet.put(id, s);
	}
	
	public String toString(){
		String result="noosDataObject{\n";
		for(TimeSeries series: this.timeSeriesSet.values()){
			result+=series.toString();
		}
		result+="}\n";
		return result;
	}
}
