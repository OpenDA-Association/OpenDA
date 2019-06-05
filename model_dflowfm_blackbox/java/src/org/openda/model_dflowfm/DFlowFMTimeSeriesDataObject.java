/*
 * Copyright (c) 2019 OpenDA Association
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

import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.QuantityInfo;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.exchange.timeseries.TimeSeriesFormatter;
import org.openda.exchange.timeseries.TimeSeriesSet;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem.Role;
import org.openda.utils.Results;

import java.io.*;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Set;

public final class DFlowFMTimeSeriesDataObject implements IDataObject {
	public static final String PROPERTY_PATHNAME = "pathName";
	private TimeSeriesSet timeSeriesSet = null;
	private LinkedHashMap<String, DoubleExchangeItem> amplitudes=null; 
	private LinkedHashMap<String, DoubleExchangeItem> phases=null; 
	private ArrayList<String> cmpFileNames =null;
	private LinkedHashMap<String, String> cmpNameFromId;
	private static final String idSeparator= ":";
	String fileName = null;
	File workingDir = null;

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
				Results.putMessage("DflowFMRestartFile: " + fileName + ", extra arguments ignored");
			}
		}
		this.timeSeriesSet = new TimeSeriesSet();
		this.amplitudes = new LinkedHashMap<String, DoubleExchangeItem>();
		this.phases = new LinkedHashMap<String, DoubleExchangeItem>();
		this.cmpFileNames = new ArrayList<String>();
		this.cmpNameFromId = new LinkedHashMap<String, String>();
		this.workingDir=workingDir;
		parseConfigurationFiles(workingDir, fileName);
	}

	/**
	 * Parse through the Dflowfm configuration files and add all defined timeseries 
	 *
	 * @param workingDir
	 *           Working directory
	 * @param fileName
	 *           The name of the file containing the data (relative to the working dir) This fileName may NOT contain wildcard
	 *           characters.
	 */
	public void parseConfigurationFiles(File workingDir, String fileName) {

		// get forcing file name and time properties from mdu file
		DFlowFMMduInputFile mduOptions = new DFlowFMMduInputFile(workingDir, fileName);
		String forcingFileName = mduOptions.get("external forcing","ExtForceFile");
		Double referenceDate = mduOptions.getReferenceDateInMjd();
		Double timeFactor = mduOptions.getTimeToMjdFactor();
//		System.out.println("reference date = " + referenceDate);
//		System.out.println("time unit conversion factor = " + timeFactor );
		
		// parse external forcing file
		DFlowFMExtInputFile extForcings = new DFlowFMExtInputFile(workingDir, forcingFileName);
		for (int i=0; i < extForcings.count() ; i++) {
			String quantity = extForcings.get("QUANTITY", i);
			String childFileName = extForcings.get("FILENAME", i);
			String baseFileName = childFileName.substring(0,childFileName.indexOf("."));
			String fileExtension = childFileName.substring(childFileName.indexOf("."));
//			System.out.println(baseFileName +  " ; " + fileExtension);
			/* PLI files*/
			if (fileExtension.equalsIgnoreCase(".pli")) {
				DFlowFMPliInputFile pliFile = new DFlowFMPliInputFile(workingDir, childFileName);
				// look for TIM or CMP files
				for (int fileNr=0 ; fileNr < pliFile.getLocationsCount(); fileNr++) {
					String timFilePath = baseFileName + String.format("_%04d", fileNr + 1) + ".tim";
					File timFile = new File(workingDir, timFilePath );
					String cmpFilePath = baseFileName + String.format("_%04d", fileNr + 1) + ".cmp";
					File cmpFile = new File(workingDir, cmpFilePath );
					String locationId = pliFile.getLocationId();
					
					// TIM file
					if ( timFile.isFile() ) {
//						System.out.println(timFile);
					    TimeSeriesFormatter timFormatter = new DFlowFMTimTimeSeriesFormatter(referenceDate, timeFactor);
						TimeSeries series = timFormatter.readFile(timFile.getAbsolutePath());
						
						series.setPosition( pliFile.getX(fileNr), pliFile.getY(fileNr));
						String location = String.format("%s.%d" , locationId ,fileNr+1);
						series.setLocation(location);
						series.setQuantity(quantity);
						String identifier = location + idSeparator + quantity ;
//						Results.putMessage("Creating exchange item with id: " + identifier );
						series.setId(identifier);
						series.setProperty(PROPERTY_PATHNAME, timFile.getAbsolutePath() );
						this.timeSeriesSet.add(series);
					}else if ( cmpFile.isFile() ) {
					// CMP file
						this.cmpFileNames.add(cmpFilePath);
						DoubleExchangeItem amplitude;
						DoubleExchangeItem phase;
						DFlowFMCmpInputFile cmpfile = new DFlowFMCmpInputFile(workingDir,cmpFilePath);
						String[] AC = cmpfile.getACname();
						for (String var: AC) {
							if (! var.contentEquals("period")){
								double lon = pliFile.getX(fileNr);
								double lat = pliFile.getY(fileNr);
								//amplitude
								String location = String.format("%s.%d" , locationId ,fileNr+1);
								String amplitudeId = location + idSeparator + quantity + "."+var+"_amplitude" ;
								double ampl = cmpfile.getAmplitude(var);
								amplitude = new DoubleExchangeItem(amplitudeId, Role.InOut, ampl);
								amplitude.setQuantityInfo(new QuantityInfo(quantity + ".amplitude","m"));
								amplitude.setLatitude(lat);
								amplitude.setLongitude(lon);
								amplitude.setLocation(location);
								this.amplitudes.put(amplitudeId, amplitude);
								this.cmpNameFromId.put(amplitudeId,cmpFilePath);
                                //phase
								String phaseId = location + idSeparator + quantity + "."+var+"_phase" ;
								double phi = cmpfile.getPhase(var);
								phase = new DoubleExchangeItem(phaseId, Role.InOut, phi);
								phase.setQuantityInfo(new QuantityInfo(quantity + ".phase","degrees"));
								phase.setLatitude(lat);
								phase.setLongitude(lon);
								phase.setLocation(location);
								this.cmpNameFromId.put(phaseId,cmpFilePath);
								this.phases.put(phaseId, phase);								
							} else {
								throw new RuntimeException("DFLOW-FM tidal components of fourier type with user specified frequency not implemented in OpenDA yet.");
							}
						}
					}
					
				}
			}
			/* XYZ files*/
//			else if (fileExtension.equalsIgnoreCase(".xyz")) {
				
//			}
		}

		
	}
	
 	public String [] getExchangeItemIDs() {
		String [] result = new String[this.timeSeriesSet.size()+2*this.amplitudes.size()];
		Set<String> quantities = this.timeSeriesSet.getQuantities();
		int idx=0;
		for (String quantity: quantities) {
//			System.out.println(quantity);
			Set<String> locations = this.timeSeriesSet.getOnQuantity(quantity).getLocations();
			for (String location: locations) {
				String id = location + idSeparator + quantity;
//				System.out.println("getExhangeItemIDs: " + id);
				result[idx]= id;
				idx++;	
			}
		}
		for( String id : this.amplitudes.keySet()){
			result[idx]=id;
			idx++;
		}
		for( String id : this.phases.keySet()){
			result[idx]=id;
			idx++;
		}
		return result;
	}

// 	public String [] getExchangeItemIDs() {
//		String [] result = new String[this.ExchangeItems.size()];
//		Set<String> keys = this.ExchangeItems.keySet();
//		int idx=0;
//		for (String key: keys) {
//			result[idx]=key;
//			idx++;
//		}
//		return result;
//	}

	public String [] getExchangeItemIDs(IExchangeItem.Role role) {
		return getExchangeItemIDs();
	}

//	public IExchangeItem getDataObjectExchangeItem(String ExchangeItemID) {
//		Set<String> keys = this.ExchangeItems.keySet();
//		for (String key: keys) {
//			if (ExchangeItemID.equals(key)){
//				return this.ExchangeItems.get(key);
//			}
//		}
//		return null;
//	}
//	
	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		
		String[] parts = exchangeItemID.split(idSeparator);
		if (parts.length != 2) {
			throw new RuntimeException("Invalid exchangeItemID " + exchangeItemID );
		}
		String location = parts[0];	
		String quantity = parts[1];
//		System.out.println(location + ", " + quantity );
		
		if(exchangeItemID.endsWith("_amplitude")){
			if(this.amplitudes.containsKey(exchangeItemID)){
				DoubleExchangeItem amplitude = this.amplitudes.get(exchangeItemID);
				return amplitude;
			}else{
				throw new RuntimeException("No tidal amplitude found for " + exchangeItemID);
			}
		}else if(exchangeItemID.endsWith("_phase")){
			if(this.phases.containsKey(exchangeItemID)){
				DoubleExchangeItem phase = this.phases.get(exchangeItemID);
				return phase;
			}else{
				throw new RuntimeException("No tidal phase found for " + exchangeItemID);
			}			
		}else{
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

	}
	
	
	/**
	 * Write all time series in this DataObject that were read from file (with property DflowfmTimTimeSeriesIoObject.PROPERTY_PATHNAME
	 * set). Ignores all other time series, including those obtained from an URL.
	 */
	public void finish() {
		if (this.timeSeriesSet == null) return;
		for (TimeSeries series : this.timeSeriesSet)
			if (series.hasProperty(PROPERTY_PATHNAME)) writeTimTimeSeries(series);
		this.writeComponents();
	}

	/**
	 * Write the specified time series to the path name it was read from (using a property).
	 *
	 * @param series
	 *           The time series to write (path name will be determined from its property).
	 */
	public void writeTimTimeSeries(TimeSeries series) {
		if (!series.hasProperty(PROPERTY_PATHNAME))
			throw new RuntimeException("Cannot write a time series without " + PROPERTY_PATHNAME + " property");
		File timFile = new File(series.getProperty(PROPERTY_PATHNAME));
		writeTimTimeSeries(series, timFile);
	}

	/**
	 * Write the specified time series to the specified file
	 *
	 * @param series
	 *           The time series to write
	 * @param timFile
	 *           The file to write to
	 */
	public static void writeTimTimeSeries(TimeSeries series, File timFile) {
		if (!timFile.exists()) {
			try {
				timFile.createNewFile();
			}
			catch (IOException e) {
				throw new RuntimeException("Cannot create TIM file " + e.getMessage());
			}
		}

		FileOutputStream timFileOutputStream;
		try {
			timFileOutputStream = new FileOutputStream(timFile);
		}
		catch (FileNotFoundException e) {
			throw new RuntimeException("Cannot find output TIM file " + e.getMessage());
		}

		DFlowFMTimTimeSeriesFormatter timFormatter = new DFlowFMTimTimeSeriesFormatter();
		timFormatter.write(timFileOutputStream, series);
		try {
			timFileOutputStream.close();
		} catch (IOException e) {
			throw new RuntimeException("Cannot close output TIM file " + e.getMessage());
		}
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
	
	/*
	 * Internal routine to write all the updates to amplitudes and phases to file.
	 * Called from finish.
	 */
	private void writeComponents(){
		for(String cmpFilePath : this.cmpFileNames){
			DFlowFMCmpInputFile cmpfile = new DFlowFMCmpInputFile(workingDir,cmpFilePath);
			// Check for updates to exchangeItem
			for(String id : this.cmpNameFromId.keySet()){
				if(cmpFilePath.equalsIgnoreCase(this.cmpNameFromId.get(id))){
					String[] dotParts = id.split("\\."); //remove upto and including the last dot
					String afterLastDot = dotParts[dotParts.length-1];
					String[] parts=afterLastDot.split("_");
					String var = parts[0];
					if(parts[1].equalsIgnoreCase("amplitude")){
						double ampl=this.amplitudes.get(id).getValue();
						cmpfile.setAmplitude(var,ampl);
					}else{
						double phase=this.phases.get(id).getValue();
						cmpfile.setPhase(var,phase);
					}
				}
			}
			cmpfile.WriteInputFile();
		}
	}
}
