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
package org.openda.model_RainfallRunoffZhang;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import org.openda.exchange.AbstractDataObject;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.IExchangeItem;

/**
 * ForcingWrapper class reads the entire time series of the example model in one go. 
 * In this simple example 2 forcing files are read: one for potential evaporation and 
 * one for precipitation. The names of these files is specified in 
 * model/bin/configuration.m.
 * <p>
 * The format of the 2 forcing files is:<br>
 *   # some comment, info about content. e.g. variable name and unit for each column: time [mjd] precipitation [mm]<br>
 *   key value<br>
 *   key value<br>
 *   ...
 * <p>  
 * Where key is a time step with time format MJD and value is a numerical 
 * value associated with that time. 
 * <p>
 * Note that only one row is allowed for comments, matlab reads in the time series 
 * from the second row.
 * <p>
 * Adapt your own wrapper: Comments on where changes have to be made and how start with --
 * Not all methods have to be adapted. Essentially, the definition of the class specific 
 * variables have to be adapted, as the routines ReadNameListFile() and finish().
 * 
 * @author Beatrice Marti, hydrosolutions ltd.
 *
 */

public class ForcingWrapper extends AbstractDataObject {

	// Class specific values
	File workingDir;
	String fileName = null;

	/** 
	 * Cache values to be read. The dynamic form of an ArrayList is chosen here 
	 * because the size of the time series to be read is not a priori known. 
	 * Cache Lists for times and values are needed.
	 * -- Modify variables for forcing to be read here.
	 */
	private List<Double> precipitationCache = new ArrayList<>();
	private List<Double> precipTimeCache = new ArrayList<>();
	private List<Double> potETCache = new ArrayList<>();
	private List<Double> potETTimeCache = new ArrayList<>();


	// --- Methods --
	
	/**
	 * Initialize the DataObject. Reads the content of a file (fileName) in
	 * directory (workingDir) with given arguments.
	 * 
	 * @param workingDir
	 *            Working directory
	 * @param arguments
	 * 			  Contains the name of the file containing the data (relative to the
	 * 	          working directory), and additional arguments (may be null zero-length)
	 */
	@Override
	public void initialize(File workingDir, String[] arguments) {
			
		this.workingDir = workingDir;
		fileName = arguments[0];
		
		try {
			ReadNameListFile(workingDir, fileName);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Updates the forcing files. Here, the entire time series is written.
	 */
	@Override
	public void finish() {
		
		double[] pt;
		double[] p;
		if (fileName.contains("precip") || fileName.contains("Precip")) {
			pt = exchangeItems.get("precipitation").getTimes();
			p = exchangeItems.get("precipitation").getValuesAsDoubles();
			//write to file
			System.out.println("ForcingWrapper.finish(): Writing to file: "+this.workingDir+"/"+fileName);
			File outputFile = new File(this.workingDir,fileName);
			try{
				if(outputFile.isFile()){
					outputFile.delete();
				}
			}catch (Exception e) {
				System.out.println("ForcingWrapper.finish(): trouble removing file "+ fileName);
			}
			try {
				FileWriter writer = new FileWriter(outputFile);
				BufferedWriter out = new BufferedWriter(writer);

				
				// Write forcing with noise from noise model propagated to the next time step.
				
				out.write("# time [mjd] value [mm] " + System.lineSeparator());
				for (int i = 0; i < pt.length; i++ ) {
					out.write(pt[i] + " " + p[i] + System.lineSeparator());
				}
				
				out.flush(); // Force java to write buffered stream to disk.
				out.close();
				writer.close();
			} catch (Exception e) {
				throw new RuntimeException("ForcingWrapper.finish(): Problem writing to file "+fileName+" : "+System.lineSeparator()+e.getMessage());
			}
		} else if (fileName.contains("pot") || fileName.contains("Pot") || fileName.contains("ET")) {
			pt = exchangeItems.get("potET").getTimes();
			p = exchangeItems.get("potET").getValuesAsDoubles();
			//write to file
			System.out.println("ForcingWrapper.finish(): Writing to file: "+this.workingDir+"/"+fileName);
			File outputFile = new File(this.workingDir,fileName);
			try{
				if(outputFile.isFile()){
					outputFile.delete();
				}
			}catch (Exception e) {
				System.out.println("ForcingWrapper.finish(): trouble removing file "+ fileName);
			}
			try {
				FileWriter writer = new FileWriter(outputFile);
				BufferedWriter out = new BufferedWriter(writer);

				// Write forcing with noise from noise model propagated to the next time step.
				
				out.write("# time [mjd] value [mm] " + System.lineSeparator());
				for (int i = 0; i < pt.length; i++ ) {
					out.write(pt[i] + " " + p[i] + System.lineSeparator());
				}
				
				out.flush(); // Force java to write buffered stream to disk.
				out.close();
				writer.close();
			} catch (Exception e) {
				throw new RuntimeException("ForcingWrapper.finish(): Problem writing to file "+fileName+" : "+System.lineSeparator() + e.getMessage());
			}
		}
						
		
	}

	/**
	 * Opens file fileName in directory workingDir, reads in values and stores
	 * them to ExchangeItems. The exchange items are time series to allow the 
	 * use of a noise model with the exchange items.
	 * This forcing wrapper is a bit more complicated than the other wrappers 
	 * because it is used to read two different files: one for precipitation data
	 * and one for potential evapotranspiration data. 
	 * -- Adapt reading of the key-value pairs to the format of the i/o file 
	 *     and the number and ids of the ExchangeItem(s).
	 * 
	 * @param workingDir WorkingDir
	 * @param fileName  The name of the file containing the data (relative to the working dir.)
	 * @throws IOException Exception that might be thrown
	 */
	private void ReadNameListFile(File workingDir, String fileName) throws IOException {
		
		File namelist = new File(workingDir, fileName);
		if (!namelist.exists()) {
			throw new RuntimeException("ForcingWrapper.ReadNameListFile(): settings file " + namelist.getAbsolutePath() + " does not exist");
		}
		// Create nested reader.
		FileInputStream in = new FileInputStream(namelist);
		BufferedReader buff = new BufferedReader(new InputStreamReader(in));
		String line; // Initialize line.
		boolean eof = false; // End of file cache.
			
		// While End of file is not reached yet do the following:
		while (!eof) {
			// Read line.
			line = buff.readLine();
			// Test for end of file.
			if (line == null) {
				eof = true;
			}
			// If the end of the file is not reached yet split line and store
			// data.
			else {
				// Now parse the line.
				if (line.startsWith("#")) {
					continue;
				} else if (line.contains(System.getProperty("line.separator"))) {
					// Skip empty lines.
					continue;
				} else if (line.equals("")) {
					continue;
				} else {
					String[] columns = line.split(" ");
					columns[0] = columns[0].trim(); // Removes white spaces in
													// the beginning or the end
													// of the string.
					columns[1] = columns[1].trim();

					// Parse the values. The file names need to contain one of the following strings.
					if (fileName.contains("precip") || fileName.contains("Precip")) {
						precipitationCache.add(Double.valueOf(columns[1]));
						precipTimeCache.add(Double.valueOf(columns[0]));
					} else if (fileName.contains("pot") || fileName.contains("Pot") || fileName.contains("ET")) {
						potETCache.add(Double.valueOf(columns[1]));
						potETTimeCache.add(Double.valueOf(columns[0]));
					}
						
				}
			}
		}
		// Close the writers.
		buff.close();
		in.close();

		// Store the content of the lists in arrays of doubles that can 
		// be parsed to TimeSeries and store the stuff in exchange items.
		if (fileName.contains("precip") || fileName.contains("Precip")) {
			int timeLength = precipTimeCache.size();
			if (timeLength != precipitationCache.size()){
				System.out.println("ForcingWrapper.ReadNameListFile(): Error reading precipitation data. Length of read times and values are not the same.");
			}
			double[] pt = new double[timeLength];
			double[] p = new double[timeLength];
			for (int i = 0; i < timeLength; i++) {
				pt[i] = precipTimeCache.get(i);
				p[i] = precipitationCache.get(i);
			}
			TimeSeries precipitationSeries = new TimeSeries(pt,p);
			String id = "precipitation";
			precipitationSeries.setId(id);
			exchangeItems.put(id, precipitationSeries);
			
		} else if (fileName.contains("pot") || fileName.contains("Pot") || fileName.contains("ET")) {
			int timeLength = potETTimeCache.size();
			if (timeLength != potETCache.size()){
				System.out.println("ForcingWrapper.ReadNameListFile(): Error reading potET data. Length of read times and values are not the same.");
			}
			double[] pt = new double[timeLength];
			double[] p = new double[timeLength];
			for (int i = 0; i < timeLength; i++) {
				pt[i] = potETTimeCache.get(i);
				p[i] = potETCache.get(i);
			}
			TimeSeries potETSeries = new TimeSeries(pt,p);
			String id = "potET";
			potETSeries.setId(id);
			exchangeItems.put(id, potETSeries);
			@SuppressWarnings("unused")
			IExchangeItem potETExchangeItem = new TimeSeries(potETSeries);
			//System.out.println("<- stored potETSeries ->");
		}

	}
	
}
