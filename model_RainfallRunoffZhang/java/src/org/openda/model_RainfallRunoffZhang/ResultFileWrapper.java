/*
* Copyright (c) 2023 OpenDA Association 
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

import org.openda.exchange.AbstractDataObject;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.IExchangeItem;

import java.io.*;
import java.util.Vector;

/**
 * Allows reading of time variables from matlab readable ASCII file and
 * transferring these to OpenDA ExchangeItems. 
 * <p>
 * This code can be copied and
 * adapted to create wrappers for other blackbox models coded in matlab. Code to
 * be modified is indicated by comments starting with "// --". The input file
 * that is read by this wrapper has following format: 
 * <p>
 * 	 key value<br>
 *   key value<br>
 *   ...
 * <p>
 * where key is a time [mjd] and value is the corresponding numerical. 
 * Actually this class reads a time series from ASCII-file. 
 * Comment lines and empty lines are skipped.  
 * 
 * @author Beatrice Marti, hydrosolutions ltd.
 *
 */
public class ResultFileWrapper extends AbstractDataObject {

	// Class specific values
	File workingDir;
	String fileName = null;

	/**
	 * Initialize the DataObject. Reads the content of a file in
	 * directory (workingDir) with given arguments.
	 * 
	 * @param workingDir
	 *            Working directory
	 * @param arguments
	 *            The name of the file containing the data (relative to the
	 *            working directory), and additional arguments (may be null zero-length)
	 */
	@Override
	public void initialize(File workingDir, String[] arguments) {
		
		this.workingDir = workingDir;
		this.fileName = arguments[0];
		
		try {
			ReadNameListFile();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	@Override
	public void finish() {
		/*
		 * In this example, writing the result file is not needed because the totalRunoff is not 
		 * read by the rainfall runoff model to initialize a run. 
		 * 
		 */
	}

	/**
	 * Opens file fileName in directory workingDir, reads in values and stores
	 * them to an ExchangeItem.
	 * <p>
	 * Actually reads one single value from file.
	 *
	 * @throws IOException
	 */
	private void ReadNameListFile() throws IOException {

		File namelist = new File(workingDir, fileName);
		if (!namelist.exists()) {
			System.out.println("ResultFileWrapper.ReadNameListFile(): Failed to open File " + fileName + " in " + workingDir);
		}
		Vector<Double> timesVector = new Vector<>();
		Vector<Double> valuesVector = new Vector<>();
		double[] times;
		double[] values;
		
		if (!namelist.exists()) {
			throw new RuntimeException("ResultFileWrapper.ReadNameListFile(): settings file "
					+ namelist.getAbsolutePath() + " does not exist");
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
			// System.out.println("line : " + line);
			// Test for end of file.
			if (line == null) {
				eof = true;
			} else if (line.startsWith("%")) {
				// If the lines starts with comment or meta data, do nothing.
				continue;
			} else if (line.startsWith("#")) {
				continue;
			} else if (line.equals("")) {
				// If the line is empty, do nothing.
				continue;
			} else {
				// If the end of the file is not reached yet split line and store
				// data.
				String[] columns = line.split("\\s");
				
				if (columns.length != 2) {
					buff.close();
					in.close();
					throw new RuntimeException("Reading TimeSeries : Wrong number of arguments : '" + line + "'");
				}
				
				// Now parse the line.
				columns[0] = columns[0].trim();
				columns[1] = columns[1].trim();

				timesVector.add(Double.valueOf(columns[0])); // Values are added to dynamic Vector.
				valuesVector.add(Double.valueOf(columns[1]));
			}
		}
		// Close the writers.
		buff.close();
		in.close();

		// Parse times and values from vector to array of doubles.
		times = new double[timesVector.size()];
	    values = new double[valuesVector.size()];
	    if (times.length != values.length) { throw new RuntimeException("Reading TimeSeries: Value array length doesn't match time array length."); }
	    for (int i = 0; i < times.length; i++) {
	    	times[i] = timesVector.get(i);
	    	values[i] = valuesVector.get(i); 
	    }
	    TimeSeries series = new TimeSeries(times, values);
	    String id = "totalRunoff";
	    series.setId(id);
	    this.exchangeItems.put(id,series);
	    
	}

}
