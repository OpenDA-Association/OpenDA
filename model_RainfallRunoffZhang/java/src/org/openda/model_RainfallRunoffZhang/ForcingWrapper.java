package org.openda.model_RainfallRunoffZhang;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Set;

import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;

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

public class ForcingWrapper implements IoObjectInterface {

	// Class specific values
	File workingDir;
	String configString;
	String fileName = null;
	HashMap<String, TimeSeries> items = new LinkedHashMap<String, TimeSeries>();

	/** 
	 * Cache values to be read. The dynamic form of an ArrayList is chosen here 
	 * because the size of the time series to be read is not a priori known. 
	 * Cache Lists for times and values are needed.
	 * -- Modify variables for forcing to be read here.
	 */
	private List<Double> precipitationCache = new ArrayList<Double>();
	private List<Double> precipTimeCache = new ArrayList<Double>();
	private List<Double> potETCache = new ArrayList<Double>();
	private List<Double> potETTimeCache = new ArrayList<Double>();
	
	
	// --- Methods --
	
	/**
	 * Initialize the IoObject. Reads the content of a file (fileName) in
	 * directory (workingDir) with given arguments.
	 * 
	 * @param workingDir
	 *            Working directory
	 * @param fileName
	 *            The name of the file containing the data (relative to the
	 *            working directory.)
	 * @param arguments
	 *            Additional arguments (may be null zero-length)
	 */
	public void initialize(File workingDir, String fileName, String[] arguments) {
			
		this.workingDir = workingDir;
		this.fileName = fileName;
		
		try {
			ReadNameListFile(workingDir, fileName, arguments);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Ask which elements can be accessed
	 * 
	 * @return The list of element identifiers that can be accessed
	 */
	public IPrevExchangeItem[] getExchangeItems() {
		// Get the number of items.
		int n = this.items.size();
		Set<String> keys = this.items.keySet();
		IExchangeItem[] result = new IExchangeItem[n];
		int i = 0;
		for (String key : keys) {
			result[i] = this.items.get(key);
			i++;
		}
		return result;
	}

	/**
	 * Updates the forcing files. Here, the entire time series is written.
	 */
	public void finish() {
		
		double[] pt;
		double[] p;
		if (this.fileName.contains("precip") || this.fileName.contains("Precip")) {
			pt = this.items.get("precipitation").getTimes();
			p = this.items.get("precipitation").getValuesAsDoubles();
			//write to file
			System.out.println("ForcingWrapper.finish(): Writing to file: "+this.workingDir+"/"+this.fileName);
			File outputFile = new File(this.workingDir,this.fileName);
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
		} else if (this.fileName.contains("pot") || this.fileName.contains("Pot") || this.fileName.contains("ET")) {
			pt = this.items.get("potET").getTimes();
			p = this.items.get("potET").getValuesAsDoubles();
			//write to file
			System.out.println("ForcingWrapper.finish(): Writing to file: "+this.workingDir+"/"+this.fileName);
			File outputFile = new File(this.workingDir,this.fileName);
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
	 * @param workingDir
	 * @param fileName
	 * @param arguments
	 * @throws IOException
	 */
	private void ReadNameListFile(File workingDir, String fileName, String[] arguments) throws IOException {
		
		File namelist = new File(workingDir, fileName);
		if (!namelist.exists()) {
			throw new RuntimeException("ForcingWrapper.ReadNameListFile(): settings file " + namelist.getAbsolutePath() + " does not exist");
		}
		// Create nested reader.
		FileInputStream in = new FileInputStream(namelist);
		BufferedReader buff = new BufferedReader(new InputStreamReader(in));
		String line = ""; // Initialize line.
		Boolean eof = false; // End of file cache.
			
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
			this.items.put(id,precipitationSeries);
			@SuppressWarnings("unused")
			IExchangeItem precipitationExchangeItem = new TimeSeries(precipitationSeries);
			//System.out.println("<- stored precipSeries ->");
			
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
			this.items.put(id,potETSeries);
			@SuppressWarnings("unused")
			IExchangeItem potETExchangeItem = new TimeSeries(potETSeries);
			//System.out.println("<- stored potETSeries ->");
		}

	}
	
}
