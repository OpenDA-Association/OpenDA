package org.openda.model_RainfallRunoffZhang;

import java.io.BufferedReader;
//import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
//import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Set;
import java.util.Vector;

import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.IExchangeItem;

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
public class ResultFileWrapper implements IoObjectInterface {

	// Class specific values
	File workingDir;
	String configString;
	String fileName = null;
	HashMap<String, TimeSeries> items = new LinkedHashMap<String, TimeSeries>();
	
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
	 * 
	 * @return The list of element identifiers that can be accessed
	 */
	public TimeSeries[] getExchangeItems() {
		// Get the number of items.
		int n = this.items.size();
		Set<String> keys = this.items.keySet();
		TimeSeries[] result = new TimeSeries[n];
		int i = 0;
		for (String key : keys) {
			result[i] = this.items.get(key);
			i++;
		}
		return result;
	}
	
	/** 
	 * A slightly more elegant variant of getExchangeItems().
	 * 
	 * @return A vector of type TimeSeries containing the values of the 
	 *         time series stored in the hash map items.
	 */
	public Vector<TimeSeries> getData() {
		return new Vector<TimeSeries>(this.items.values());
	}
	
	
	public void finish() {
		/*
		 * In this example, writing the result file is not needed because the totalRunoff is not 
		 * read by the rainfall runoff model to initialize a run. 
		 * 
		 * 
		// Write updated predictor
		double[] currentTimeCache = this.items.get("totalRunoff").getTimes();
		double[] totalRunoffCache = this.items.get("totalRunoff").getValuesAsDoubles();
		
		//write to file
		System.out.println("Result wrapper finish(): Writing to file: "+this.workingDir+"/"+this.fileName);
		File outputFile = new File(this.workingDir,this.fileName);
		try{
			if(outputFile.isFile()){
				outputFile.delete();
			}
		}catch (Exception e) {
			System.out.println("ResultWrapper: trouble removing file "+ fileName);
		}
		try {
			FileWriter writer = new FileWriter(outputFile);
			BufferedWriter out = new BufferedWriter(writer);

			int seriesLength = currentTimeCache.length;
			if (seriesLength != totalRunoffCache.length) {
				System.out.println("Error writing result file. Length of times and values are not the same.");
			}
			for (int i = 0 ; i < seriesLength; i++) {
				out.write(currentTimeCache[i] + " " + totalRunoffCache[i] + System.getProperty("line.separator"));
			}
			out.flush(); // Force java to write buffered stream to disk.
			out.close();
			writer.close();

		} catch (Exception e) {
			throw new RuntimeException("Result file wrapper: Problem writing to file "+fileName+" : " + System.getProperty("line.separator") + e.getMessage());
		}
		*/
	}

	/**
	 * Opens file fileName in directory workingDir, reads in values and stores
	 * them to an ExchangeItem.
	 * <p>
	 * Actually reads one single value from file.
	 * 
	 * @param workingDir
	 * @param fileName
	 * @param arguments
	 * @throws IOException
	 */
	private void ReadNameListFile(File workingDir, String fileName,
			String[] arguments) throws IOException {
		
		File namelist = new File(workingDir, fileName);
		if (!namelist.exists()) {
			System.out.println("ResultFileWrapper.ReadNameListFile(): Failed to open File " + fileName + " in " + workingDir);
		}
		//TimeSeries series = new TimeSeries();
		Vector<Double> timesVector = new Vector<Double>();
		Vector<Double> valuesVector = new Vector<Double>();
		//ArrayList<Double> times = new ArrayList<Double>();
		//ArrayList<Double> values = new ArrayList<Double>();
		double[] times = null;
		double[] values = null;
		
		if (!namelist.exists()) {
			throw new RuntimeException("ResultFileWrapper.ReadNameListFile(): settings file "
					+ namelist.getAbsolutePath() + " does not exist");
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
	    this.items.put(id,series);
	    @SuppressWarnings("unused")
		IExchangeItem totalRunoffExchangeItem = new TimeSeries(series);
	    
	    
	}

}
