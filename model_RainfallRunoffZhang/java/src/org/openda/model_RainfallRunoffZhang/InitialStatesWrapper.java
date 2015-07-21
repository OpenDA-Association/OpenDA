package org.openda.model_RainfallRunoffZhang;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Set;

import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;

/**
 * Allows reading of initial states from matlab readable ASCII file and
 * transferring these to OpenDA ExchangeItems. 
 * <p>
 * This code can be copied and
 * adapted to create wrappers for other blackbox models coded in matlab. Code to
 * be modified is indicated by comments starting with "// --". The input file
 * that is read by this wrapper has following format: 
 * <p>
 *   % currentTime = 26000, finalTime = 26001<br>
 * 	 key = value; % Some optional comment.<br>
 *   key = value; % Some more optional comment.<br>
 *   ... 
 * <p>
 * White spaces are optional as is the comment. Comment lines and empty lines 
 * are skipped. keys that are not specified in this wrapper to be read are skipped. 
 * <p>
 * A workaround is necessary to be able to use the TimeSeriesNoiseModel for 
 * perturbing the initial states. The time series noise model propagates noise 
 * from one time step to the next. Therefore, at least 2 time steps are required 
 * in the exchange item where the noise model has to be applied. In this example
 * 2 time steps are read and the value for the initial state is duplicated for 
 * these two time steps. The noise model then propagates the noise from the first 
 * time step to the second time step. finish() finally writes the value at the 
 * second time (perturbed with noise) to the rainfall runoff model input file. 
 * 
 * @author Beatrice Marti, hydrosolutions ltd.
 *
 */
public class InitialStatesWrapper implements
		IoObjectInterface {

	// Class specific values
	File workingDir;
	String configString;
	String fileName = null;
	HashMap<String, TimeSeries> items = new LinkedHashMap<String, TimeSeries>();

	// Cache values to be read.
	// -- Add variables for initial States to be read here.
	private double[] soilMoistureCache = new double[2];
	private double[] gwStorageCache = new double[2];
	private double[] timeCache = new double[2];

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

	public void finish() {
		// Updates initial states file.
				double[] currentTimeCache = this.items.get("soilMoisture").getTimes();
				double[] soilMoistureCache = this.items.get("soilMoisture").getValuesAsDoubles();
				double[] gwStorageCache = this.items.get("gwStorage").getValuesAsDoubles();
				
				//write to file
				System.out.println("InitialStatesWrapper.finish(): Writing to file: "+this.workingDir+"/"+this.fileName);
				File outputFile = new File(this.workingDir,this.fileName);
				try{
					if(outputFile.isFile()){
						outputFile.delete();
					}
				}catch (Exception e) {
					System.out.println("InitialStatesWrapper.finish(): trouble removing file "+ fileName);
				}
				try {
					FileWriter writer = new FileWriter(outputFile);
					BufferedWriter out = new BufferedWriter(writer);

					/**
					 * Write initial states values with noise from noise model propagated to the next time step.
					 */
					out.write("% currentTime = " + currentTimeCache[0] + ", finalTime = " + currentTimeCache[1] + System.getProperty("line.separator"));
					out.write("soilMoisture = " + soilMoistureCache[1] + "; " + System.getProperty("line.separator"));
					out.write("gwStorage = " + gwStorageCache[1] + "; " + System.getProperty("line.separator"));
					
					out.flush(); // Force java to write buffered stream to disk.
					out.close();
					writer.close();

				} catch (Exception e) {
					throw new RuntimeException("InitialStatesWrapper.finish(): Problem writing to file "+fileName+" : " + System.getProperty("line.separator") + e.getMessage());
				}
	}

	/**
	 * Opens file fileName in directory workingDir, reads in values and stores
	 * them to an ExchangeItem.
	 * -- Adapt reading of the key-value pairs and writing to the ExchangeItem 
	 *     here below.
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
			throw new RuntimeException("InitialStatesWrapper.ReadNameListFile(): settings file "
					+ namelist.getAbsolutePath() + " does not exist");
		}
		// Create nested reader.
		FileInputStream in = new FileInputStream(namelist);
		BufferedReader buff = new BufferedReader(new InputStreamReader(in));
		String line = ""; // Initialize line.
		Boolean eof = false; // End of file cache.
		
		//System.out.println("Initial States wrapper: Start reading file.");
		// While End of file is not reached yet do the following:
		while (!eof) {
			// Read line.
			line = buff.readLine();
			//System.out.println("line : " + line);
			// Test for end of file.
			if (line == null) {
				eof = true;
			}
			// If the end of the file is not reached yet split line and store
			// data.
			else {
				// Now parse the line.
				// Remove comments at end of line.
				if (line.indexOf("%") > 1) {
					String columns[] = line.split("%");
					line = columns[0];
					//System.out.println("Line with comment at end of key-value pair : " + line);
				}
				if (line.startsWith("%")) {
					// If the lines starts with comment and it is possible to read 
					// currentTime, do that. The first entry of columns will contain %, 
					// the second currentTime, the third =, and the fourth the value 
					// for currentTime. This is followed by some more comments. 
					//System.out.println(">> Line starts with %.");
					String[] columns = line.split(" +");
					if (columns.length > 6) { 
						// Read input of the format: % currentTime = 333, finalTime = 444
						columns[1] = columns[1].trim();
						if (columns[1].equals("currentTime")) {
							columns[3] = columns[3].trim();
							// Remove comma from columns[3].
							String[] tempTime = columns[3].split(",");
							timeCache[0] = Double.valueOf(tempTime[0]);
						} else {
							System.out.println("InitialStatesWrapper.ReadNameListFile(): trouble reading current time.");
						}
						
						if (columns[4].equals("finalTime")) {
							columns[6] = columns[6].trim();
							String[] temp = columns[6].split(";");
							timeCache[1] = Double.valueOf(temp[0]);
						} else {
							System.out.println("InitialStatesWrapper.ReadNameListFile(): trouble reading final time.");
						}
					}
				} else if (line.startsWith("#")) {
					continue;
				} else if (line.contains(System.getProperty("line.separator"))) {
					// Skip empty lines.
					continue;
				} else if (line.equals("")) {
					continue;
				} else if (line.indexOf("=") > 0) {
					// In the key - value format, only sequential simulations are possible. 
					// Split key and value at "=".
					String[] columns = line.split("=");
					columns[0] = columns[0].trim(); // Removes white spaces in
													// the beginning or the end
													// of the string.
					columns[1] = columns[1].trim();
					// Remove the semicollon at the end of the string in
					// columns[1].
					String temp[] = columns[1].split(";");
					columns[1] = temp[0];
					// Parse the values to the key caches in Java.
					// -- Add if-loops for variables to be read here.
					if (columns[0].equals("soilMoisture")) {
						soilMoistureCache[0] = Double.valueOf(columns[1]);
						soilMoistureCache[1] = soilMoistureCache[0];
					}

					if (columns[0].equals("gwStorage")) {
						gwStorageCache[0] = Double.valueOf(columns[1]);
						gwStorageCache[1] = gwStorageCache[0];
					}
					
				}
			}
		}
		// Close the writers.
		buff.close();
		in.close();

		// Parse the cached values to IExchangeItems.
		// -- Add commands for storing the read key-value pairs in the exchange items.
		TimeSeries soilMoistureSeries = new TimeSeries(timeCache, soilMoistureCache);
	    String id1 = "soilMoisture";
	    soilMoistureSeries.setId(id1);
	    this.items.put(id1,soilMoistureSeries);
	    @SuppressWarnings("unused")
		IExchangeItem soilMoistureExchangeItem = new TimeSeries(soilMoistureSeries);
	    
	    TimeSeries gwStorageSeries = new TimeSeries(timeCache, gwStorageCache);
	    String id2 = "gwStorage";
	    gwStorageSeries.setId(id2);
	    this.items.put(id2,gwStorageSeries);
	    @SuppressWarnings("unused")
		IExchangeItem gwStorageExchangeItem = new TimeSeries(gwStorageSeries);

	}

}
