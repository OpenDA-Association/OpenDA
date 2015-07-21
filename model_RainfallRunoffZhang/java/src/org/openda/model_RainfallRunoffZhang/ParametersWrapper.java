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
 * Allows reading of time variables from matlab readable ASCII file and
 * transferring these to OpenDA ExchangeItems. 
 * <p>
 * This code can be copied and
 * adapted to create wrappers for other blackbox models coded in matlab. Code to
 * be modified is indicated by comments starting with "// --". The input file
 * that is read by this wrapper has following format: 
 * <p>
 *   % currentTime = 26000, finalTime = 26001; <br>
 * 	 key = value; % Some optional comment. <br>
 *   key = value; % Another optional comment. <br>
 *   ... 
 * <p>
 * White spaces are optional as is the comment. Comment lines and empty lines 
 * are skipped, except if currentTime and finalTime can be read. keys that are 
 * not specified in this wrapper to be read are skipped. 
 * <p>
 * Details : In order to be able to use the TimeSeriesNoiseModel a minimum of two time
 * steps are read into the exchange item. Here these are currentTime and finalTime. 
 * Since we only have one parameter value for the current time step, this value is 
 * duplicated for the final time step. The noise model then propagates a noise with 
 * initial value given in the noise model configuration file to the next time step, 
 * here finalTime. The wrapper then writes the parameter value with noise added from 
 * finalTime to the model input file for the current time step. 
 * This is a workaround to be able to use uncertain parameters for the kalman filter 
 * algorithms of oda. In this example model, parameter noise is produced by using 
 * stochForcing="true" in the algorithm configuration and by specifying noise models 
 * for the uncertain parameters in the state vector of the stochastic model configuration. 
 * In this example, the noise model used is the TimeSeriesNoiseModel with zero time 
 * correlation length. This means that a pseudo-Gaussian number with zero mean and 
 * standard deviation of 1 will be multiplied to the standard deviation specified in the 
 * noise model configuration and this value will be added or multiplied (depending on 
 * the noise model configuration) to the parameter of the central model. However, this 
 * workaround also allows the use of time dependent noise.
 * 
 * @author Beatrice Marti, hydrosolutions ltd.
 *
 */
public class ParametersWrapper implements
		IoObjectInterface {


	// Class specific values
	File workingDir;
	String configString;
	String fileName = null;
	HashMap<String, IPrevExchangeItem> items = new LinkedHashMap<String, IPrevExchangeItem>();

	// Cache values to be read.
	// -- Add variables for initial States to be read here.
	private double[] parameter_dCache = new double[2];
	private double[] parameter_SmaxCache = new double[2];
	private double[] parameter_alpha1Cache = new double[2];
	private double[] parameter_alpha2Cache = new double[2];
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
			System.out.println("Parameter wrapper starts reading file.");
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
		IPrevExchangeItem[] result = new IPrevExchangeItem[n];
		int i = 0;
		for (String key : keys) {
			result[i] = this.items.get(key);
			i++;
		}
		return result;
	}

	public void finish() {
		// Updates parameter file.
		double[] currentTimeCache = this.items.get("parameter_d").getTimes();
		double[] parameter_dCache = this.items.get("parameter_d").getValuesAsDoubles();
		double[] parameter_SmaxCache = this.items.get("parameter_Smax").getValuesAsDoubles();
		double[] parameter_alpha1Cache = this.items.get("parameter_alpha1").getValuesAsDoubles();
		double[] parameter_alpha2Cache = this.items.get("parameter_alpha2").getValuesAsDoubles();
		
		//write to file
		System.out.println("ParametersWrapper.finish(): Writing to file: "+this.workingDir+"/"+this.fileName);
		File outputFile = new File(this.workingDir,this.fileName);
		try{
			if(outputFile.isFile()){
				outputFile.delete();
			}
		}catch (Exception e) {
			System.out.println("ParametersWrapper.finish(): trouble removing file "+ fileName);
		}
		try {
			FileWriter writer = new FileWriter(outputFile);
			BufferedWriter out = new BufferedWriter(writer);

			// Writes the parameter value with the added/multiplied with the noise to the file.
			out.write("% currentTime = " + currentTimeCache[0] + ", finalTime = " + currentTimeCache[1] + System.getProperty("line.separator"));
			out.write("parameter_d = " + parameter_dCache[1] + "; " + System.getProperty("line.separator"));
			out.write("parameter_Smax = " + parameter_SmaxCache[1] + "; " + System.getProperty("line.separator"));
			out.write("parameter_alpha1 = " + parameter_alpha1Cache[1] + "; " + System.getProperty("line.separator"));
			out.write("parameter_alpha2 = " + parameter_alpha2Cache[1] + "; " + System.getProperty("line.separator"));
			
			out.flush(); // Force java to write buffered stream to disk.
			out.close();
			writer.close();

		} catch (Exception e) {
			throw new RuntimeException("ParametersWrapper.finish(): Problem writing to file "+fileName+" :" + System.getProperty("line.separator") + e.getMessage());
		}
	}

	/**
	 * Opens file fileName in directory workingDir, reads in values and stores
	 * them to an ExchangeItem.
	 * <p>
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
			throw new RuntimeException("ParametersWrapper.ReadNameListFile(): settings file "
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
			}
			// If the end of the file is not reached yet split line and store
			// data.
			else {
				// Now parse the line.
				// Remove comments at end of line.
				if (line.indexOf("%") > 1) {
					String columns[] = line.split("%");
					line = columns[0];
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
							System.out.println("Error in parameters wrapper. Trouble reading current time.");
						}
						if (columns[4].equals("finalTime")) {
							columns[6] = columns[6].trim();
							String[] temp = columns[6].split(";");
							timeCache[1] = Double.valueOf(temp[0]);
						} else {
							System.out.println("Error in parameters wrapper. Trouble reading final time.");
						}
					}
				} else if (line.contains(System.getProperty("line.separator"))) {
					// Skip empty lines.
				} else if (line.indexOf("=") > 0) {
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
					if (columns[0].equals("parameter_d")) {
						parameter_dCache[0] = Double.valueOf(columns[1]);
						parameter_dCache[1] = parameter_dCache[0];
					}
					if (columns[0].equals("parameter_Smax")) {
						parameter_SmaxCache[0] = Double.valueOf(columns[1]);
						parameter_SmaxCache[1] = parameter_SmaxCache[0];
					}
					
					if (columns[0].equals("parameter_alpha1")) {
						parameter_alpha1Cache[0] = Double.valueOf(columns[1]);
						parameter_alpha1Cache[1] = parameter_alpha1Cache[0];
					}

					if (columns[0].equals("parameter_alpha2")) {
						parameter_alpha2Cache[0] = Double.valueOf(columns[1]);
						parameter_alpha2Cache[1] = parameter_alpha2Cache[0];
					}

				}
			}
		}
		// Close the writers.
		buff.close();
		in.close();

		// Parse the cached values to IExchangeItems.
		// -- Add commands for storing the read key-value pairs in the exchange items.
		TimeSeries parameter_dSeries = new TimeSeries(timeCache, parameter_dCache);
		parameter_dSeries.setLocation("default");
	    String id1 = "parameter_d";
	    parameter_dSeries.setId(id1);
	    this.items.put(id1,parameter_dSeries);
	    @SuppressWarnings("unused")
		IExchangeItem parameter_dExchangeItem = new TimeSeries(parameter_dSeries);
		
	    TimeSeries parameter_SmaxSeries = new TimeSeries(timeCache, parameter_SmaxCache);
		parameter_SmaxSeries.setLocation("default");
	    String id2 = "parameter_Smax";
	    parameter_SmaxSeries.setId(id2);
	    this.items.put(id2,parameter_SmaxSeries);
	    @SuppressWarnings("unused")
		IExchangeItem parameter_SmaxExchangeItem = new TimeSeries(parameter_SmaxSeries);
		
	    TimeSeries parameter_alpha1Series = new TimeSeries(timeCache, parameter_alpha1Cache);
		parameter_alpha1Series.setLocation("default");
	    String id3 = "parameter_alpha1";
	    parameter_alpha1Series.setId(id3);
	    this.items.put(id3,parameter_alpha1Series);
	    @SuppressWarnings("unused")
		IExchangeItem parameter_alpha1ExchangeItem = new TimeSeries(parameter_alpha1Series);
		
	    TimeSeries parameter_alpha2Series = new TimeSeries(timeCache, parameter_alpha2Cache);
		parameter_alpha2Series.setLocation("default");
	    String id4 = "parameter_alpha2";
	    parameter_alpha2Series.setId(id4);
	    this.items.put(id4,parameter_alpha2Series);
	    @SuppressWarnings("unused")
		IExchangeItem parameter_alpha2ExchangeItem = new TimeSeries(parameter_alpha2Series);
	    
	   
		
	}

}
