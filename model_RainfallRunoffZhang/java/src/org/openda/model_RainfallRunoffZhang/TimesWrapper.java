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
import org.openda.exchange.DoubleExchangeItem;
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
 * 	key = value; % Some optional comment. <br>
 *  ...
 * <p>
 * White spaces are optional as is the comment. Comment lines and empty lines 
 * are skipped. keys that are not specified in this wrapper to be read are skipped. 
 * <p>
 * Note: Time specifications are the only exchange items which are not stored in 
 * time series but in conventional IExchangeItems. 
 * 
 * @author Beatrice Marti, hydrosolutions ltd.
 *
 */
public class TimesWrapper implements
		IoObjectInterface {

	// Define a private class to cache the exchange items.
	private class TimesWrapperExchangeItem {
		public IExchangeItem exchangeItem;

		TimesWrapperExchangeItem(IExchangeItem exchangeItem) {
			this.exchangeItem = exchangeItem;
		}
	}

	// Class specific values
	File workingDir;
	String configString;
	String fileName = null;
	HashMap<String, TimesWrapperExchangeItem> items = new LinkedHashMap<String, TimesWrapperExchangeItem>();

	// Cache values to be read.
	// -- Add variables for initial States to be read here.
	private double currentTimeCache = 0.0;
	private double simulationTimeStepCache = 0.0;
	private double finalTimeCache = 0.0;

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
			result[i] = this.items.get(key).exchangeItem;
			i++;
		}
		return result;
	}

	public void finish() {
		// Updates time configuration file.
		double currentTime = this.items.get("currentTime").exchangeItem.getValuesAsDoubles()[0];
		double simulationTimeStep = this.items.get("simulationTimeStep").exchangeItem.getValuesAsDoubles()[0];
		double finalTime = this.items.get("finalTime").exchangeItem.getValuesAsDoubles()[0];
		
		//write to file
		System.out.println("TimesWrapper.finish(): writing to " +this.workingDir+"/"+this.fileName);
		File outputFile = new File(this.workingDir,this.fileName);
		try{
			if(outputFile.isFile()){
				outputFile.delete();
			}
		}catch (Exception e) {
			System.out.println("TimesWrapper.finish(): trouble removing file "+ fileName);
		}
		try {
			FileWriter writer = new FileWriter(outputFile);
			BufferedWriter out = new BufferedWriter(writer);

			out.write("currentTime = " + currentTime + ";\n");
			out.write("simulationTimeStep = " + simulationTimeStep + ";\n");
			out.write("finalTime = " + finalTime + ";\n");
			
			System.out.println("TimesWrapper.finish(): writes times [ " + currentTime + " , " + simulationTimeStep + " , " + finalTime + " ]");
			
			out.close();
			writer.close();

		} catch (Exception e) {
			throw new RuntimeException("TimesWrapper.finish(): Problem writing to file "+fileName+" :\n "+e.getMessage());
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
			throw new RuntimeException("TimesWrapper.ReadNameListFile(): settings file "
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
					// If the lines starts with comment or meta data do nothing.
				} else if (line.contains(System.getProperty("line.separator"))) {
					// Skip empty lines.
				} else if (line.indexOf("=") > 0) {
					// Split key and value at "=".
					String[] columns = line.split("=");
					columns[0] = columns[0].trim(); // Removes white spaces in
													// the beginning or the end
													// of the string.
					columns[1] = columns[1].trim();
					// Remove the semicollon at the end of the string in columns[1].
					String temp[] = columns[1].split(";");
					columns[1] = temp[0];

					// Parse the values to the key caches in Java.
					// -- Add if-loops for variables to be read here.
					if (columns[0].equals("currentTime")) {
						currentTimeCache = Double.valueOf(columns[1]);
					}
					if (columns[0].equals("simulationTimeStep")) {
						simulationTimeStepCache = Double.valueOf(columns[1]);
					}
					
					if (columns[0].equals("finalTime")) {
						finalTimeCache = Double.valueOf(columns[1]);
					}

				}
			}
		}
		// Close the writers.
		buff.close();
		in.close();

		// Parse the cached values to IExchangeItems.
		// -- Add commands for storing the read key-value pairs in the exchange items.
		IExchangeItem currentTimeExchangeItem = new DoubleExchangeItem(
				"currentTime", this.currentTimeCache);
		this.items.put("currentTime", new TimesWrapperExchangeItem(
				currentTimeExchangeItem));
        
        IExchangeItem finalTimeExchangeItem = new DoubleExchangeItem(
                "finalTime", this.finalTimeCache);
        this.items.put("finalTime", new TimesWrapperExchangeItem(
                finalTimeExchangeItem));
       
        IExchangeItem simulationTimeStepExchangeItem = new DoubleExchangeItem(
                "simulationTimeStep", this.simulationTimeStepCache);
        this.items.put("simulationTimeStep", new TimesWrapperExchangeItem(
                simulationTimeStepExchangeItem));
        
	}

}
