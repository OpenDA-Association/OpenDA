package org.openda.model_RainfallRunoffZhang;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;

import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import junit.framework.TestCase;

/**
 * Tester for a wrapper. The reading of a correct input file and the storing of
 * the read data as openDA ExchangeItems is tested. If the method is moved to another 
 * location, the path of the workingDir has to be adapted!
 * The input file has extenstion .m. 
 * 
 * @author Beatrice Marti, hydrosolutions ltd.
 *
 */
public class TimesWrapperTest extends TestCase {

	// Use openDA test suite.
	private File testRunDataDir;
    private OpenDaTestSupport testData;

	private String fileName = "timesWrapperTestInput.m"; // Needs to be created. 
	
	// Reference ids and values.
	private String id1 = "currentTime";
	private String id2 = "simulationTimeStep";
	private String id3 = "finalTime";
	private double value1 = 1;
	private double value2 = 4;
	private double value3 = 20;

	/**
	 * Writes correct test file to be read by the wrapper to be tested.
	 */
	protected void setUp() throws IOException {
		
		// Use openDA test utilities. This allows the test files to be stored 
		// in a separate directory from the source code.
		testData = new OpenDaTestSupport(ResultFileWrapperTest.class, "model_RainfallRunoffZhang");
		testRunDataDir = testData.getTestRunDataDir();
		        
		// Write a test input file.
		String line1 = id1 + " = " + value1 + "; % Some comment." + System.getProperty("line.separator"); // Correct input line.
		String line2 = id2 + " = " + value2 + ";" + System.getProperty("line.separator"); // Correct input line.
		String line3 = id3 + " = " + value3 + ";" + System.getProperty("line.separator"); 
		BufferedWriter output = null;
		try {
			File file = new File(testRunDataDir, fileName);
			output = new BufferedWriter(new FileWriter(file));
			output.write(line1);
			output.write(line2);
			output.write(line3);
			output.write(System.getProperty("line.separator"));
		} catch (IOException e) {
			System.out.println("TimesWrapperTest.setUp(): IOException");
			e.printStackTrace();
		} finally {
			if (output != null)
				output.close();
		}
	}

	/**
	 * Cleans up test file created in setUp().
	 */
	protected void tearDown() throws Exception {
		try {
			File file = new File(testRunDataDir, fileName);
			Path path = file.toPath();
			// System.out.println(path);
			Files.delete(path);
		} catch (NoSuchFileException x) {
			System.err.format("%s: no such" + " file or directory%n", fileName);
		} catch (IOException x) {
			// File permission problems are caught here.
			System.err.println(x);
		}
		
	}

	/**
	 * Test whether a correct test input file written in setUp() is read as it
	 * should.
	 */
	public void testReadInput() {

		// Creates the I/O-Object from the wrapper to be tested.
		IoObjectInterface ioObject = new TimesWrapper();
		String args[] = {};
		// Call to initialize -> reads the file and writes 2 Exchange items.
		ioObject.initialize(testRunDataDir, fileName, args);

		// 2 exchange items should be present: soilMoistureExchangeItem and
		// gwStorageExchangeItem.
		IPrevExchangeItem[] exchangeItems = ioObject.getExchangeItems();

		for (int item = 0; item < exchangeItems.length; item++) {
			if (exchangeItems[item].getId().equalsIgnoreCase(id1)) {
				IPrevExchangeItem expectedExchangeItem = exchangeItems[item];
				String expectedId = expectedExchangeItem.getId();
				assertEquals(id1, expectedId);

				// public Object getValues();
				double[] expectedValue = expectedExchangeItem.getValuesAsDoubles();
				assertEquals(value1, expectedValue[0]);
			}
			if (exchangeItems[item].getId().equalsIgnoreCase(id2)) {
				IPrevExchangeItem expectedExchangeItem = exchangeItems[item];
				String expectedId = expectedExchangeItem.getId();
				assertEquals(id2, expectedId);

				double[] expectedValue = expectedExchangeItem.getValuesAsDoubles();
				assertEquals(value2, expectedValue[0]);
			}
			if (exchangeItems[item].getId().equalsIgnoreCase(id3)) {
				IPrevExchangeItem expectedExchangeItem = exchangeItems[item];
				String expectedId = expectedExchangeItem.getId();
				assertEquals(id3, expectedId);

				double[] expectedValue = expectedExchangeItem.getValuesAsDoubles();
				assertEquals(value3, expectedValue[0]);
			}
		}
	}
	
	public void testFinish() {
		
		double readCurrentTime = 0.0;
		double readSimulationTimeStep = 0.0;
		double readFinalTime = 0.0;
		double[] expectedCurrentTime = new double[1];
		double[] expectedSimTimeStep = new double[1];
		double[] expectedFinalTime = new double[1];
		
		IoObjectInterface timesWrapper = new TimesWrapper();
		String args[] = {};
		timesWrapper.initialize(testRunDataDir, fileName, args);
		
		IPrevExchangeItem[] exchangeItems = timesWrapper.getExchangeItems();
		
		// modify the entries of the exchange items.
		for (int item = 0 ; item < exchangeItems.length; item++) {
			if (exchangeItems[item].getId().equalsIgnoreCase(id1)) {
				IPrevExchangeItem ei = exchangeItems[item];
				expectedCurrentTime = ei.getValuesAsDoubles();
				expectedCurrentTime[0] = 2;
				ei.setValuesAsDoubles(expectedCurrentTime);
			}
			if (exchangeItems[item].getId().equalsIgnoreCase(id2)) {
				IPrevExchangeItem ei = exchangeItems[item];
				expectedSimTimeStep = ei.getValuesAsDoubles();
				expectedSimTimeStep[0] = 100;
				ei.setValuesAsDoubles(expectedSimTimeStep);
			}
			if (exchangeItems[item].getId().equalsIgnoreCase(id3)) {
				IPrevExchangeItem ei = exchangeItems[item];
				expectedFinalTime = ei.getValuesAsDoubles();
				expectedFinalTime[0] = 21;
				ei.setValuesAsDoubles(expectedFinalTime);
			}
		}
		
		// write the time file.
		timesWrapper.finish();
		
		// read in the test file to check whether or not TimesWrapper.finish() worked correctly or not.
		File namelist = new File(testRunDataDir, fileName);
		if (!namelist.exists()) {
			throw new RuntimeException("TimesWrapper.ReadNameListFile(): settings file "
					+ namelist.getAbsolutePath() + " does not exist");
		}
		// Create nested reader.
		try{
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
					// --> Add if-loops for variables to be read here.
					if (columns[0].equals("currentTime")) {
						readCurrentTime = Double.valueOf(columns[1]);
					}
					if (columns[0].equals("simulationTimeStep")) {
						readSimulationTimeStep = Double.valueOf(columns[1]);
					}
					
					if (columns[0].equals("finalTime")) {
						readFinalTime = Double.valueOf(columns[1]);
					}

				}
			}
		}
		// Close the writers.
		buff.close();
		in.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		assertEquals(expectedCurrentTime[0],readCurrentTime);
		assertEquals(expectedSimTimeStep[0],readSimulationTimeStep);
		assertEquals(expectedFinalTime[0],readFinalTime);
		
	}

}
