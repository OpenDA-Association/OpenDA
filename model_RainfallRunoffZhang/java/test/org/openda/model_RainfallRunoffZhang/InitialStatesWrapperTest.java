package org.openda.model_RainfallRunoffZhang;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
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
 * the read data as openDA ExchangeItems is tested. 
 * The input file has extenstion .m. 
 * 
 * @author Beatrice Marti, hydrosolutions ltd.
 *
 */
public class InitialStatesWrapperTest extends TestCase {

	// Use openDA test suite.
	private File testRunDataDir;
    private OpenDaTestSupport testData;

	// Class types definitions:
	private String fileName = "initialStatesTestInput.m"; // Needs to be created. 
	
	// Reference ids and values.
	private String id0 = "currentTime";
	private String id1 = "soilMoisture";
	private String id2 = "gwStorage";
	private double value0 = 33000;
	private double value1 = 5;
	private double value2 = 8.6;

	/**
	 * Writes correct test file to be read by the wrapper to be tested.
	 */
	protected void setUp() throws IOException {
		// Use openDA test utilities. This allows the test files to be stored 
		// in a separate directory from the source code.
		testData = new OpenDaTestSupport(ResultFileWrapperTest.class, "model_RainfallRunoffZhang");
		testRunDataDir = testData.getTestRunDataDir();
		        
		// Write a test file.
		String line0 = "% " + id0 + " = " + value0 + ", " + " finalTime " + " = " + value0 + System.getProperty("line.separator");
		String line1 = id1 + " = " + value1 + "; % Some comment."
				+ System.getProperty("line.separator"); // Correct input line.
		String line2 = id2 + " = " + value2 + ";"
				+ System.getProperty("line.separator"); // Correct input line.
		BufferedWriter output = null;
		try {
			System.out.println(testRunDataDir + " , " + fileName);
			File file = new File(testRunDataDir, fileName);
			// file.createNewFile();
			output = new BufferedWriter(new FileWriter(file));
			output.write(line0);
			output.write(line1);
			output.write(line2);
			output.write(System.getProperty("line.separator"));
		} catch (IOException e) {
			System.out.println("IOException in WrapperTest -> setUp()");
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
	 * Test whether the correct test name list "testInput.txt" is read as it
	 * should.
	 */
	public void testReadInput() {

		// Creates the I/O-Object from the wrapper to be tested.
		IoObjectInterface ioObject = new InitialStatesWrapper();
		String args[] = {};
		double[] expectedTimes = new double[1];
		double[] expectedTimes2 = new double[1];
		double[] expectedValue = new double[1];
		
		// Call to initialize -> reads the file and writes 2 Exchange items.
		ioObject.initialize(testRunDataDir, fileName, args);

		// 2 exchange items should be present: soilMoistureExchangeItem and
		// gwStorageExchangeItem.
		IPrevExchangeItem[] exchangeItems = ioObject.getExchangeItems();

		for (int item = 0; item < exchangeItems.length; item++) {
			if (exchangeItems[item].getId().equalsIgnoreCase(id1)) {
				IPrevExchangeItem expectedExchangeItem = exchangeItems[item];
				// String getId();
				String expectedId = expectedExchangeItem.getId();
				assertEquals(id1, expectedId);

				// public Object getValues();
				expectedTimes = (double[]) expectedExchangeItem.getTimes();
				expectedValue = (double[]) expectedExchangeItem.getValuesAsDoubles();
				assertEquals(value1, expectedValue[0]);
				assertEquals(value0, expectedTimes[0]);
			}
			if (exchangeItems[item].getId().equalsIgnoreCase(id2)) {
				IPrevExchangeItem expectedExchangeItem = exchangeItems[item];
				String expectedId = expectedExchangeItem.getId();
				assertEquals(id2, expectedId);

				expectedTimes2 = (double[]) expectedExchangeItem.getTimes();
				expectedValue = (double[]) expectedExchangeItem.getValuesAsDoubles();
				assertEquals(value2, expectedValue[0]);
				assertEquals(value0, expectedTimes2[0]);
				
				
			}
		}

		assertEquals(expectedTimes[0], expectedTimes2[0]);
		
	}
	
	/**
	 * Test whether or not the new initial states are written to file.
	 * @throws FileNotFoundException 
	 */
	
	public void testFinish() throws FileNotFoundException {
		// Creates the I/O-Object from the wrapper to be tested.
		IoObjectInterface initialStatesWrapper = new InitialStatesWrapper();
		String args[] = {};
		double[] times = new double[2];
		double[] values = new double[2];
		double[] values2 = new double[2];
				
		// Call to initialize -> reads the file and writes 2 Exchange items.
		initialStatesWrapper.initialize(testRunDataDir, fileName, args);
		
		// Modify times and values in exchange items
		IPrevExchangeItem[] exchangeItems = initialStatesWrapper.getExchangeItems();
		times[0] = value0 + 1;
		times[1] = value0 + 2;
		for (int item = 0; item < exchangeItems.length; item++) {
			if (exchangeItems[item].getId().equalsIgnoreCase(id1)) {
				values[0] = value1 + 1;
				values[1] = value1 + 2;
				exchangeItems[item].setValues(values);
				exchangeItems[item].setTimes(times);
			} else if (exchangeItems[item].getId().equalsIgnoreCase(id2)) {
				values2[0] = value2 + 1;
				values2[1] = value2 + 2;
				exchangeItems[item].setValues(values2);
				exchangeItems[item].setTimes(times);
				
			}
		}
		
		
		initialStatesWrapper.finish();
		
		// Now the states should be present in the file
		
		// Read in the file again.
		// Create nested reader.
		try{
		File testFile = new File(testRunDataDir, fileName);
		FileInputStream in = new FileInputStream(testFile);
		BufferedReader buff = new BufferedReader(new InputStreamReader(in));
		String line = ""; // Initialize line.
		Boolean eof = false; // End of file cache.
		double[] expectedTime = new double[2];
		double[] expectedSoilMoisture = new double[1];
		double[] expectedGwStorage = new double[1];
				
		System.out.println("Test of initial states wrapper writer.");
		// While End of file is not reached yet do the following:
		while (!eof) {
			// Read line.
			try {
				line = buff.readLine();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
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
				}
				if (line.startsWith("%")) {
					// If the lines starts with comment and it is possible to read 
					// currentTime, do that. The first entry of columns will contain %, 
					// the second currentTime, the third =, and the fourth the value 
					// for currentTime. This is followed by some more comments. 
					String[] columns = line.split(" +");
					if (columns.length > 6) { 
						// Read input of the format: % currentTime = 333, finalTime = 444
						columns[1] = columns[1].trim();
						if (columns[1].equals("currentTime")) {
							columns[3] = columns[3].trim();
							// Remove comma from columns[3].
							String[] tempTime = columns[3].split(",");
							expectedTime[0] = Double.valueOf(tempTime[0]);
						} else {
							System.out.println("InitialStatesWrapperTest.TestFinish(): Trouble reading current time.");
						}
						
						if (columns[4].equals("finalTime")) {
							columns[6] = columns[6].trim();
							expectedTime[1] = Double.valueOf(columns[6]);
						} else {
							System.out.println("InitialStatesWrapperTest.TestFinish(): Trouble reading final time.");
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
					// --> Add if-loops for variables to be read here.
					if (columns[0].equals("soilMoisture")) {
						expectedSoilMoisture[0] = Double.valueOf(columns[1]);
						System.out.println(">> soilMoistureCache = " + expectedSoilMoisture[0]);
					}
					if (columns[0].equals("gwStorage")) {
						expectedGwStorage[0] = Double.valueOf(columns[1]);
						System.out.println(">> gwStorageCache = " + expectedGwStorage[0]);
					}
				}
			}
		}
		
		buff.close();
		in.close();
		
		assertEquals(times[0], expectedTime[0]);
		assertEquals(values[1], expectedSoilMoisture[0]);
		assertEquals(values2[1],expectedGwStorage[0]);
		
		} catch (IOException e) {
			e.printStackTrace();
		}
		
	}
	

}
