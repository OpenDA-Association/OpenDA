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
public class ParametersWrapperTest extends TestCase {

	// Use openDA test suite.
	private File testRunDataDir;
	private OpenDaTestSupport testData;

	private String fileName = "parametersWrapperTestInput.m"; // Needs to be created. 
	
	// Reference ids and values.
	private String id1 = "parameter_d";
	private String id2 = "parameter_Smax";
	private String id3 = "parameter_alpha1";
	private String id4 = "parameter_alpha2";
	private double value1 = 0.3;
	private double value2 = 30;
	private double value3 = 0.2;
	private double value4 = 0.4;

	/**
	 * Writes correct test file to be read by the wrapper to be tested.
	 */
	protected void setUp() throws IOException {
		
		// Use openDA test utilities. This allows the test files to be stored 
		// in a separate directory from the source code.
		testData = new OpenDaTestSupport(ResultFileWrapperTest.class, "model_RainfallRunoffZhang");
		testRunDataDir = testData.getTestRunDataDir();
		        
		// Write a test file.
		String line1 = id1 + " = " + value1 + "; % Some comment."
				+ System.getProperty("line.separator"); // Correct input line.
		String line2 = id2 + " = " + value2 + ";"
				+ System.getProperty("line.separator"); // Correct input line.
		String line3 = id3 + " = " + value3 + ";"
				+ System.getProperty("line.separator"); 
		String line4 = id4 + " = " + value4 + ";"
				+ System.getProperty("line.separator");
		BufferedWriter output = null;
		try {
			// System.out.println(workingDir + fileName);
			File file = new File(testRunDataDir, fileName);
			// file.createNewFile();
			output = new BufferedWriter(new FileWriter(file));
			output.write("% currentTime = 30000, finalTime = 40000;" + System.getProperty("line.separator"));
			output.write(line1);
			output.write(line2);
			output.write(line3);
			output.write(line4);
			output.write(System.getProperty("line.separator"));
		} catch (IOException e) {
			System.out.println("ParametersWrappterTest.setUp(): IOException");
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
		IoObjectInterface ioObject = new ParametersWrapper();
		String args[] = {};
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
			if (exchangeItems[item].getId().equalsIgnoreCase(id4)) {
				IPrevExchangeItem expectedExchangeItem = exchangeItems[item];
				String expectedId = expectedExchangeItem.getId();
				assertEquals(id4, expectedId);

				double[] expectedValue = expectedExchangeItem.getValuesAsDoubles();
				assertEquals(value4, expectedValue[0]);
			}
		}
	}
	
	
	/**
	 * Test whether or not the new initial states are written to file.
	 * @throws FileNotFoundException 
	 */
	
	public void testFinish() throws FileNotFoundException {
		// Creates the I/O-Object from the wrapper to be tested.
		IoObjectInterface parameterWrapper = new ParametersWrapper();
		String args[] = {};
		double[] times = new double[2];
		double[] values1 = new double[2];
		double[] values2 = new double[2];
		double[] values3 = new double[2];
		double[] values4 = new double[2];
				
		// Call to initialize -> reads the file and writes 2 Exchange items.
		parameterWrapper.initialize(testRunDataDir, fileName, args);
		
		IPrevExchangeItem[] exchangeItems = parameterWrapper.getExchangeItems();
		for (int item = 0; item < exchangeItems.length; item++) {
			if (exchangeItems[item].getId().equalsIgnoreCase(id1)) {
				values1 = exchangeItems[item].getValuesAsDoubles();
				values1[0] = values1[0] + 1;
				values1[1] = values1[1] + 1;
				exchangeItems[item].setValues(values1);
				times = exchangeItems[item].getTimes();
				times[0] = times[0] + 1;
				times[1] = times[1] + 1;
				exchangeItems[item].setTimes(times);
				System.out.println("% currentTime = " + times[0] + ", finalTime = " + times[1]);
			} else if (exchangeItems[item].getId().equalsIgnoreCase(id2)) {
				values2 = exchangeItems[item].getValuesAsDoubles();
				values2[0] = values2[0] + 1;
				values2[1] = values2[1] + 1;
				exchangeItems[item].setValues(values2);
				exchangeItems[item].setTimes(times);
			} else if (exchangeItems[item].getId().equalsIgnoreCase(id3)) {
				values3 = exchangeItems[item].getValuesAsDoubles();
				values3[0] = values3[0] + 0.4;
				values3[1] = values3[1] + 0.4;
				exchangeItems[item].setValues(values3);
				exchangeItems[item].setTimes(times);
			} else if (exchangeItems[item].getId().equalsIgnoreCase(id4)) {
				values4 = exchangeItems[item].getValuesAsDoubles();
				values4[0] = values4[0] - 0.4;
				values4[1] = values4[1] - 0.4;
				exchangeItems[item].setValues(values4);
				exchangeItems[item].setTimes(times);
			}
		}
		
		
		parameterWrapper.finish();
		
		// Now the states should be present in the file
		
		// Read in the file again.
		// Create nested reader.
		File testFile = new File(testRunDataDir, fileName);
		FileInputStream in = new FileInputStream(testFile);
		BufferedReader buff = new BufferedReader(new InputStreamReader(in));
		String line = ""; // Initialize line.
		Boolean eof = false; // End of file cache.
		double[] expectedTime = new double[1];
		double[] expectedParam_d = new double[1];
		double[] expectedParam_Smax = new double[1];
		double[] expectedParam_a1 = new double[1];
		double[] expectedParam_a2 = new double[1];
				
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
							System.out.println(">> Parameters : currentTime = " + tempTime[0]);
							expectedTime[0] = Double.valueOf(tempTime[0]);
						} else {
							System.out.println("ParametersWrapper.testFinish(): trouble reading current time.");
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
					//System.out.println("dbg1 - key:"+columns[0]+" = value:"+columns[1]);
					// Remove the semicollon at the end of the string in
					// columns[1].
					String temp[] = columns[1].split(";");
					columns[1] = temp[0];
					// Parse the values to the key caches in Java.
					// --> Add if-loops for variables to be read here.
					if (columns[0].equals("parameter_d")) {
						expectedParam_d[0] = Double.valueOf(columns[1]);
						System.out.println("expectedParam_d = " + expectedParam_d[0]);
					}
					if (columns[0].equals("parameter_Smax")) {
						expectedParam_Smax[0] = Double.valueOf(columns[1]);
						System.out.println("expectedParam_Smax = " + expectedParam_Smax[0]);
					}
					// System.out.println("2 - key:"+columns[0]+" = value:"+columns[1]);
					if (columns[0].equals("parameter_alpha1")) {
						expectedParam_a1[0] = Double.valueOf(columns[1]);
						System.out.println("expectedParam_a1 = " + expectedParam_a1[0]);
					}
					if (columns[0].equals("parameter_alpha2")) {
						expectedParam_a2[0] = Double.valueOf(columns[1]);
						System.out.println("expectedParam_a2 = " + expectedParam_a2[0]);
					}
				}
			}
		}
		
		try {
			buff.close();
			in.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
		assertEquals(times[0], expectedTime[0]);
		assertEquals(values1[0], expectedParam_d[0]);
		assertEquals(values2[0],expectedParam_Smax[0]);
		assertEquals(values3[0],expectedParam_a1[0]);
		assertEquals(values4[0],expectedParam_a2[0]);
		
		
		
	}

}
