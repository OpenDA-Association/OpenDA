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

import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;

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
		OpenDaTestSupport testData = new OpenDaTestSupport(ResultFileWrapperTest.class, "model_RainfallRunoffZhang");
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
	protected void tearDown() {
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
		TimesWrapper timesWrapper = new TimesWrapper();
		String[] args = {fileName};
		// Call to initialize -> reads the file and writes 2 Exchange items.
		timesWrapper.initialize(testRunDataDir, args);

		// 2 exchange items should be present: soilMoistureExchangeItem and
		// gwStorageExchangeItem.
		String[] exchangeItemIDs = timesWrapper.getExchangeItemIDs();

		for (String id : exchangeItemIDs) {
			IExchangeItem exchangeItem = timesWrapper.getDataObjectExchangeItem(id);
			String exId = exchangeItem.getId();
			assertEquals(id, exId);
			if (exId.equalsIgnoreCase(id1)) {
				double[] expectedValue = exchangeItem.getValuesAsDoubles();
				assertEquals(value1, expectedValue[0]);
			}
			if (exId.equalsIgnoreCase(id2)) {
				double[] expectedValue = exchangeItem.getValuesAsDoubles();
				assertEquals(value2, expectedValue[0]);
			}
			if (exId.equalsIgnoreCase(id3)) {
				double[] expectedValue = exchangeItem.getValuesAsDoubles();
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
		
		TimesWrapper timesWrapper = new TimesWrapper();
		String[] args = {fileName};
		timesWrapper.initialize(testRunDataDir, args);
		
		String[] exchangeItemIDs = timesWrapper.getExchangeItemIDs();
		
		// modify the entries of the exchange items.
		for (String id : exchangeItemIDs) {
			IExchangeItem exchangeItem = timesWrapper.getDataObjectExchangeItem(id);
			String exId = exchangeItem.getId();
			assertEquals(id, exId);
			if (exId.equalsIgnoreCase(id1)) {
				expectedCurrentTime = exchangeItem.getValuesAsDoubles();
				expectedCurrentTime[0] = 2;
				exchangeItem.setValuesAsDoubles(expectedCurrentTime);
			}
			if (exId.equalsIgnoreCase(id2)) {
				expectedSimTimeStep = exchangeItem.getValuesAsDoubles();
				expectedSimTimeStep[0] = 100;
				exchangeItem.setValuesAsDoubles(expectedSimTimeStep);
			}
			if (exId.equalsIgnoreCase(id3)) {
				expectedFinalTime = exchangeItem.getValuesAsDoubles();
				expectedFinalTime[0] = 21;
				exchangeItem.setValuesAsDoubles(expectedFinalTime);
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
			}
			// If the end of the file is not reached yet split line and store
			// data.
			else {
				// Now parse the line.
				// Remove comments at end of line.
				if (line.indexOf("%") > 1) {
					String[] columns = line.split("%");
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
					String[] temp = columns[1].split(";");
					columns[1] = temp[0];

					// Parse the values to the key caches in Java.
					// --> Add if-loops for variables to be read here.
					if (columns[0].equals("currentTime")) {
						readCurrentTime = Double.parseDouble(columns[1]);
					}
					if (columns[0].equals("simulationTimeStep")) {
						readSimulationTimeStep = Double.parseDouble(columns[1]);
					}
					
					if (columns[0].equals("finalTime")) {
						readFinalTime = Double.parseDouble(columns[1]);
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
