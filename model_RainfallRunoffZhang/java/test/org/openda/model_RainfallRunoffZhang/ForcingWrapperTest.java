/* MOD_V2.0 
* Copyright (c) 2012 OpenDA Association 
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

import org.openda.exchange.AbstractDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import junit.framework.TestCase;

public class ForcingWrapperTest extends TestCase {
	
	// Use openDA test suite.
	private File testRunDataDir;

	// Class types definitions:
	private String fileNamePrecip = "forcingWrapperTestPrecip.txt"; // Needs to be created. 
	private String fileNamePotET = "forcingWrapperTestPotET.txt";
	
	// Reference ids and values.
	private double time0 = 32000;
	private double time1 = 32001;
	private double time2 = 32002;
	private double value0 = 0.3;
	private double value1 = 5;
	private double value2 = 8.6;
	
	private double time00 = time0 + 10;
	private double time01 = time1 + 10;
	private double time02 = time2 + 10;

	/**
	 * Writes correct test file to be read by the wrapper to be tested.
	 */
	protected void setUp() throws IOException {
		// Use openDA test utilities. This allows the test files to be stored 
		// in a separate directory from the source code.
		OpenDaTestSupport testData = new OpenDaTestSupport(ResultFileWrapperTest.class, "model_RainfallRunoffZhang");
		testRunDataDir = testData.getTestRunDataDir();
		        
		// Write a test file.
		String line0 = time0 + " " + value0	+ System.getProperty("line.separator"); // Correct input line.
		String line1 = time1 + " " + value1 + System.getProperty("line.separator"); // Correct input line.
		String line2 = time2 + " " + value2 + System.getProperty("line.separator"); // Correct input line.
		String line00 = time00 + " " + value0	+ System.getProperty("line.separator"); // Correct input line.
		String line01 = time01 + " " + value1 + System.getProperty("line.separator"); // Correct input line.
		String line02 = time02 + " " + value2 + System.getProperty("line.separator"); // Correct input line.
		
		BufferedWriter output = null;
		try {
			//System.out.println(testRunDataDir + " , " + fileNamePrecip);
			File file = new File(testRunDataDir, fileNamePrecip);
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
		try {
			//System.out.println(testRunDataDir + " , " + fileNamePotET);
			File file = new File(testRunDataDir, fileNamePotET);
			output = new BufferedWriter(new FileWriter(file));
			output.write(line00);
			output.write(line01);
			output.write(line02);
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
	protected void tearDown() {
		
		try {
			File file = new File(testRunDataDir, fileNamePrecip);
			Path path = file.toPath();
			// System.out.println(path);
			Files.delete(path);
		} catch (NoSuchFileException x) {
			System.err.format("%s: no such" + " file or directory%n", fileNamePrecip);
		} catch (IOException x) {
			// File permission problems are caught here.
			System.err.println(x);
		}
		try {
			File file = new File(testRunDataDir, fileNamePotET);
			Path path = file.toPath();
			// System.out.println(path);
			Files.delete(path);
		} catch (NoSuchFileException x) {
			System.err.format("%s: no such" + " file or directory%n", fileNamePotET);
		} catch (IOException x) {
			// File permission problems are caught here.
			System.err.println(x);
		}
		
	}

	/**
	 * Test whether the correct test name list "testInput.txt" is read as it
	 * should.
	 */
	public void testReadPrecipitationInput() {
		// Creates the I/O-Object from the wrapper to be tested.
		AbstractDataObject forcingWrapper = new ForcingWrapper();
		String[] args = {fileNamePrecip};
		double[] expectedTimes;
		double[] expectedValues;
			
		// Call to initialize -> reads the file and writes 2 Exchange items.
		forcingWrapper.initialize(testRunDataDir, args);

		String[] exchangeItemIDs = forcingWrapper.getExchangeItemIDs();

		for (String id:exchangeItemIDs) {
			IExchangeItem expectedExchangeItem = forcingWrapper.getDataObjectExchangeItem(id);
			String exId = expectedExchangeItem.getId();
			assertEquals(id, exId);
			if (exId.equalsIgnoreCase("Precipitation")) {
				expectedTimes = expectedExchangeItem.getTimes();
				expectedValues = expectedExchangeItem.getValuesAsDoubles();
				assertEquals(value0, expectedValues[0]);
				assertEquals(time0, expectedTimes[0]);
				assertEquals(value1, expectedValues[1]);
				assertEquals(time1, expectedTimes[1]);
				assertEquals(value2, expectedValues[2]);
				assertEquals(time2, expectedTimes[2]);
			}
		}
	}
	
	public void testReadPotETInput() {
		// Creates the I/O-Object from the wrapper to be tested.
		AbstractDataObject forcingWrapper = new ForcingWrapper();
		String[] args = {fileNamePotET};
		double[] expectedTimes;
		double[] expectedValues;
			
		// Call to initialize -> reads the file and writes 2 Exchange items.
		forcingWrapper.initialize(testRunDataDir, args);

		String[] exchangeItemIDs = forcingWrapper.getExchangeItemIDs();

		for (String id:exchangeItemIDs) {
			IExchangeItem expectedExchangeItem = forcingWrapper.getDataObjectExchangeItem(id);
			String exId = expectedExchangeItem.getId();
			assertEquals(id, exId);
			if (exId.equalsIgnoreCase("potET")) {
				expectedTimes = expectedExchangeItem.getTimes();
				expectedValues = expectedExchangeItem.getValuesAsDoubles();
				assertEquals(value0, expectedValues[0]);
				assertEquals(time00, expectedTimes[0]);
				assertEquals(value1, expectedValues[1]);
				assertEquals(time01, expectedTimes[1]);
				assertEquals(value2, expectedValues[2]);
				assertEquals(time02, expectedTimes[2]);
			}
		}
	}
		
	/**
	 * Test whether or not the new forcing is written to file.
	 */
	
	public void testPrecipFinish() {
		// Creates the I/O-Object from the wrapper to be tested.
		AbstractDataObject precipWrapper = new ForcingWrapper();
		String[] args = {fileNamePrecip};
		double[] times = new double[3];
		double[] values = new double[3];
					
		// Call to initialize -> reads the file and writes 2 Exchange items.
		precipWrapper.initialize(testRunDataDir, args);
			
		// Modify times and values in exchange items
		String[] exchangeItemIDs = precipWrapper.getExchangeItemIDs();
		times[0] = time0 + 30;
		times[1] = time1 + 30;
		times[2] = time2 + 30;
		for (int i = 0; i < exchangeItemIDs.length; i++) {
			String id = exchangeItemIDs[i];
			IExchangeItem exchangeItem = precipWrapper.getDataObjectExchangeItem(id);
			values[0] = value1 + 1;
			values[1] = value1 + 2;
			values[2] = value1 + 3;
			exchangeItem.setValues(values);
			exchangeItem.setTimes(times);
			System.out.println("testPrecipFinish(): writes exchangeItems[" + i + "].setValues[0] = " + values[0]);
			System.out.println("testPrecipFinish(): writes exchangeItems[" + i + "].setValues[1] = " + values[1]);
			System.out.println("testPrecipFinish(): writes exchangeItems[" + i + "].setTimes[0] = " + times[0]);
			System.out.println("testPrecipFinish(): writes exchangeItems[" + i + "].setTimes[1] = " + times[1]);
		}
				
		precipWrapper.finish();
			
		// Now the forcing data should be present in the file
			
		// Read in the file again.
		// Create nested reader.
		File testFile = new File(testRunDataDir, fileNamePrecip);
		try{
		FileInputStream in = new FileInputStream(testFile);
		BufferedReader buff = new BufferedReader(new InputStreamReader(in));
		String line = ""; // Initialize line.
		boolean eof = false; // End of file cache.
		double[] expectedTimes = new double[3];
		double[] expectedValues = new double[3];
					
		System.out.println("Test of forcing wrapper precip finish.");
		// While End of file is not reached yet do the following:
		int index = 0;
		while (!eof) {
			// Read line.
			try {
				line = buff.readLine();
			} catch (IOException e) {
				e.printStackTrace();
			}
			//System.out.println("line : " + line);
			// Test for end of file.
			if (line == null) {
				eof = true;
			} else if (line.startsWith("#")) {
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

				// Parse the values.
				expectedTimes[index] = Double.parseDouble(columns[0]);
				expectedValues[index] = Double.parseDouble(columns[1]);
				System.out.println("testPrecipFinish(): reads times[" + index + "] = " + expectedTimes[index]);
				System.out.println("testPrecipFinish(): reads values[" + index + "] = " + expectedValues[index]);
				index = index + 1;
			}
		}
		buff.close();
		in.close();
		
		assertEquals(times[0], expectedTimes[0]);
		assertEquals(times[1], expectedTimes[1]);
		assertEquals(values[0],expectedValues[0]);
		assertEquals(values[1],expectedValues[1]);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public void testPotETFinish() {
		// Creates the I/O-Object from the wrapper to be tested.
		AbstractDataObject potETWrapper = new ForcingWrapper();
		String[] args = {fileNamePotET};
		double[] times = new double[3];
		double[] values = new double[3];
					
		// Call to initialize -> reads the file and writes 2 Exchange items.
		potETWrapper.initialize(testRunDataDir, args);
			
		// Modify times and values in exchange items
		String[] exchangeItemIDs = potETWrapper.getExchangeItemIDs();
		times[0] = time00 + 30;
		times[1] = time01 + 30;
		times[2] = time02 + 30;
		for (int i = 0; i < exchangeItemIDs.length; i++) {
			String id = exchangeItemIDs[i];
			IExchangeItem exchangeItem = potETWrapper.getDataObjectExchangeItem(id);
			values[0] = value1 + 1;
			values[1] = value1 + 2;
			values[2] = value1 + 3;
			exchangeItem.setValues(values);
			exchangeItem.setTimes(times);
			System.out.println("testPrecipFinish(): writes exchangeItems[" + i + "].setValues[0] = " + values[0]);
			System.out.println("testPrecipFinish(): writes exchangeItems[" + i + "].setValues[1] = " + values[1]);
			System.out.println("testPrecipFinish(): writes exchangeItems[" + i + "].setTimes[0] = " + times[0]);
			System.out.println("testPrecipFinish(): writes exchangeItems[" + i + "].setTimes[1] = " + times[1]);
		}
				
		potETWrapper.finish();
			
		// Now the forcing data should be present in the file
			
		// Read in the file again.
		// Create nested reader.
		try{
		File testFile = new File(testRunDataDir, fileNamePotET);
		FileInputStream in = new FileInputStream(testFile);
		BufferedReader buff = new BufferedReader(new InputStreamReader(in));
		String line = ""; // Initialize line.
		boolean eof = false; // End of file cache.
		double[] expectedTimes = new double[3];
		double[] expectedValues = new double[3];
					
		System.out.println("Test of forcing wrapper writer.");
		// While End of file is not reached yet do the following:
		int index = 0;
		while (!eof) {
			// Read line.
			try {
				line = buff.readLine();
			} catch (IOException e) {
				e.printStackTrace();
			}
			//System.out.println("line : " + line);
			// Test for end of file.
			if (line == null) {
				eof = true;
			} else if (line.startsWith("#")) {
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

				// Parse the values.
				expectedTimes[index] = Double.parseDouble(columns[0]);
				expectedValues[index] = Double.parseDouble(columns[1]);
				System.out.println("testPotETFinish(): reads times[" + index + "] = " + expectedTimes[index]);
				System.out.println("testPotETFinish(): reads values[" + index + "] = " + expectedValues[index]);
				index = index + 1;
			}
		}
		buff.close();
		in.close();
			
		assertEquals(times[0], expectedTimes[0]);
		assertEquals(times[1], expectedTimes[1]);
		assertEquals(values[0],expectedValues[0]);
		assertEquals(values[1],expectedValues[1]);
		
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
}
