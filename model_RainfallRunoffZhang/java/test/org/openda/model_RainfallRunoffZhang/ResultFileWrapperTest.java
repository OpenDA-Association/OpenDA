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
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;

import org.openda.exchange.DoublesExchangeItem;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.exchange.timeseries.TimeSeriesFormatter;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import junit.framework.TestCase;

/**
 * Tester for a wrapper. The reading of a correct input file and the storing of
 * the read data as openDA ExchangeItems is tested. 
 * 
 * @author Beatrice Marti, hydrosolutions ltd.
 *
 */
public class ResultFileWrapperTest extends TestCase {

	// Use openDA test suite.
	private File testRunDataDir;

	private String fileName = "resultFileWrapperTestInput.txt"; // Needs to be created. 

	// Reference values.
	private double time1 = 30000;
	private double time2 = 30001;
	private double time3 = 30002;
	private double time4 = 30010;
	private double value1 = 0.3;
	private double value2 = 0.45;
	private double value3 = 2;
	private double value4 = 8.000000e-01;
	
	/**
	 * Writes correct test file to be read by the wrapper to be tested.
	 */
	protected void setUp() throws IOException {
	
		// Use openDA test utilities. This allows the test files to be stored 
		// in a separate directory from the source code.
		OpenDaTestSupport testData = new OpenDaTestSupport(ResultFileWrapperTest.class, "model_RainfallRunoffZhang");
        testRunDataDir = testData.getTestRunDataDir();
        
		// Write a test file.
		BufferedWriter output = null;
		try {
			//System.out.println("WorkingDir : " + workingDir);
			//System.out.println("FileName : " + fileName);
			//File file = new File(workingDir, fileName);
			File file = new File(testRunDataDir, fileName);
			output = new BufferedWriter(new FileWriter(file));
			output.write("" + time1 + " " +value1 + "\n");
			output.write("" + time2 + " " +value2 + "\n");
			output.write("" + time3 + " " +value3 + "\n");
			output.write("" + time4 + " " +value4 + "\n");
			output.write(System.getProperty("line.separator"));
		} catch (IOException e) {
			System.out.println("IOException in WrapperTest -> setUp()");
			e.printStackTrace();
		} finally {
			if (output != null)
				output.close();
		}
		File file = new File(testRunDataDir, fileName);
		if (!file.exists()) {
			System.out.println("Error ResultWrapperTest.setUp(): Failed opening file " + fileName + " in " + testRunDataDir);
		} else {
			System.out.println("ResultFileWrapperTest.setUp(): Success: File " + fileName + " is present in " + testRunDataDir);
		}
		
	}

	/**
	 * Cleans up (deletes) test file created in setUp().
	 */
	protected void tearDown() {
		
		try {
			File file = new File(testRunDataDir, fileName);
			Path path = file.toPath();
			System.out.println("ResultFileWrapperTest.tearDown(): removing file: " + path);
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
		ResultFileWrapper resultFileWrapper = new ResultFileWrapper();
		String[] args = {fileName};
		// Call to initialize -> reads the file and writes exchange items.
		resultFileWrapper.initialize(testRunDataDir, args);

		String[] exchangeItemIDs = resultFileWrapper.getExchangeItemIDs();
		
		// Iterate through all found exchange items.
		for (String id : exchangeItemIDs) {
			IExchangeItem exchangeItem = resultFileWrapper.getDataObjectExchangeItem(id);
			String exId = exchangeItem.getId();
			assertEquals(exId, id);

			// If the exchange item with id totalRunoff is found, do the following tests:
			if (exId.equalsIgnoreCase("totalRunoff")) {
				assertEquals("ex.getId()", "totalRunoff", exId);

				double[] values = exchangeItem.getValuesAsDoubles();
				double[] times = exchangeItem.getTimes();
				assertEquals("values.length", 4, values.length); // Test if 4 values have been read.
				assertEquals("times.length", 4, times.length);

				assertEquals("time1", time1, times[0]);
				assertEquals("time2", time2, times[1]);
				assertEquals("time3", time3, times[2]);
				assertEquals("time4", time4, times[3]);

				assertEquals("value1", value1, values[0]);
				assertEquals("value2", value2, values[1]);
				assertEquals("value3", value3, values[2]);
				assertEquals("value4", value4, values[3]);

			}

			// Write to standard output.
			if (exchangeItem instanceof TimeSeries) {
				TimeSeriesFormatter ASCIIFormatter = new ASCIITimeSeriesFormatter();
				TimeSeries series = (TimeSeries) exchangeItem;
				ASCIIFormatter.writeToStandardOut(series);
			} else if (exchangeItem instanceof DoublesExchangeItem) {
				System.out.println("Item[" + exchangeItem.getId() + "] =" + exchangeItem.toString());
			}
		} // for
		
	} // public void testReadInput()

}
