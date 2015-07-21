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
import org.openda.interfaces.IPrevExchangeItem;
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
    private OpenDaTestSupport testData;

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
	    testData = new OpenDaTestSupport(ResultFileWrapperTest.class, "model_RainfallRunoffZhang");
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
	protected void tearDown() throws Exception {
		
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
		String args[] = {};
		// Call to initialize -> reads the file and writes exchange items.
		resultFileWrapper.initialize(testRunDataDir, fileName, args);

		IPrevExchangeItem[] resultExchangeItems = resultFileWrapper.getExchangeItems();
		
		// Iterate through all found exchange items.
        for(int item=0;item<resultExchangeItems.length;item++){

        	// If the exchange item with id totalRunoff is found. do the following tests:
        	if(resultExchangeItems[item].getId().equalsIgnoreCase("totalRunoff")){
				IPrevExchangeItem ex = resultExchangeItems[item];
				//String getId();
				String id = ex.getId();
				assertEquals("ex.getId()","totalRunoff", id);
				
				double[] values = ex.getValuesAsDoubles();
				double[] times = ex.getTimes();
				assertEquals("values.length",4,values.length); // Test if 4 values have been read.
				assertEquals("times.length",4,times.length);
				
				assertEquals("time1",time1,times[0]);
				assertEquals("time2",time2,times[1]);
				assertEquals("time3",time3,times[2]);
				assertEquals("time4",time4,times[3]);
				
				assertEquals("value1",value1,values[0]);
				assertEquals("value2",value2,values[1]);
				assertEquals("value3",value3,values[2]);
				assertEquals("value4",value4,values[3]);
				
			}

        	// Write to standard output.
			if(resultExchangeItems[item] instanceof TimeSeries){
				TimeSeriesFormatter ASCIIFormatter = new ASCIITimeSeriesFormatter();
				TimeSeries series= (TimeSeries)resultExchangeItems[item];
				ASCIIFormatter.writeToStandardOut(series);
			}else if(resultExchangeItems[item] instanceof DoublesExchangeItem){
				System.out.println("Item["+resultExchangeItems[item].getId()+"] ="+ resultExchangeItems[item].toString());
			}
		} // for
		
	} // public void testReadInput()
	
	/**
	 * testFinish() is not needed here because ResultFileWrapper does not do anything in finsh().
	 */
	public void testFinish() {
		/**
		System.out.println("-- testFinish() --");
		System.out.println("working dir: " + workingDir);
		System.out.println("fileName: " + fileName);
		// Creates the I/O-Object from the wrapper to be tested.
		IoObjectInterface resultWrapper = new ResultFileWrapper();
		String args[] = {};
		Vector<Double> timesVector = new Vector<Double>();
		Vector<Double> valuesVector = new Vector<Double>();
		double[] times = new double[4];
		double[] values = new double[4];
						
		// Call to initialize -> reads the file and writes 2 Exchange items.
		resultWrapper.initialize(workingDir, fileName, args);
				
		// Modify times and values in exchange items
		IPrevExchangeItem[] exchangeItems = resultWrapper.getExchangeItems();
		times[0] = time1 + 10;
		times[1] = time2 + 10;
		times[2] = time3 + 10;
		times[3] = time4 + 10;
		for (int item = 0; item < exchangeItems.length; item++) {
			values[0] = value1 + 1;
			values[1] = value1 + 2;
			values[2] = value1 + 3;
			values[3] = value1 + 4;
			exchangeItems[item].setValues(values);
			exchangeItems[item].setTimes(times);
		}
				
		resultWrapper.finish();
				
		// Read in the file again.
		// Create nested reader.
		File testFile = new File(workingDir, fileName);
		try{
		FileInputStream in = new FileInputStream(testFile);
		BufferedReader buff = new BufferedReader(new InputStreamReader(in));
		String line = ""; // Initialize line.
		Boolean eof = false; // End of file cache.
				
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
		} catch (IOException e) {
			System.out.println("IOException in WrapperTest -> testFinish()");
			e.printStackTrace();
		} 
		
		assertEquals(times[0],timesVector.get(0));
		assertEquals(times[1],timesVector.get(1));
		assertEquals(times[2],timesVector.get(2));
		assertEquals(times[3],timesVector.get(3));
		assertEquals(values[0],valuesVector.get(0));
		assertEquals(values[1],valuesVector.get(1));
		assertEquals(values[2],valuesVector.get(2));
		assertEquals(values[3],valuesVector.get(3));
	*/	
	}
	

}
