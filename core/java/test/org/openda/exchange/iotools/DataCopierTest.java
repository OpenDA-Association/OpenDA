/* OpenDA v2.4 
* Copyright (c) 2017 OpenDA Association 
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
package org.openda.exchange.iotools;
import java.io.File;
import java.io.IOException;

import junit.framework.TestCase;

import org.openda.exchange.dataobjects.NetcdfDataObject;
import org.openda.exchange.dataobjects.TestDataObject;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.io.AsciiFileUtils;

public class DataCopierTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(DataCopierTest.class, "core");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testCopyNetcdfToTextObject(){
		//read input.
		NetcdfDataObject input = new NetcdfDataObject();
		input.initialize(this.testRunDataDir, new String[]{"fews_wind_small.nc", "false", "false"});
		TestDataObject output = new TestDataObject();
		output.initialize(this.testRunDataDir, new String[]{"fews_wind_small_copy1.txt"});

		//copy data.
		DataCopier copier = new DataCopier(input, output);
		copier.copyAll();
		copier.finish();

		File actualOutputFile = new File(this.testRunDataDir, "fews_wind_small_copy1.txt");
		File expectedOutputFile = new File(this.testRunDataDir, "TestCopyNetcdfToText_expected_output.txt");
		assertEquals("Actual output file '" + actualOutputFile + "' does not equal expected output file '" + expectedOutputFile + "'.",
				AsciiFileUtils.readText(expectedOutputFile), AsciiFileUtils.readText(actualOutputFile));
	}

	public void testCopyNetcdfToTextMain(){
		File inputFile = new File(testRunDataDir, "fews_wind_small.nc");
		File outputFile = new File(testRunDataDir, "fews_wind_small_copy2.txt");
		DataCopier.main(new String[]{"-c", "org.openda.exchange.dataobjects.NetcdfDataObject", "-a", "false false", inputFile.getAbsolutePath(),
				"-c", "org.openda.exchange.dataobjects.TestDataObject", outputFile.getAbsolutePath()});

		File expectedOutputFile = new File(this.testRunDataDir, "TestCopyNetcdfToText_expected_output.txt");
		assertEquals("Actual output file '" + outputFile + "' does not equal expected output file '" + expectedOutputFile + "'.",
				AsciiFileUtils.readText(expectedOutputFile), AsciiFileUtils.readText(outputFile));
	}
	
	public void testCopyNoosToText(){
		System.out.println("-------------------------------------------------------------------");
		System.out.println("DataCopier - copy a noos file");
		System.out.println("-------------------------------------------------------------------");
		File inputFile = new File(testRunDataDir,"*.noos");
		File outputFile = new File(testRunDataDir,"waterlevel_astro.txt");
		DataCopier.main(new String[]{inputFile.getAbsolutePath(),
				"-c", "org.openda.exchange.dataobjects.TestDataObject", outputFile.getAbsolutePath()});

		File refOutputFile = new File(testRunDataDir,"waterlevel_astro.txt.ref");
		boolean equalFiles=testData.FilesAreIdentical(outputFile, refOutputFile);
		assertTrue(equalFiles);
	}
	
	//TODO also test copying to an existing file. AK
	//TODO implement. AK
	public void testCopyNetcdfToNetcdf() {
		//read input.
		NetcdfDataObject input = new NetcdfDataObject();
		input.initialize(this.testRunDataDir, new String[]{"dcsm_v5_sample.nc", "true", "false"});
		NetcdfDataObject output = new NetcdfDataObject();
		output.initialize(this.testRunDataDir, new String[]{"output3.nc", "true", "false"});

		//copy data.
		DataCopier copier = new DataCopier(input, output);
		copier.copyAll();
		copier.finish();

		//TODO this may not be an exact copy
//		NetcdfDataObject expectedOutput = new NetcdfDataObject();
//		expectedOutput.initialize(this.testRunDataDir, new String[]{"expected_output.nc"});
//		DataDumper expectedOutputDumper = new DataDumper(expectedOutput);
//		DataDumper outputDumper = new DataDumper(output);
//		expectedOutputDumper.dump();
//		outputDumper.dump();
//		assertEquals(OpenDaTestSupport.readFile(new File(this.testRunDataDir, "TestCopyNetcdfToText_expected_output.txt")),
//				OpenDaTestSupport.readFile(outputFile));
	}
}
