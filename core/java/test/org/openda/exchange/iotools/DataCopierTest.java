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
package org.openda.exchange.iotools;
import java.io.File;
import java.io.IOException;

import junit.framework.TestCase;

import org.openda.exchange.dataobjects.NetcdfDataObject;
import org.openda.exchange.dataobjects.TestDataObject;
import org.openda.utils.OpenDaTestSupport;

public class DataCopierTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(DataCopierTest.class, "core");
		testRunDataDir = testData.getTestRunDataDir();
	}
	
	public void testHelpText(){
		String help=DataCopier.getUsageMessage();
		System.out.println(help);
		assertEquals("NAME", help.substring(0, 4));
	}

	public void testCopyNetcdfToTextObject(){
		//read input.
		NetcdfDataObject input = new NetcdfDataObject();
		input.initialize(this.testRunDataDir, new String[]{"fews_wind_small.nc"});
		TestDataObject output = new TestDataObject();
		output.initialize(this.testRunDataDir, new String[]{"fews_wind_small_copy1.txt"});

		//copy data.
		DataCopier copier = new DataCopier(input, output);
		copier.copyAll();

		//write output.
		output.finish();

		assertEquals(OpenDaTestSupport.readText(new File(this.testRunDataDir, "TestCopyNetcdfToText_expected_output.txt")),
				OpenDaTestSupport.readText(new File(this.testRunDataDir, "fews_wind_small_copy1.txt")));
	}

	public void testCopyNetcdfToTextMain(){
		File inputFile = new File(testRunDataDir, "fews_wind_small.nc");
		File outputFile = new File(testRunDataDir, "fews_wind_small_copy2.txt");
		DataCopier.main(new String[]{"-c", "org.openda.exchange.dataobjects.NetcdfDataObject",inputFile.getAbsolutePath(),
				"-c", "org.openda.exchange.dataobjects.TestDataObject", outputFile.getAbsolutePath()});

		assertEquals(OpenDaTestSupport.readText(new File(this.testRunDataDir, "TestCopyNetcdfToText_expected_output.txt")),
				OpenDaTestSupport.readText(outputFile));
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
	public void todoTestCopyNetcdfToNetcdf() {
		//read input.
		NetcdfDataObject input = new NetcdfDataObject();
		input.initialize(this.testRunDataDir, new String[]{"dcsm_v5_sample.nc"});
		NetcdfDataObject output = new NetcdfDataObject();
		output.initialize(this.testRunDataDir, new String[]{"output3.nc"});

		//copy data.
		DataCopier copier = new DataCopier(input, output);
		copier.copyAll();

		//write output.
		output.finish();

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
