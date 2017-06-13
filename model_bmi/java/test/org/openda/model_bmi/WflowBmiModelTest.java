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

package org.openda.model_bmi;

import junit.framework.TestCase;
import org.openda.application.OpenDaApplication;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.StochVector;

import java.io.File;
import java.io.IOException;

/**
 * Test for running a BMI model (WFLOW).
 *
 * For information about the WFLOW model see www.openstreams.org
 * and https://publicwiki.deltares.nl/display/OpenS/wflow+-+PCRaster-Python+based+distributed+hydrological+models
 * For information about the PCRaster framework see http://pcraster.geo.uu.nl/
 *
 * For this test to work, CPython version 2.7, PCRaster version 4.0 and the Thrift Python package need to be installed.
 * For this test to work, the openda python code and python resources must be present in the folder public/bin/python.
 * For this test to work, the following folders need to be present in the corresponding environment variables:
 * PATH: folder containing python executable (e.g. C:\Anaconda),
 *       folder with PCRaster dll files (e.g. C:\pcraster-4.0.1_x86-64\bin),
 *       folder with gdal dll files (e.g. C:\programs\gdal_release-1800-x64-gdal-1-11-1-mapserver-6-4-1\bin)
 * PYTHONPATH: folder with PCRaster python scripts (e.g. C:\pcraster-4.0.1_x86-64\python)
 * PYTHONHOME: folder containing python executable (e.g. C:\Anaconda\)
 *
 * This test only works when the OpenDA bin directory is the current working directory.
 */
public class WflowBmiModelTest extends TestCase {
	private OpenDaTestSupport testData;
	private File testRunDataDir;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(WflowBmiModelTest.class, "model_bmi");
		testRunDataDir = testData.getTestRunDataDir();
	}

	/**
	 * This test class only contains methods for manual testing.
	 * Added this empty test method to avoid causing JUnit
	 * to throw Exception "No runnable methods".
	 */
	public void testEmpty() {
	}

	/**
	 * Manual test for running a BMI model (WFLOW).
	 * This uses the EnKF algorithm.
	 */
	public void _testWflowBmiModel() throws Exception {

		StochVector.setSeed(42);

		//testRunDataDir is public/opendaTestRuns/model_bmi/org/openda/model_bmi/
		File workDir = new File(testRunDataDir, "wflowBmiModelTest");

		//run.
		//oda and stochObserver file paths need to be relative to the current working dir or absolute.
		File odaFile = new File(workDir, "openda_config/Murrumbegee/OS_SBM_enkf.oda");
		String args[] = new String[1];
		args[0] = odaFile.getAbsolutePath();
		OpenDaApplication.main(args);

		//compare actual output files with expected output files.
		//convert netcdf data to text for human readable text comparison.
		//only compare key variables of the output to avoid out of memory problems in compare in TeamCity run.
		File expectedOutputFile1 = new File(workDir, "expectedResult/work0/model_grid_data_before_analysis.txt");
		File actualOutputFile1 = new File(workDir, "work0/model_grid_data_before_analysis.nc");
		OpenDaTestSupport.compareNetcdfFileInTextFormat(expectedOutputFile1, actualOutputFile1, "time;y;x;WaterLevel");

		File expectedOutputFile2 = new File(workDir, "expectedResult/work2/model_grid_data_after_analysis.txt");
		File actualOutputFile2 = new File(workDir, "work2/model_grid_data_after_analysis.nc");
		OpenDaTestSupport.compareNetcdfFileInTextFormat(expectedOutputFile2, actualOutputFile2, "time;y;x;WaterLevel");

		File expectedOutputFile3 = new File(workDir, "expectedResult/outmaps.txt");
		File actualOutputFile3 = new File(workDir, "work1/run_default/outmaps.nc");
		OpenDaTestSupport.compareNetcdfFileInTextFormat(expectedOutputFile3, actualOutputFile3, "time;lat;lon;run");

		File outputStateFile = new File(workDir, "states/outstate.zip");
		assertTrue("Output state file does not exist.", outputStateFile.exists());
	}
}
