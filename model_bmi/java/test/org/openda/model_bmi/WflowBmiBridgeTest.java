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

import bmi.EBMI;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;

import java.io.File;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.TimeZone;

/**
 * Test that creates a BMI Thrift connection to a local WFLOW BMI model.
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
 * @author Arno Kockx
 */
public class WflowBmiBridgeTest extends AbstractModelBridgeTest {

	protected void setUp() throws Exception {
		super.setUp();

		shape = new int[] { 169, 187 };
		gridSpacing = new double[] { 0.036666665226221085, 0.036666665226221085 };
		gridOrigin = new double[]{52.05426788330078, 5.227163314819336};

		variableName = "SurfaceRunoff";
		variableType = "float32";
		variableUnits = "m^3/sec";
		variableSize = shape[0] * shape[1];
		variableByteSize = 126412;

		componentName = "wflow_hbv";
		inputVarNames = new String[] { "FreeWater", "SoilMoisture", "UpperZoneStorage", "LowerZoneStorage", "InterceptionStorage", "SurfaceRunoff", "WaterLevel", "DrySnow", "ForecQ_qmec", "P", "PET", "TEMP" };
		outputVarNames = new String[] { "FreeWater", "SoilMoisture", "UpperZoneStorage", "LowerZoneStorage", "InterceptionStorage", "SurfaceRunoff", "WaterLevel", "DrySnow", "Percolation" };

		//calculate start and end time
		try {
			DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss SSS Z");
			dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));

			startTime = dateFormat.parse("2012-01-01 00:00:00 000 +0000").getTime() / 1000.0;
			endTime = dateFormat.parse("2012-01-21 00:00:00 000 +0000").getTime() / 1000.0;
			timeStep = 3600.0 * 24.0;
		} catch (ParseException e) {
			throw new RuntimeException(e);
		}
		timeUnits = "seconds since 1970-01-01 00:00:00.0 00:00";

		attributeName = "model:InterpolationMethod";
		attributeValue = "inv";
		attributeNames = new String[]{"model:AnnualDischarge",
				"model:Alpha",
				"model:ModelSnow",
				"model:ScalarInput",
				"model:InterpolationMethod",
				"model:Tslice",
				"model:UpdMaxDist",
				"model:reinit",
				"model:fewsrun",
				"model:OverWriteInit",
				"model:updating",
				"model:updateFile",
				"model:sCatch",
				"model:intbl",
				"model:timestepsecs",
				"model:P_style",
				"model:PET_style",
				"model:TEMP_style",
				"model:MaxUpdMult",
				"model:MinUpdMult",
				"model:UpFrac",
				"model:ExternalQbase",
				"model:SetKquickFlow",
				"model:MassWasting",
				"model:SubCatchFlowOnly",
				"model:wflow_subcatch",
				"model:wflow_dem",
				"model:wflow_ldd",
				"model:wflow_river",
				"model:wflow_riverlength",
				"model:wflow_riverlength_fact",
				"model:wflow_landuse",
				"model:wflow_soil",
				"model:wflow_gauges",
				"model:wflow_inflow",
				"model:wflow_mgauges",
				"model:wflow_riverwidth",
				"model:TemperatureCorrectionMap",
				"run:starttime",
				"run:endtime",
				"run:timestepsecs",
				"run:reinit",
				"API:FreeWater",
				"API:SoilMoisture",
				"API:UpperZoneStorage",
				"API:LowerZoneStorage",
				"API:InterceptionStorage",
				"API:SurfaceRunoff",
				"API:WaterLevel",
				"API:DrySnow",
				"API:Percolation",
				"API:ForecQ_qmec",
				"API:P",
				"API:PET",
				"API:TEMP",
				"API:PERC",
				"API:FC",
				"framework:netcdfoutput",
				"framework:netcdfwritebuffer",
				"framework:outputformat",
				"framework:debug",
				"framework:netcdfinput",
				"framework:netcdfstaticoutput",
				"framework:netcdfstaticinput",
				"framework:EPSG",
				"layout:sizeinmetres",
				"outputmaps:self.SurfaceRunoff",
				"outputmaps:self.WaterLevel",
				"outputmaps:self.Percolation",
				"outputcsv_0:samplemap",
				"outputtss_0:samplemap",
				"inputmapstacks:Precipitation",
				"inputmapstacks:EvapoTranspiration",
				"inputmapstacks:Temperature",
				"inputmapstacks:Inflow",
				"inputmapstacks:Seepage"};
	}

	protected String getConfigFile() {
		return new File(testData.getTestRunDataDir(), "wflowBmiBridgeTest/case_default_hbv/wflow_hbv.ini").getAbsolutePath();
	}

	protected EBMI createModel() throws Exception {

		File modelWorkDir = new File(testData.getTestRunDataDir(), "wflowBmiBridgeTest/case_default_hbv");

		// python executable must be on the PATH environment variable.
		String pythonExecutable = "python";
		File opendaPythonPath = new File(testData.getProjectRootDir(), "bin/python");
		File modelPythonPath = new File(testData.getTestRunDataDir(), "wflow_bin");
		String modelPythonModuleName = "wflow.wflow_bmi";
		String modelPythonClassName = "wflowbmi_csdms";

		return BmiModelFactory.createModelBridge(null, pythonExecutable, opendaPythonPath, modelPythonPath, modelPythonModuleName, modelPythonClassName, modelWorkDir);
	}

	public void testGetTimeData() throws Exception {
		initializeModel();

		DateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss SSS Z");
		dateFormat.setTimeZone(TimeZone.getTimeZone("GMT"));

			double timeStep = model.getTimeStep();
			assertEquals(24 * 3600.0, timeStep);

			String timeUnits = model.getTimeUnits();
			assertEquals("seconds since 1970-01-01 00:00:00.0 00:00", timeUnits);

			double startTime = model.getStartTime();
			assertEquals("2012-01-01 00:00:00 000 +0000", dateFormat.format((long) startTime * 1000));
			assertEquals("2012-01-01 00:00:00 000 +0000", dateFormat.format(Time.mjdToMillies(TimeUtils.udUnitsTimeToMjd(startTime, timeUnits))));

			double endTime = model.getEndTime();
			assertEquals("2012-01-21 00:00:00 000 +0000", dateFormat.format((long) endTime * 1000));
			assertEquals("2012-01-21 00:00:00 000 +0000", dateFormat.format(Time.mjdToMillies(TimeUtils.udUnitsTimeToMjd(endTime, timeUnits))));

			assertEquals(model.getStartTime(), model.getCurrentTime());
	}

	public void testRunModel() throws Exception {
		initializeModel();

		while (model.getCurrentTime() < model.getEndTime()) {
			model.update();
		}

		// compare actual output files with expected output files.
		// convert netcdf data to text for human readable text comparison.
		// only compare key variables of the output to avoid out of memory
		// problems in compare in TeamCity run and in IntelliJ.
		File actualOutputFile1 = new File(testData.getTestRunDataDir(), "wflowBmiBridgeTest/case_default_hbv/run_default/outmaps.nc");
		File expectedOutputFile1 = new File(testData.getTestRunDataDir(), "wflowBmiBridgeTest/expectedResult/outmaps_expected.txt");
		assertTrue("Actual output file does not exist.", actualOutputFile1.exists());
		// TODO also compare data variables after fixing out of memory errors in
		// OpenDaTestSupport.compareNetcdfFileInTextFormat. AK
		OpenDaTestSupport.compareNetcdfFileInTextFormat(expectedOutputFile1, actualOutputFile1, "time;lat;lon");
	}
}
