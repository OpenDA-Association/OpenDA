/* MOD_V2.0
* Copyright (c) 2013 OpenDA Association
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
package org.openda.model_dflowfm;
import junit.framework.TestCase;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.FileComparer;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;

public class DimrConfigFileTest extends TestCase
{
	private File testRunDataDir;
	private File testRunDataDirOnlyFlow1D;

	// Note: d_hydro_config files are nowadays called DIMR config files
	//       (test code and file names will be adjusted in trunk, not in
	//        the present release branch)
	private String dHydroConfigFileNameOriginal = "d_hydro_config.xml";
	private String dHydroConfigFileNameGenerated = "d_hydro_config_generated.xml";

	protected void setUp() {
		OpenDaTestSupport testData = new OpenDaTestSupport(DimrConfigFileTest.class, "model_dflowfm_blackbox");
		testRunDataDir = new File(testData.getTestRunDataDir(), "DHydroFile");
		testRunDataDirOnlyFlow1D = new File(testData.getTestRunDataDir(), "DimrConfigFileOnlyFlow1D");
	}

	public void testDHydroConfigFileUpdatesCategoriesCorrectly()
	{
		// Step 1: Read original test file
		IDataObject dHydroConfigFile = new DimrConfigFile();
		dHydroConfigFile.initialize(testRunDataDir, new String[]{dHydroConfigFileNameOriginal, dHydroConfigFileNameGenerated});

		// Step 2: Alter ExchangeItem Values
		IExchangeItem startTimeEI = dHydroConfigFile.getDataObjectExchangeItem("StartTime");
		double value = startTimeEI.getValuesAsDoubles()[0];
		startTimeEI.setValuesAsDoubles(new double[]{value + 0.5});

		IExchangeItem stopTimeEI = dHydroConfigFile.getDataObjectExchangeItem("StopTime");
		value = stopTimeEI.getValuesAsDoubles()[0];
		stopTimeEI.setValuesAsDoubles(new double[]{value + 0.5});

		//Step 2: Write test file
		dHydroConfigFile.finish();

		// Step 4: Compare written file to expected results
		String flowMd1dFileNameGenerated = "dflow1d/flow-model-1d.md1d";
		String flowMd1dFileNameExpected = "dflow1d/flow-model-1d_expected.md1d";
		String rtcRuntimeConfigNameGenerated = "rtc/rtcRuntimeConfig.xml";
		String rtcRuntimeConfigNameExpected = "rtc/rtcRuntimeConfig_expected.xml";

		assertTrue("Compare dimr config file", FileComparer.CompareXmlFiles(new File(testRunDataDir, "d_hydro_config_expected.xml"),
				new File(testRunDataDir, dHydroConfigFileNameGenerated)));
        assertTrue("Compare flow1d md1d file", FileComparer.CompareIniFiles(new File(testRunDataDir, flowMd1dFileNameExpected),
				new File(testRunDataDir, flowMd1dFileNameGenerated)));
        assertTrue("Compare rtc run time config file", FileComparer.CompareXmlFiles(new File(testRunDataDir, rtcRuntimeConfigNameExpected),
				new File(testRunDataDir, rtcRuntimeConfigNameGenerated)));
	}

	public void testDimrConfigFileOnlyFlow1D()
	{
		// Step 1: Read original test file
		IDataObject dimrConfigFile = new DimrConfigFile();
		String dimrConfigFileNameOriginal = "dimr.xml";
		String dimrConfigFileNameGenerated = "dimr_generated.xml";
		String dimrConfigFileNameExpected = "dimr_expected.xml";
		dimrConfigFile.initialize(testRunDataDirOnlyFlow1D, new String[]{dimrConfigFileNameOriginal, dimrConfigFileNameGenerated});

		// Step 2: Alter ExchangeItem Values
		IExchangeItem startTimeEI = dimrConfigFile.getDataObjectExchangeItem("StartTime");
		double value = startTimeEI.getValuesAsDoubles()[0];
		startTimeEI.setValuesAsDoubles(new double[]{value + 0.5});

		IExchangeItem stopTimeEI = dimrConfigFile.getDataObjectExchangeItem("StopTime");
		value = stopTimeEI.getValuesAsDoubles()[0];
		stopTimeEI.setValuesAsDoubles(new double[]{value + 0.5});

		//Step 2: Write test file
		dimrConfigFile.finish();

		// Step 4: Compare written file to expected results
		String flowMd1dFileNameGenerated = "dflow1d/Flow1D.md1d";
		String flowMd1dFileNameExpected = "dflow1d/Flow1D_expected.md1d";

		assertTrue("Compare dimr config file", FileComparer.CompareXmlFiles(new File(testRunDataDirOnlyFlow1D, dimrConfigFileNameExpected),
			new File(testRunDataDirOnlyFlow1D, dimrConfigFileNameGenerated)));
		assertTrue("Compare flow1d md1d file", FileComparer.CompareIniFiles(new File(testRunDataDirOnlyFlow1D, flowMd1dFileNameExpected),
			new File(testRunDataDirOnlyFlow1D, flowMd1dFileNameGenerated)));
	}

	public void testDHydroConfigFileGeneratesCorrectFileInCaseOfNoChanges()
	{
		// Step 1: Read original test file
		IDataObject dHydroConfigFile = new DimrConfigFile();
		dHydroConfigFile.initialize(testRunDataDir, new String[]{dHydroConfigFileNameOriginal, dHydroConfigFileNameGenerated});

		//Step 2: Write test file
		dHydroConfigFile.finish();

		// Step 3: Compare written file to expected results
		assertTrue("Compare resulting dimr config file", FileComparer.CompareXmlFiles(new File(testRunDataDir, dHydroConfigFileNameOriginal),
				new File(testRunDataDir, dHydroConfigFileNameGenerated)));
	}
}
