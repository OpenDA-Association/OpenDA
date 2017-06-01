/* OpenDA v2.3.1 
* Copyright (c) 2016 OpenDA Association 
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
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.FileComparer;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;

/**
 * Created by prevel on 30-Nov-15.
 */
public class RtcToolsRuntimeConfigFileTest extends TestCase
{
	OpenDaTestSupport testData = null;
	private File testRtcToolsRuntimeConfigFileDir;
	private String RtcToolsRuntimeConfigFileName = "rtc/rtcRuntimeConfig.xml";
	private String RtcToolsRuntimeConfigFileNameGenerated = "rtc/rtcRuntimeConfig_generated.xml";
	private String RtcToolsRuntimeConfigFileNameExpected = "rtc/rtcRuntimeConfig_expected.xml";

	protected void setUp()
	{
		testData = new OpenDaTestSupport(BcFileTest.class, "model_dflowfm_blackbox");
		testRtcToolsRuntimeConfigFileDir = new File(testData.getTestRunDataDir(), "DHydroFile");
	}

	protected void tearDown(){}

	public void testRtcToolsRuntimeConfigFileUpdatesCategoriesCorrectly()
	{
		// Step 1: Read original test file
		RtcToolsRuntimeConfigFile RtcToolsRuntimeConfigFile = new RtcToolsRuntimeConfigFile();
		RtcToolsRuntimeConfigFile.initialize(testRtcToolsRuntimeConfigFileDir, new String[]{RtcToolsRuntimeConfigFileName});

		// Step 2: Alter ExchangeItem Values
		IExchangeItem startTimeEI = RtcToolsRuntimeConfigFile.getDataObjectExchangeItem("StartTime");
		double value = startTimeEI.getValuesAsDoubles()[0];
		startTimeEI.setValuesAsDoubles(new double[]{value + 0.5});

		IExchangeItem stopTimeEI = RtcToolsRuntimeConfigFile.getDataObjectExchangeItem("StopTime");
		value = stopTimeEI.getValuesAsDoubles()[0];
		stopTimeEI.setValuesAsDoubles(new double[]{value + 0.5});

		//Step 3: Write test file
		RtcToolsRuntimeConfigFile.finish();

		// Step 4: Compare written file to expected results
		assertTrue(FileComparer.CompareXmlFiles(new File(testRtcToolsRuntimeConfigFileDir, RtcToolsRuntimeConfigFileName),
				new File(testRtcToolsRuntimeConfigFileDir, RtcToolsRuntimeConfigFileNameExpected)));
	}

	public void testRtcToolsRuntimeConfigFileGeneratesExpectedFile()
	{
		// Step 1: Read original test file
		RtcToolsRuntimeConfigFile RtcToolsRuntimeConfigFile = new RtcToolsRuntimeConfigFile();
		RtcToolsRuntimeConfigFile.initialize(testRtcToolsRuntimeConfigFileDir, new String[]{RtcToolsRuntimeConfigFileName, RtcToolsRuntimeConfigFileNameGenerated});

		//Step 2: Write test file
		RtcToolsRuntimeConfigFile.finish();

		// Step 3: Compare written file to expected results
		assertTrue(FileComparer.CompareXmlFiles(new File(testRtcToolsRuntimeConfigFileDir, RtcToolsRuntimeConfigFileName),
				new File(testRtcToolsRuntimeConfigFileDir, RtcToolsRuntimeConfigFileNameGenerated)));

	}
}
