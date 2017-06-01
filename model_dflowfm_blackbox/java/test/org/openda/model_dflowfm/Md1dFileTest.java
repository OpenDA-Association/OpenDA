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
 * Tests for Data object implementation for FLOW 1D's md1d-file
 */
public class Md1dFileTest extends TestCase
{
	OpenDaTestSupport testData = null;
	private File testMd1dFileDir;
	private String md1dFileNameOriginal = "Model.md1d";
	private String md1dFileNameGenerated = "Model_generated.md1d";

	protected void setUp()
	{
		testData = new OpenDaTestSupport(Md1dFileTest.class, "model_dflowfm_blackbox");
		testMd1dFileDir = new File(testData.getTestRunDataDir(), "Md1dFile");
	}

	protected void tearDown(){}

	public void testMd1dFileUpdatesCategoriesCorrectly()
	{
		// Step 1: Read original test file
		Md1dFile md1dFile = new Md1dFile();
		md1dFile.initialize(testMd1dFileDir, new String[]{md1dFileNameOriginal, md1dFileNameGenerated});

		// Step 2: Alter ExchangeItem Values
		IExchangeItem startTimeEI = md1dFile.getDataObjectExchangeItem("StartTime");
		double value = startTimeEI.getValuesAsDoubles()[0];
		startTimeEI.setValuesAsDoubles(new double[]{value * 0.5});

		IExchangeItem stopTimeEI = md1dFile.getDataObjectExchangeItem("StopTime");
		value = stopTimeEI.getValuesAsDoubles()[0];
		stopTimeEI.setValuesAsDoubles(new double[]{value * 0.5});

		IExchangeItem outputTimeStepEI = md1dFile.getDataObjectExchangeItem("OutTimeStepGridPoints");
		value = outputTimeStepEI.getValuesAsDoubles()[0];
		outputTimeStepEI.setValuesAsDoubles(new double[]{value * 0.5});

		//Step 3: Write test file
		md1dFile.finish();

		// Step 4: Compare written file to expected results
		assertTrue(FileComparer.CompareIniFiles(new File(testMd1dFileDir, "Model_TimeValuesHalved.md1d"),
				new File(testMd1dFileDir, md1dFileNameGenerated)));
	}

	public void testMd1dFileThrowsIfUpdatingTimeStep()
	{
		// Step 1: Read original test file
		Md1dFile md1dFile = new Md1dFile();
		md1dFile.initialize(testMd1dFileDir, new String[]{md1dFileNameOriginal, md1dFileNameGenerated});

		// Step 2: Alter ExchangeItem Values
		Exception expectedException = null;
		try
		{
			//md1dFile.alterTimeStep();
			IExchangeItem timeStepEI = md1dFile.getDataObjectExchangeItem("TimeStep");
			timeStepEI.setValuesAsDoubles(new double[]{0.0});
		}
		catch(Exception ex)
		{
			expectedException = ex;
		}
		assertNotNull(expectedException);
	}

	public void testMd1dFileGeneratesExpectedFile()
	{
		// Step 1: Read original test file
		Md1dFile md1dFile = new Md1dFile();
		md1dFile.initialize(testMd1dFileDir, new String[]{md1dFileNameOriginal, md1dFileNameGenerated});

		//Step 2: Write test file
		md1dFile.finish();

		// Step 3: Compare written file to expected results
		assertTrue(FileComparer.CompareIniFiles(new File(testMd1dFileDir, md1dFileNameOriginal),
				new File(testMd1dFileDir, md1dFileNameGenerated)));

	}

	public void testMd1dFileInitialiseThrowsExceptionForInvalidFile_MissingData()
	{
		Exception expectedException = null;
		Md1dFile md1dFile = new Md1dFile();
		try
		{
			md1dFile.initialize(testMd1dFileDir, new String[]{"Model_BadFormat1.md1d", ""});
		}
		catch(Exception ex)
		{
			expectedException = ex;
		}
		assertNotNull(expectedException);
	}

	public void testMd1dFileInitialiseThrowsExceptionForInvalidFile_NonDoubleData()
	{
		Exception expectedException = null;
		Md1dFile md1dFile = new Md1dFile();
		try
		{
			md1dFile.initialize(testMd1dFileDir, new String[]{"Model_BadFormat2.md1d", ""});
		}
		catch(Exception ex)
		{
			expectedException = ex;
		}
		assertNotNull(expectedException);
	}
}
