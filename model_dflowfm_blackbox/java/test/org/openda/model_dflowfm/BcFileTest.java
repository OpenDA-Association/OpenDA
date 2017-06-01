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
 * Tests for IDataObject for Flow-1D's boundary condition file
 */
public class BcFileTest extends TestCase
{
	OpenDaTestSupport testData = null;
	private File testBcFileDir;
	private String bcFileNameOriginal = "BoundaryConditions.bc";
	private String bcFileNameGenerated = "BoundaryConditions_generated.bc";
	private String bcFileNameTimeSeriesValuesHalved = "BoundaryConditions_TimeSeriesValuesHalved.bc";

	protected void setUp()
	{
		testData = new OpenDaTestSupport(BcFileTest.class, "model_dflowfm_blackbox");
		testBcFileDir = new File(testData.getTestRunDataDir(), "BcFile");
	}

	protected void tearDown(){}

	public void testBcFileUpdatesCategoriesCorrectly_MultiplyValues()
	{
		// Step 1: Read original test file
		BcFile bcFile = new BcFile();
		bcFile.initialize(testBcFileDir, new String[]{bcFileNameOriginal, bcFileNameGenerated});

		// Step 2: Alter ExchangeItem Values
		String[] exchangeItemIDs = bcFile.getExchangeItemIDs();
		for(String id : exchangeItemIDs)
		{
			IExchangeItem exchangeItem = bcFile.getDataObjectExchangeItem(id);
			double[] multiplicationFactors = new double[exchangeItem.getValuesAsDoubles().length];
			for(int i = 0; i < multiplicationFactors.length; i++){	multiplicationFactors[i] = 0.5; }
			exchangeItem.multiplyValues(multiplicationFactors);
		}

		//Step 3: Write test file
		bcFile.finish();

		// Step 4: Compare written file to expected results
        assertTrue(FileComparer.CompareIniFiles(new File(testBcFileDir, bcFileNameTimeSeriesValuesHalved),
				new File(testBcFileDir, bcFileNameGenerated)));
	}

	public void testBcFileUpdatesCategoriesCorrectly_AxpyOnValues()
	{
		// Step 1: Read original test file
		BcFile bcFile = new BcFile();
		bcFile.initialize(testBcFileDir, new String[]{bcFileNameOriginal, bcFileNameGenerated});

		// Step 2: Alter ExchangeItem Values
		String[] exchangeItemIDs = bcFile.getExchangeItemIDs();
		for(String id : exchangeItemIDs)
		{
			IExchangeItem exchangeItem = bcFile.getDataObjectExchangeItem(id);
			double[] axpyValues = new double[exchangeItem.getValuesAsDoubles().length];
			for(int i = 0; i < axpyValues.length; i++){	axpyValues[i] = 0.5; }
			exchangeItem.axpyOnValues(1.0, axpyValues);
		}

		//Step 3: Write test file
		bcFile.finish();

		// Step 4: Compare written file to expected results
        assertTrue(!FileComparer.CompareIniFiles(new File(testBcFileDir, bcFileNameOriginal),
				new File(testBcFileDir, bcFileNameGenerated)));
	}

	public void testBcFileUpdatesCategoriesCorrectly_SetValuesAsDoubles()
	{
		// Step 1: Read original test file
		BcFile bcFile = new BcFile();
		bcFile.initialize(testBcFileDir, new String[]{bcFileNameOriginal, bcFileNameGenerated});

		// Step 2: Alter ExchangeItem Values
		String[] exchangeItemIDs = bcFile.getExchangeItemIDs();
		for(String id : exchangeItemIDs)
		{
			IExchangeItem exchangeItem = bcFile.getDataObjectExchangeItem(id);
			double[] values = exchangeItem.getValuesAsDoubles();
			for(int i = 0; i < values.length; i++)
			{
				values[i] = values[i] * 0.5;
			}
			exchangeItem.setValuesAsDoubles(values);
		}

		//Step 3: Write test file
		bcFile.finish();

		// Step 4: Compare written file to expected results
        assertTrue(FileComparer.CompareIniFiles(new File(testBcFileDir, bcFileNameTimeSeriesValuesHalved),
				new File(testBcFileDir, bcFileNameGenerated)));
	}

	public void testBcFileReaderWriterGeneratesExpectedFile()
	{
		// Step 1: Read original test file
		BcFile bcFile = new BcFile();
		bcFile.initialize(testBcFileDir, new String[]{bcFileNameOriginal, bcFileNameGenerated});

		//Step 2: Write test file
		bcFile.finish();

		// Step 3: Compare written file to original
        assertTrue(FileComparer.CompareIniFiles(new File(testBcFileDir, bcFileNameOriginal), new File(testBcFileDir, bcFileNameGenerated)));
	}

	public void testBcFileInitialiseThrowsExceptionForInvalidFile_MissingData()
	{
		Exception expectedException = null;
		BcFile bcFile = new BcFile();
		try
		{
			bcFile.initialize(testBcFileDir, new String[]{"BoundaryConditions_BadFormat1.bc"});
		}
		catch(Exception ex)
		{
			expectedException = ex;
		}
        assertNotNull(expectedException);
	}

	public void testBcFileInitialiseThrowsExceptionForInvalidFile_NonDoubleData()
	{
		Exception expectedException = null;
		BcFile bcFile = new BcFile();
		try
		{
			bcFile.initialize(testBcFileDir, new String[]{"BoundaryConditions_BadFormat2.bc"});
		}
		catch(Exception ex)
		{
			expectedException = ex;
		}
		assertNotNull(expectedException);
	}
}
