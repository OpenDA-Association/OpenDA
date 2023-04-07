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
package org.openda.model_dflowfm;
import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.FileComparer;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.io.AsciiFileUtils;

import java.io.File;
import java.util.Arrays;
import java.util.List;

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

	public void testBcFileWithTabs()
	{
		// Step 1: Read original test file
		BcFile bcFile = new BcFile();
		bcFile.initialize(testBcFileDir, new String[]{"BoundaryConditions_tabs.bc", "BoundaryConditions_tabs_generated.bc"});

		// Step 2: Alter ExchangeItem Values
		String[] exchangeItemIDs = bcFile.getExchangeItemIDs();
		for(String id : exchangeItemIDs)
		{
			IExchangeItem exchangeItem = bcFile.getDataObjectExchangeItem(id);
			double[] multiplicationFactors = new double[exchangeItem.getValuesAsDoubles().length];
			Arrays.fill(multiplicationFactors, 0.5);
			exchangeItem.multiplyValues(multiplicationFactors);
		}

		//Step 3: Write test file
		bcFile.finish();

		// Step 4: Compare written file to expected results
		assertTrue(FileComparer.CompareIniFiles(new File(testBcFileDir, "BoundaryConditions_tabs_expected.bc"),
			new File(testBcFileDir, "BoundaryConditions_tabs_generated.bc")));
	}

	public void testBcFileWaterLevel() {
		// Step 1: Read original test file
		BcFile bcFile = new BcFile();
		String rewrittenFileName = "WaterLevel_rewritten.bc";
		String[] arguments = {"WaterLevel.bc", rewrittenFileName};
		bcFile.initialize(testBcFileDir, arguments);

		List<BcCategory> categories = bcFile.getCategories();

		//Step 3: Write test file
		bcFile.finish();

		String rewrittenAgainFileName = "WaterLevel_rewrittenAgain.bc";
		String[] args = {"WaterLevel_rewritten.bc", rewrittenAgainFileName};
		BcFile bcFileAgain = new BcFile();
		bcFileAgain.initialize(testBcFileDir, args);

		List<BcCategory> categoriesAgain = bcFileAgain.getCategories();

		assertEquals(3, categories.size());

		assertEquals(3, categoriesAgain.size());

		for (int i = 0; i < categories.size(); i++) {
			BcCategory bcCategory = categories.get(i);
			BcCategory bcCategoryAgain = categoriesAgain.get(i);
			assertEquals(bcCategory.getName(), bcCategoryAgain.getName());
			assertEquals(bcCategory.getProperties().size(), bcCategoryAgain.getProperties().size());
			assertEquals(bcCategory.getTable().size(), bcCategoryAgain.getTable().size());
		}

		// Step 4: Compare written file to expected results
		File expected = new File(testBcFileDir, "WaterLevel_expected.bc");
		File rewrittenFile = new File(testBcFileDir, rewrittenFileName);
		List<String> expectedLines = AsciiFileUtils.readLines(expected);
		List<String> rewrittenLines = AsciiFileUtils.readLines(rewrittenFile);
		for (int i = 1; i < rewrittenLines.size(); i++) {
			String rewrittenLine = rewrittenLines.get(i).trim();
			String expectedLine = expectedLines.get(i).trim();
			assertEquals(rewrittenLine + "\nnot equal to\n" + expectedLine, expectedLine, rewrittenLine);
		}
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
