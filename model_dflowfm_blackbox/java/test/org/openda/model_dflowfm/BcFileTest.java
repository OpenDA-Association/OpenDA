package org.openda.model_dflowfm;

import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;
import org.springframework.util.Assert;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by prevel on 20-Nov-15.
 */
public class BcFileTest extends TestCase
{
	OpenDaTestSupport testData = null;
	private File testBcFileDir;
	private String bcFileNameOriginal = "BoundaryConditions.bc";
	private String bcFileNameGenerated = "BoundaryConditions_generated.bc";

	protected void setUp()
	{
		testData = new OpenDaTestSupport(BcFileTest.class, "model_dflowfm_blackbox");
		testBcFileDir = new File(testData.getTestRunDataDir(), "BcFile");
	}

	protected void tearDown(){}

	private class MockBcFile extends BcFile
	{
		public void alterExchangeItems(double factor)
		{
			for(IExchangeItem exchangeItem : exchangeItems.values())
			{
				double[] values = exchangeItem.getValuesAsDoubles();
				for(int i = 0; i < values.length; i++) values[i] = values[i] * factor;
			}
		}
	}

	public void testBcFileUpdatesCategoriesCorrectly()
	{
		// Step 1: Read original test file
		MockBcFile bcFile = new MockBcFile();
		bcFile.initialize(testBcFileDir, new String[]{bcFileNameOriginal, bcFileNameGenerated});

		// Step 2: Alter ExchangeItem Values
		bcFile.alterExchangeItems(0.5);

		//Step 3: Write test file
		bcFile.finish();

		// Step 4: Compare written file to expected results
		Assert.isTrue(compareBcFiles(new File(testBcFileDir, bcFileNameGenerated),
				new File(testBcFileDir, "BoundaryConditions_TimeSeriesValuesHalved.bc")));
	}

	public void testBcFileReaderWriterGeneratesExpectedFile()
	{
		// Step 1: Read original test file
		BcFile bcFile = new BcFile();
		bcFile.initialize(testBcFileDir, new String[]{bcFileNameOriginal, bcFileNameGenerated});

		//Step 2: Write test file
		bcFile.finish();

		// Step 3: Compare written file to original
		Assert.isTrue(compareBcFiles(new File(testBcFileDir, bcFileNameOriginal), new File(testBcFileDir, bcFileNameGenerated)));
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
		Assert.notNull(expectedException);
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
		Assert.notNull(expectedException);
	}

	public void testBcUtilsThrowsExceptionForInvalidDateValue()
	{
		Exception expectedException = null;

		List<Double> values = new ArrayList<>();
		values.add(0.0);
		values.add(15.0);
		values.add(30.0);
		values.add(45.0);
		values.add(60.0);

		try
		{
			BcUtils.ConvertDateTimesToModifiedJulianDayValues("minutes since 2015-11-20 AA:BB:CC", values);
		}
		catch(Exception ex)
		{
			expectedException = ex;
		}
		Assert.notNull(expectedException);
	}

	public void testBcUtilsThrowsExceptionForInvalidDateUnit()
	{
		Exception expectedException = null;

		List<Double> values = new ArrayList<>();
		values.add(0.0);
		values.add(15.0);
		values.add(30.0);
		values.add(45.0);
		values.add(60.0);

		try
		{
			BcUtils.ConvertDateTimesToModifiedJulianDayValues("some other unit 2015-11-20 00:00:00", values);
		}
		catch(Exception ex)
		{
			expectedException = ex;
		}
		Assert.notNull(expectedException);
	}

	public void testBcUtilsModifiedJulianDateConversionWorks()
	{
		List<Double> values = new ArrayList<>();
		values.add(0.0);
		values.add(15.0);
		values.add(30.0);
		values.add(45.0);
		values.add(60.0);

		List<Double> modifiedValues = BcUtils.ConvertDateTimesToModifiedJulianDayValues("minutes since 2015-11-20 00:00:00", values);

		Assert.isTrue(Double.compare(modifiedValues.get(0), 57346) == 0);
		Assert.isTrue(Double.compare(modifiedValues.get(1), 57346.010416666664) == 0);
		Assert.isTrue(Double.compare(modifiedValues.get(2), 57346.020833333336) == 0);
		Assert.isTrue(Double.compare(modifiedValues.get(3), 57346.03125) == 0);
		Assert.isTrue(Double.compare(modifiedValues.get(4), 57346.041666666664) == 0);

		List<Double> retrievedValues = BcUtils.ConvertDateTimesFromModifiedJulianDayValues("minutes since 2015-11-20 00:00:00", modifiedValues);

		for(int i = 0; i < values.size(); i++)
		{
			Assert.isTrue(Double.compare(values.get(i), retrievedValues.get(i)) == 0);
		}
	}

	private boolean compareBcFiles(File bcFile1, File bcFile2)
	{
		List<String> bcFile1Lines = new ArrayList<>();
		List<String> bcFile2Lines = new ArrayList<>();

		try
		{
			BufferedReader br1 = new BufferedReader(new FileReader(bcFile1));
			BufferedReader br2 = new BufferedReader(new FileReader(bcFile2));

			String nextLine;

			while ((nextLine = br1.readLine()) != null)
			{
				bcFile1Lines.add(nextLine);
			}
			while ((nextLine = br2.readLine()) != null)
			{
				bcFile2Lines.add(nextLine);
			}

			br1.close();
			br2.close();
		}
		catch(Exception ex)
		{
			// swallow exception for tests
			return false;
		}

		if(bcFile1Lines.size() != bcFile2Lines.size()) return false;

		List<Integer> unmatchingLines = new ArrayList<>();
		for(int i = 1; i < bcFile1Lines.size(); i++) // ignore 'generated on' comment
		{
			// useful for debugging
			if(!bcFile1Lines.get(i).replaceFirst("\\s+$", "").equals(
				bcFile2Lines.get(i).replaceFirst("\\s+$", "")))
				unmatchingLines.add(i);
		}

		return (unmatchingLines.size() == 0);
	}
}
