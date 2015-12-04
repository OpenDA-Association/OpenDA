package org.openda.model_dflowfm;

import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;
import org.springframework.util.Assert;

import java.io.File;

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

	public void testBcFileUpdatesCategoriesCorrectly()
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
		Assert.isTrue(FileComparer.CompareIniFiles(new File(testBcFileDir, "BoundaryConditions_TimeSeriesValuesHalved.bc"),
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
		Assert.isTrue(FileComparer.CompareIniFiles(new File(testBcFileDir, bcFileNameOriginal), new File(testBcFileDir, bcFileNameGenerated)));
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
}
