package org.openda.model_dflowfm;

import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.FileComparer;
import org.openda.utils.OpenDaTestSupport;
import org.springframework.util.Assert;

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
		Assert.isTrue(FileComparer.CompareIniFiles(new File(testMd1dFileDir, "Model_TimeValuesHalved.md1d"),
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
		Assert.notNull(expectedException);
	}

	public void testMd1dFileGeneratesExpectedFile()
	{
		// Step 1: Read original test file
		Md1dFile md1dFile = new Md1dFile();
		md1dFile.initialize(testMd1dFileDir, new String[]{md1dFileNameOriginal, md1dFileNameGenerated});

		//Step 2: Write test file
		md1dFile.finish();

		// Step 3: Compare written file to expected results
		Assert.isTrue(FileComparer.CompareIniFiles(new File(testMd1dFileDir, md1dFileNameOriginal),
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
		Assert.notNull(expectedException);
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
		Assert.notNull(expectedException);
	}
}
