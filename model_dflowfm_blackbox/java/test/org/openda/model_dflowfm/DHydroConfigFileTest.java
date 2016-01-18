package org.openda.model_dflowfm;

import junit.framework.TestCase;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.FileComparer;
import org.openda.utils.OpenDaTestSupport;
import org.springframework.util.Assert;

import java.io.File;

public class DHydroConfigFileTest extends TestCase
{
	private File testRunDataDir;
	private OpenDaTestSupport testData;
	private String dHydroConfigFileNameOriginal = "d_hydro_config.xml";
	private String dHydroConfigFileNameGenerated = "d_hydro_config_generated.xml";

	protected void setUp() {
		testData = new OpenDaTestSupport(DHydroConfigFileTest.class, "public", "model_dflowfm_blackbox");
		testRunDataDir = new File(testData.getTestRunDataDir(), "DHydroFile");
	}

	public void testMd1dFileUpdatesCategoriesCorrectly()
	{
		// Step 1: Read original test file
		IDataObject dHydroConfigFile = new DHydroConfigFile();
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
		Assert.isTrue(FileComparer.CompareXmlFiles(new File(testRunDataDir, "d_hydro_config_expected.xml"),
				new File(testRunDataDir, dHydroConfigFileNameGenerated)));
	}

	public void testMd1dFileGeneratesExpectedFile()
	{
		// Step 1: Read original test file
		IDataObject dHydroConfigFile = new DHydroConfigFile();
		dHydroConfigFile.initialize(testRunDataDir, new String[]{dHydroConfigFileNameOriginal, dHydroConfigFileNameGenerated});

		//Step 2: Write test file
		dHydroConfigFile.finish();

		// Step 3: Compare written file to expected results
		Assert.isTrue(FileComparer.CompareXmlFiles(new File(testRunDataDir, dHydroConfigFileNameOriginal),
				new File(testRunDataDir, dHydroConfigFileNameGenerated)));
	}
}