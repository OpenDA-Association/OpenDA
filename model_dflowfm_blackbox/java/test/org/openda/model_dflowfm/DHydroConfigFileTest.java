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
	private String dHydroConfigFileNameOriginal = "d_hydro_config.xml";
	private String dHydroConfigFileNameGenerated = "d_hydro_config_generated.xml";

	protected void setUp() {
		OpenDaTestSupport testData = new OpenDaTestSupport(DHydroConfigFileTest.class, "public", "model_dflowfm_blackbox");
		testRunDataDir = new File(testData.getTestRunDataDir(), "DHydroFile");
	}

	public void testDHydroConfigFileUpdatesCategoriesCorrectly()
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
		String flowMd1dFileNameGenerated = "dflow1d/flow-model-1d.md1d";
		String flowMd1dFileNameExpected = "dflow1d/flow-model-1d_expected.md1d";
		String rtcRuntimeConfigNameGenerated = "rtc/rtcRuntimeConfig.xml";
		String rtcRuntimeConfigNameExpected = "rtc/rtcRuntimeConfig_expected.xml";

		Assert.isTrue(FileComparer.CompareXmlFiles(new File(testRunDataDir, "d_hydro_config_expected.xml"),
				new File(testRunDataDir, dHydroConfigFileNameGenerated)), "Compare dhydro-config file");
		Assert.isTrue(FileComparer.CompareIniFiles(new File(testRunDataDir, flowMd1dFileNameExpected),
				new File(testRunDataDir, flowMd1dFileNameGenerated)), "Compare flow1d md1d file");
		Assert.isTrue(FileComparer.CompareXmlFiles(new File(testRunDataDir, rtcRuntimeConfigNameExpected),
				new File(testRunDataDir, rtcRuntimeConfigNameGenerated)), "Compare rtc run time config file");
	}

	public void testDHydroConfigFileGeneratesExpectedFile()
	{
		// Step 1: Read original test file
		IDataObject dHydroConfigFile = new DHydroConfigFile();
		dHydroConfigFile.initialize(testRunDataDir, new String[]{dHydroConfigFileNameOriginal, dHydroConfigFileNameGenerated});

		//Step 2: Write test file
		dHydroConfigFile.finish();

		// Step 3: Compare written file to expected results
		Assert.isTrue(FileComparer.CompareXmlFiles(new File(testRunDataDir, dHydroConfigFileNameOriginal),
				new File(testRunDataDir, dHydroConfigFileNameGenerated)), "Compare resulting dhydro-config file");
	}
}