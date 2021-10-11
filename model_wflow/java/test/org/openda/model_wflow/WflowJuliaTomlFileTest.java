package org.openda.model_wflow;

import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;

public class WflowJuliaTomlFileTest extends TestCase {
	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() {
		testData = new OpenDaTestSupport(WflowJuliaNetcdfForcingFileTest.class, "model_wflow");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testReadAndModifyStartEndTimeNetcdfPeriod() {
		WflowJuliaTomlFile wflowJuliaNetcdfForcingFile = new WflowJuliaTomlFile();
		File workingDir = new File(this.testRunDataDir, "tomlfile");
		wflowJuliaNetcdfForcingFile.initialize(workingDir, new String[]{"sbm_config.toml", "inmaps_ncgen.nc"});

		IExchangeItem startTimeEI = wflowJuliaNetcdfForcingFile.getDataObjectExchangeItem("startTime");
		assertNotNull(startTimeEI);
		IExchangeItem endTimeEI = wflowJuliaNetcdfForcingFile.getDataObjectExchangeItem("endTime");
		assertNotNull(endTimeEI);

		double startTime = startTimeEI.getValuesAsDoubles()[0];
		assertEquals(52076.0, startTime);

		double endTime = endTimeEI.getValuesAsDoubles()[0];
		assertEquals(52091.0, endTime);

		double startTimeEdit = startTime + 2;
		startTimeEI.setValuesAsDoubles(new double[]{startTimeEdit});
		double endTimeEdit = startTime + 4;
		endTimeEI.setValuesAsDoubles(new double[]{endTimeEdit});

		wflowJuliaNetcdfForcingFile.finish();

		File tomlFile = new File(workingDir, "sbm_config.toml");
		File expectedTomlFile = new File(workingDir, "expected_sbm_config_netcdf_period.toml");
		assertTrue(tomlFile.exists());
		assertTrue(expectedTomlFile.exists());

		assertTrue(testData.FilesAreIdentical(tomlFile, expectedTomlFile));
	}

	public void testReadAndModifyStartEndTimeTomlPeriod() {
		WflowJuliaTomlFile wflowJuliaNetcdfForcingFile = new WflowJuliaTomlFile();
		File workingDir = new File(this.testRunDataDir, "tomlfile");
		wflowJuliaNetcdfForcingFile.initialize(workingDir, new String[]{"sbm_config.toml"});

		IExchangeItem startTimeEI = wflowJuliaNetcdfForcingFile.getDataObjectExchangeItem("startTime");
		assertNotNull(startTimeEI);
		IExchangeItem endTimeEI = wflowJuliaNetcdfForcingFile.getDataObjectExchangeItem("endTime");
		assertNotNull(endTimeEI);

		double startTime = startTimeEI.getValuesAsDoubles()[0];
		assertEquals(47527.0, startTime);

		double endTime = endTimeEI.getValuesAsDoubles()[0];
		assertEquals(58848.0, endTime);

		double startTimeEdit = startTime + 2;
		startTimeEI.setValuesAsDoubles(new double[]{startTimeEdit});
		double endTimeEdit = startTime + 4;
		endTimeEI.setValuesAsDoubles(new double[]{endTimeEdit});

		wflowJuliaNetcdfForcingFile.finish();

		File tomlFile = new File(workingDir, "sbm_config.toml");
		File expectedTomlFile = new File(workingDir, "expected_sbm_config.toml");
		assertTrue(tomlFile.exists());
		assertTrue(expectedTomlFile.exists());

		assertTrue(testData.FilesAreIdentical(tomlFile, expectedTomlFile));
	}

}
