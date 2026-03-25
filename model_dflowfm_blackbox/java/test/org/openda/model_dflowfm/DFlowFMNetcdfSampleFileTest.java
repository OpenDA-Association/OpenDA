package org.openda.model_dflowfm;

import junit.framework.TestCase;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

public class DFlowFMNetcdfSampleFileTest extends TestCase {
	OpenDaTestSupport testData = null;
	private File testRunDataRestartFileDir;


	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(DFlowFMRestartTest.class, "model_dflowfm_blackbox");
		testRunDataRestartFileDir = new File(testData.getTestRunDataDir(), "DFlowFMNetCDFSample");
	}

	public void testTimeConstant() {
		DFlowFMNetcdfSampleFile dataObject = new DFlowFMNetcdfSampleFile();
		dataObject.initialize(testRunDataRestartFileDir, new String[]{"ExampleTimeIndependent.nc", "idPrefix=prefix", "netcdfVariable=phase", "dataFormat=TimeIndependent"});
		String[] exchangeItemIDs = dataObject.getExchangeItemIDs();
		assertEquals(67, exchangeItemIDs.length);
	}
}
