package org.openda.model_dflowfm;

import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;

public class DFlowFMNetcdfSampleFileTest extends TestCase {
	OpenDaTestSupport testData = null;
	private File testRunDataRestartFileDir;


	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(DFlowFMRestartTest.class, "model_dflowfm_blackbox");
		testRunDataRestartFileDir = new File(testData.getTestRunDataDir(), "DFlowFMNetCDFSample");
	}

	public void testTimeConstant() {
		DFlowFMNetcdfSampleFile dataObject = new DFlowFMNetcdfSampleFile();
		dataObject.initialize(testRunDataRestartFileDir, new String[]{"ExampleTimeIndependent.nc", "idPrefix=prefix", "netcdfVariable=phase", "netcdfVariable=amplitude", "dataFormat=TimeIndependent"});
		String[] exchangeItemIDs = dataObject.getExchangeItemIDs();
		assertEquals(134, exchangeItemIDs.length);
		for (int i = 0; i < exchangeItemIDs.length; i++) {
			IExchangeItem ei = dataObject.getDataObjectExchangeItem(exchangeItemIDs[i]);
			double[] values = ei.getValuesAsDoubles();
			Arrays.fill(values, i);
			ei.setValuesAsDoubles(values);
		}
		dataObject.finish();
	}
}
