package org.openda.model_dflowfm;


import java.io.File;
import java.io.IOException;

import junit.framework.TestCase;

import org.openda.utils.OpenDaTestSupport;

public class DFlowFMPliInputFileTest extends TestCase{

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(DFlowFMPliInputFile.class,"model_dflowfm_blackbox");
		testRunDataDir = new File(testData.getTestRunDataDir(),"Timeseries");
	}

	public void testDflowfmParsePliInputFile() {
		String arg[] = new String[1];
		arg[0] = "estuary_01.pli";
		DFlowFMPliInputFile pliFile = new DFlowFMPliInputFile(testRunDataDir , arg );
		assertEquals(pliFile.getLocationsCount(),2);
	}
}