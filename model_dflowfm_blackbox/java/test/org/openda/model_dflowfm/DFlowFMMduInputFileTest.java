package org.openda.model_dflowfm;


import java.io.File;
import java.io.IOException;

import junit.framework.TestCase;

import org.openda.utils.OpenDaTestSupport;

public class DFlowFMMduInputFileTest extends TestCase{

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(DFlowFMMduInputFile.class,"model_dflowfm_blackbox");
		testRunDataDir = new File(testData.getTestRunDataDir(),"Timeseries");
	}

	public void testDflowfmParseExtInputFile() {

		File dataDir=testRunDataDir;

		DFlowFMMduInputFile mduOptions = new DFlowFMMduInputFile(dataDir, "estuary.mdu");
		
		String forcingFilePath = mduOptions.get("external forcing","ExtForceFile");
		assertEquals( "estuary.ext" , forcingFilePath);
		
		Double refDate = mduOptions.getReferenceDateInMjd();
		assertEquals( "48257.0", refDate.toString() );
		
		Double factor = mduOptions.getTimeToMjdFactor();
		assertEquals( 24.0 * 60.0, 1.0/factor);
	}
}