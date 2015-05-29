package org.openda.model_dflowfm;


import java.io.File;
import java.io.IOException;

import junit.framework.TestCase;
import org.openda.utils.OpenDaTestSupport;

public class DFlowFMExtInputFileTest extends TestCase{

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(DFlowFMExtInputFile.class,"model_dflowfm_blackbox");
		testRunDataDir = new File(testData.getTestRunDataDir(),"Forcingfile");
	}

	public void testDflowfmParseExtInputFile() {

		File dataDir=testRunDataDir;
		File testfile=new File(dataDir,"simple_waal.ext");
		DFlowFMExtInputFile extOptions = new DFlowFMExtInputFile(testfile);
		String[] ref_quantities = new String[4];
		ref_quantities[0] = "dischargebnd";
		ref_quantities[1] = "waterlevelbnd";
		ref_quantities[2] = "frictioncoefficient";
		ref_quantities[3] = "frictioncoefficient";
		for (int i=0; i < extOptions.count(); i++ ) {
			assertEquals(extOptions.get("QUANTITY", i), ref_quantities[i]);
		}
	}
}