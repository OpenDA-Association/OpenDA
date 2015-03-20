package org.openda.model_dflowfm;

import java.io.File;
import java.io.IOException;

import junit.framework.TestCase;

import org.openda.utils.OpenDaTestSupport;

public class DFlowFMCmpInputFileTest extends TestCase{

	private File testRunDataDir;
	OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(DFlowFMCmpInputFile.class,"model_dflowfm_blackbox");
		testRunDataDir = new File(testData.getTestRunDataDir(),"Timeseries");
	}

	public void testDflowfmParseAstrocompontentFile() {
		DFlowFMCmpInputFile cmpFile = new DFlowFMCmpInputFile(testRunDataDir , "astro.cmp");
		String[] AC = cmpFile.getACname();
		for (String var : AC) {
			if (var.contentEquals("M2")) {
				assertEquals(cmpFile.getAmplitude(var),0.6);
				assertEquals(cmpFile.getPhase(var),0.0);
			}
			if (var.contentEquals("S2")) {
				assertEquals(cmpFile.getAmplitude(var),0.1);
				assertEquals(cmpFile.getPhase(var),0.0);
			}
		}
	}

	public void testDflowfmParsePeriodFile() {
		DFlowFMCmpInputFile cmpFile = new DFlowFMCmpInputFile(testRunDataDir , "period.cmp" );
		String[] AC = cmpFile.getACname();
		for (String var : AC) {
			if (var.contentEquals("Period")) {
				assertEquals(cmpFile.getPeriod(),0.0);
				assertEquals(cmpFile.getAmplitude(var),1.0);
				assertEquals(cmpFile.getPhase(var),0.0);
			}
		}
	}

}