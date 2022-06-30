package org.openda.model_dflowfm;

import junit.framework.TestCase;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

public class DFlowFMRestartFilePostProcessorTest extends TestCase {
	OpenDaTestSupport testData = null;
	private File testRunDataRestartFileDir;


	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(DFlowFMRestartTest.class, "model_dflowfm_blackbox");
		testRunDataRestartFileDir = new File(testData.getTestRunDataDir(), "RestartFilePostProcessor");
	}


	public void testRestartFileInsteadOfMap() {
		File restartFile4 = new File(testRunDataRestartFileDir, "notARealRestartFile_20070104_000000_rst.nc");
		assertTrue(restartFile4.exists());
		File restartFile5 = new File(testRunDataRestartFileDir, "notARealRestartFile_20070105_000000_rst.nc");
		assertTrue(restartFile5.exists());
		DFlowFMRestartFilePostProcessor dFlowFMRestartFilePostProcessor = new DFlowFMRestartFilePostProcessor();
		dFlowFMRestartFilePostProcessor.initialize(testRunDataRestartFileDir, new String[]{"runId=notARealRestartFile", "targetRestartFileName=notARealRestartFile_20220101_000000_rst.nc"});
		assertTrue(restartFile4.exists());
		assertFalse(restartFile5.exists());
		File restartFile2022 = new File(testRunDataRestartFileDir, "notARealRestartFile_20220101_000000_rst.nc");
		assertTrue(restartFile2022.exists());
	}
}
