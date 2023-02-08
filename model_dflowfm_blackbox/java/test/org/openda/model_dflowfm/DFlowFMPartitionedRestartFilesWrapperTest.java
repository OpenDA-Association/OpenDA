package org.openda.model_dflowfm;

import junit.framework.TestCase;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

public class DFlowFMPartitionedRestartFilesWrapperTest extends TestCase {
	OpenDaTestSupport testData = null;
	private File testRunDataRestartFileDir;


	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(DFlowFMPartitionedRestartFilesWrapperTest.class, "model_dflowfm_blackbox");
		testRunDataRestartFileDir = new File(testData.getTestRunDataDir(), "DFlowFMPartionedRestartFilesWrapper");
	}


	public void testRenameNewestRestartFile() {
		DFlowFMPartitionedRestartFilesWrapper dFlowFMRestartFileWrapper = new DFlowFMPartitionedRestartFilesWrapper();
		dFlowFMRestartFileWrapper.initialize(testRunDataRestartFileDir, new String[]{"runId=dcsmv5", "numberOfPartitions=3"});
		String[] exchangeItemIDs = dFlowFMRestartFileWrapper.getExchangeItemIDs();
		assertEquals(42, exchangeItemIDs.length);
		dFlowFMRestartFileWrapper.finish();
	}

}
