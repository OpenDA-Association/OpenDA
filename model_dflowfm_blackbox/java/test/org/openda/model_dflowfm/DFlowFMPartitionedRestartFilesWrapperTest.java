package org.openda.model_dflowfm;

import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;

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
		IExchangeItem s1_0001 = dFlowFMRestartFileWrapper.getDataObjectExchangeItem("s1_0001");
		assertNotNull(s1_0001);

		double[] originalValuesS1_0001 = s1_0001.getValuesAsDoubles();
		double[] axpyValues = new double[originalValuesS1_0001.length];
		Arrays.fill(axpyValues, 1.1);
		s1_0001.axpyOnValues(1.0, axpyValues);

		IExchangeItem s1_0000 = dFlowFMRestartFileWrapper.getDataObjectExchangeItem("s1_0000");
		assertNotNull(s1_0000);
		double[] originalValuesS1_0000 = s1_0000.getValuesAsDoubles();

		IExchangeItem s1_0002 = dFlowFMRestartFileWrapper.getDataObjectExchangeItem("s1_0002");
		assertNotNull(s1_0002);
		double[] originalValuesS1_0002 = s1_0002.getValuesAsDoubles();

		dFlowFMRestartFileWrapper.finish();

		DFlowFMPartitionedRestartFilesWrapper dFlowFMRestartFileWrapper2 = new DFlowFMPartitionedRestartFilesWrapper();
		dFlowFMRestartFileWrapper2.initialize(testRunDataRestartFileDir, new String[]{"runId=dcsmv5", "numberOfPartitions=3"});
		String[] exchangeItemIDs2 = dFlowFMRestartFileWrapper2.getExchangeItemIDs();
		assertEquals(42, exchangeItemIDs2.length);
		IExchangeItem s1_0001_2 = dFlowFMRestartFileWrapper2.getDataObjectExchangeItem("s1_0001");
		assertNotNull(s1_0001_2);
		double[] valuesAsDoublesS1_0001_2 = s1_0001_2.getValuesAsDoubles();
		assertEquals(originalValuesS1_0001.length, valuesAsDoublesS1_0001_2.length);
		for (int i = 0; i < valuesAsDoublesS1_0001_2.length; i++) {
			assertEquals(axpyValues[i] + originalValuesS1_0001[i], valuesAsDoublesS1_0001_2[i], 0.00001);
		}

		IExchangeItem s1_0000_2 = dFlowFMRestartFileWrapper.getDataObjectExchangeItem("s1_0000");
		assertNotNull(s1_0000_2);
		double[] rewrittenValuesS1_0000 = s1_0000_2.getValuesAsDoubles();
		assertEquals(originalValuesS1_0000.length, rewrittenValuesS1_0000.length);
		for (int i = 0; i < originalValuesS1_0000.length; i++) {
			assertEquals(originalValuesS1_0000[i], rewrittenValuesS1_0000[i], 0.00001);
		}

		IExchangeItem s1_0002_2 = dFlowFMRestartFileWrapper.getDataObjectExchangeItem("s1_0002");
		assertNotNull(s1_0002_2);
		double[] rewrittenValuesS1_0002 = s1_0002_2.getValuesAsDoubles();
		assertEquals(originalValuesS1_0002.length, rewrittenValuesS1_0002.length);
		for (int i = 0; i < originalValuesS1_0002.length; i++) {
			assertEquals(originalValuesS1_0002[i], rewrittenValuesS1_0002[i], 0.00001);
		}
	}

}
