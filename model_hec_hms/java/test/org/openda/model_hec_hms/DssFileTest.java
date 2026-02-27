package org.openda.model_hec_hms;

import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.util.Arrays;

public class DssFileTest extends TestCase {
	OpenDaTestSupport testData = null;
	private File testRunDataDir;


	protected void setUp() {
		testData = new OpenDaTestSupport(DssFileTest.class, "model_hec_hms");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testRead() {
		File dssFile = new File(testRunDataDir, "Hec.dss");
		DssFile dssDataObject = new DssFile();
		dssDataObject.initialize(testRunDataDir, new String[]{dssFile.getName()});
		String[] exchangeItemIDs = dssDataObject.getExchangeItemIDs();
		assertEquals(1, exchangeItemIDs.length);
	}

	public void testReadReal() {
		File dssFile = new File(testRunDataDir, "Run_2.dss");
		DssFile dssDataObject = new DssFile();
		dssDataObject.initialize(testRunDataDir, new String[]{dssFile.getName()});

		String[] exchangeItemIDs = dssDataObject.getExchangeItemIDs();
		Arrays.sort(exchangeItemIDs);
		assertEquals(34, exchangeItemIDs.length);
		IExchangeItem junction1FlowItem = dssDataObject.getDataObjectExchangeItem("/Junction-1/FLOW/RUN:Run 2");
		assertNotNull(junction1FlowItem);
		double[] timesJ1 = junction1FlowItem.getTimeInfo().getTimes();
		assertEquals(1152, timesJ1.length);
		assertEquals(60611.0, timesJ1[0], 0.00001);
		assertEquals(60611.34375, timesJ1[99], 0.00001);
		assertEquals(60614.99652777778, timesJ1[1151], 0.00001);
		
		double[] valuesJ1 = junction1FlowItem.getValuesAsDoubles();
		assertEquals(1152, valuesJ1.length);
		assertEquals(10.5, valuesJ1[0], 0.00001);
		assertEquals(4.08500, valuesJ1[99], 0.00001);
		assertEquals(0.01787, valuesJ1[1151], 0.00001);


		IExchangeItem subBasin1FlowItem = dssDataObject.getDataObjectExchangeItem("/Subbasin-8/ET-POTENTIAL/RUN:Run 2");
		assertNotNull(subBasin1FlowItem);
		double[] timesSB8 = subBasin1FlowItem.getTimeInfo().getTimes();
		assertEquals(1151, timesSB8.length);
		assertEquals(60611.003472, timesSB8[0], 0.00001);
		assertEquals(60611.347222, timesSB8[99], 0.00001);
		assertEquals(60614.99652777778, timesSB8[1150], 0.00001);

		double[] valuesSB8 = subBasin1FlowItem.getValuesAsDoubles();
		assertEquals(1151, valuesSB8.length);
		assertEquals(0.0106597, valuesSB8[0], 0.00001);
		assertEquals(0.0106597, valuesSB8[99], 0.00001);
		assertEquals(0.0106597, valuesSB8[1150], 0.00001);
	}
}
