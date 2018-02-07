package org.openda.model_metaswap;

import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;

public class SwapResultFileTest extends TestCase {
	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() {
		testData = new OpenDaTestSupport(SwapResultFileTest.class, "model_metaswap");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public static void testDummy() {
		// No action. Test only exists to avoid warnings on empty test class
	}

	public void tstRead() {
		SwapResultFile swapResultFile = new SwapResultFile();
		swapResultFile.initialize(testRunDataDir, new String[]{"svat_dtgw_0000207106.csv"});
		String[] exchangeItemIDs = swapResultFile.getExchangeItemIDs();
		assertNotNull(exchangeItemIDs);
		assertEquals(1, exchangeItemIDs.length);
		String exchangeItemID = exchangeItemIDs[0];
		assertEquals("0000207106.soilmoisture", exchangeItemID);
		IExchangeItem exchangeItem = swapResultFile.getDataObjectExchangeItem(exchangeItemID);
		assertNotNull(exchangeItem);
		assertTrue(exchangeItem instanceof SwapResultExchangeItem);
		double[] values = exchangeItem.getValuesAsDoubles();
		double[] times = exchangeItem.getTimes();
		assertEquals(125, values.length);
		assertEquals(125, times.length);
		assertEquals(124.0,times[times.length - 1] - times[0]);

		assertEquals(329.8, values[0], 0.00001);
		assertEquals(57143.0, times[0], 0.00001);

		assertEquals(273.075, values[values.length - 1], 0.00001);
	}
}
