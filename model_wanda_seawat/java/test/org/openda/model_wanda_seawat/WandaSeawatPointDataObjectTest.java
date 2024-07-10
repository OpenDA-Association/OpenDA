package org.openda.model_wanda_seawat;

import junit.framework.TestCase;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;

public class WandaSeawatPointDataObjectTest extends TestCase {
	OpenDaTestSupport testData = null;
	private File testRunDataDir;

	protected void setUp() {
		testData = new OpenDaTestSupport(WandaSeawatPointDataObjectTest.class, "model_wanda_seawat");
		testRunDataDir = new File(testData.getTestRunDataDir(), "WandaSeawatPointDataObject");
	}

	public void test_givenWandaSeawatData_whenInitialized_thenExchangeItemsLoaded() {
		File file = new File(testRunDataDir, "HTO_TEMP_20220531000100.ASC");
		WandaSeawatPointDataObject wandaSeawatPointDataObject = new WandaSeawatPointDataObject();
		wandaSeawatPointDataObject.initialize(testRunDataDir, new String[]{file.getName()});
		assertEquals("Exchange items all loaded", 2160, wandaSeawatPointDataObject.getExchangeItemIDs().length);
		String key = wandaSeawatPointDataObject.getExchangeItemIDs()[19 * 72 + 1];
		assertEquals("Get key from 20th row of 30 and 2nd column of 72", "depth-298.0_radius0.7624999881", key);
		Object exchangeItemObject = wandaSeawatPointDataObject.getDataObjectExchangeItem(key);
		assertTrue(exchangeItemObject instanceof DoubleExchangeItem);
		DoubleExchangeItem doubleExchangeItem = (DoubleExchangeItem) exchangeItemObject;
		assertEquals("Get value from 20th row of 30 and 2nd column of 72", 0.10554802E+02, doubleExchangeItem.getValue());
	}
}
