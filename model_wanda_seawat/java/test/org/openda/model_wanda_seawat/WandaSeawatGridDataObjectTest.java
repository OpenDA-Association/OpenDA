package org.openda.model_wanda_seawat;

import junit.framework.TestCase;
import org.openda.exchange.DoublesExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;

public class WandaSeawatGridDataObjectTest extends TestCase {
	OpenDaTestSupport testData = null;
	private File testRunDataDir;

	protected void setUp() {
		testData = new OpenDaTestSupport(WandaSeawatGridDataObjectTest.class, "model_wanda_seawat");
		testRunDataDir = new File(testData.getTestRunDataDir(), "WandaSeawatGridDataObject");
	}

	public void test_givenWandaSeawatData_whenInitialized_thenExchangeItemsLoaded() {
		File file = new File(testRunDataDir, "HTO_TEMP_20220531000100.ASC");
		WandaSeawatGridDataObject wandaSeawatGridDataObject = new WandaSeawatGridDataObject();
		wandaSeawatGridDataObject.initialize(testRunDataDir, new String[]{file.getName()});
		assertEquals("Exchange items all loaded", 3, wandaSeawatGridDataObject.getExchangeItemIDs().length);

		Object radiiExchangeItemObject = wandaSeawatGridDataObject.getDataObjectExchangeItem("radii");
		assertTrue(radiiExchangeItemObject instanceof DoublesExchangeItem);
		DoublesExchangeItem radiiExchangeItem = (DoublesExchangeItem) radiiExchangeItemObject;
		double[] radii = radiiExchangeItem.getValuesAsDoubles();
		assertEquals("72 radii exist", 72, radii.length);
		assertEquals("The 2nd radius is 0.7624999881", 0.7624999881, radii[1]);

		Object heightsExchangeItemObject = wandaSeawatGridDataObject.getDataObjectExchangeItem("heights");
		assertTrue(heightsExchangeItemObject instanceof DoublesExchangeItem);
		DoublesExchangeItem heightsExchangeItem = (DoublesExchangeItem) heightsExchangeItemObject;
		double[] heights = heightsExchangeItem.getValuesAsDoubles();
		assertEquals("30 heights exist", 30, heights.length);
		assertEquals("The 20th height is -298.0", -298.0, heights[19]);

		Object dataExchangeItemObject = wandaSeawatGridDataObject.getDataObjectExchangeItem("data");
		assertTrue(dataExchangeItemObject instanceof DoublesExchangeItem);
		DoublesExchangeItem dataExchangeItem = (DoublesExchangeItem) dataExchangeItemObject;
		double[] values = dataExchangeItem.getValuesAsDoubles();
		assertEquals("Get value from 20th row of 30 and 2nd column of 72", 0.10554802E+02, values[19 * 72 + 1]);
	}
}
