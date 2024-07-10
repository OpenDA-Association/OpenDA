package org.openda.model_wanda_seawat;

import junit.framework.TestCase;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.IntExchangeItem;
import org.openda.exchange.StringExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;

public class WandaSeawatModelRunIniFileDataObjectTest extends TestCase {
	OpenDaTestSupport testData = null;
	private File testRunDataDir;

	protected void setUp() {
		testData = new OpenDaTestSupport(WandaSeawatModelRunIniFileDataObjectTest.class, "model_wanda_seawat");
		testRunDataDir = new File(testData.getTestRunDataDir(), "WandaSeawatModelRunIniFileDataObject");
	}

	public void test_givenWandaSeawatIniFile_whenInitialized_thenExchangeItemsLoaded() {
		File file = new File(testRunDataDir, "example.ini");
		WandaSeawatModelRunIniFileDataObject wandaSeawatModelRunIniFileDataObject = new WandaSeawatModelRunIniFileDataObject();
		wandaSeawatModelRunIniFileDataObject.initialize(testRunDataDir, new String[]{file.getName()});
		assertEquals("The 5 exchange items exist", 5, wandaSeawatModelRunIniFileDataObject.getExchangeItemIDs().length);

		Object startDateTimeExchangeItemObject = wandaSeawatModelRunIniFileDataObject.getDataObjectExchangeItem("startDateTime");
		assertTrue(startDateTimeExchangeItemObject instanceof StringExchangeItem);
		StringExchangeItem startDateTimeExchangeItem = (StringExchangeItem) startDateTimeExchangeItemObject;
		assertEquals("The start date time is 20240101000000", "20240101000000", startDateTimeExchangeItem.getValue());

		Object endDateTimeExchangeItemObject = wandaSeawatModelRunIniFileDataObject.getDataObjectExchangeItem("endDateTime");
		assertTrue(endDateTimeExchangeItemObject instanceof StringExchangeItem);
		StringExchangeItem endDateTimeExchangeItem = (StringExchangeItem) endDateTimeExchangeItemObject;
		assertEquals("The end date time is 20240102000000", "20240102000000", endDateTimeExchangeItem.getValue());

		Object bulkDensityExchangeItemObject = wandaSeawatModelRunIniFileDataObject.getDataObjectExchangeItem("h1GeoformationsBulkDensity");
		assertTrue(bulkDensityExchangeItemObject instanceof DoubleExchangeItem);
		DoubleExchangeItem bulkDensityExchangeItem = (DoubleExchangeItem) bulkDensityExchangeItemObject;
		assertEquals("The bulk density is 0.998754", 0.998754, bulkDensityExchangeItem.getValue());

		Object porosityExchangeItemObject = wandaSeawatModelRunIniFileDataObject.getDataObjectExchangeItem("h1GeoformationsPorosity");
		assertTrue(porosityExchangeItemObject instanceof DoubleExchangeItem);
		DoubleExchangeItem porosityExchangeItem = (DoubleExchangeItem) porosityExchangeItemObject;
		assertEquals("The bulk density is 0.6", 0.6, porosityExchangeItem.getValue());

		Object maxYearlyWaterVolumeExchangeItemObject = wandaSeawatModelRunIniFileDataObject.getDataObjectExchangeItem("h1MaxYearlyWaterVolume");
		assertTrue(maxYearlyWaterVolumeExchangeItemObject instanceof IntExchangeItem);
		IntExchangeItem maxYearlyWaterVolumeExchangeItem = (IntExchangeItem) maxYearlyWaterVolumeExchangeItemObject;
		assertEquals("The bulk density is 65484", 65484, maxYearlyWaterVolumeExchangeItem.getValue());
	}
}
