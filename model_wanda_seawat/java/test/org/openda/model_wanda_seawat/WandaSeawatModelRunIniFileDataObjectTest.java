package org.openda.model_wanda_seawat;

import junit.framework.TestCase;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class WandaSeawatModelRunIniFileDataObjectTest extends TestCase {
	OpenDaTestSupport testData = null;
	private File testRunDataDir;

	protected void setUp() {
		testData = new OpenDaTestSupport(WandaSeawatModelRunIniFileDataObjectTest.class, "model_wanda_seawat");
		testRunDataDir = new File(testData.getTestRunDataDir(), "WandaSeawatModelRunIniFileDataObject");
	}

	public void test_givenWandaSeawatIniFile_whenInitialized_thenExchangeItemsLoaded() throws IOException {
		File file = new File(testRunDataDir, "example.ini");
		WandaSeawatModelRunIniFileDataObject wandaSeawatModelRunIniFileDataObject = new WandaSeawatModelRunIniFileDataObject();
		wandaSeawatModelRunIniFileDataObject.initialize(testRunDataDir, new String[]{file.getName()});
		assertEquals("The 5 exchange items exist", 5, wandaSeawatModelRunIniFileDataObject.getExchangeItemIDs().length);

		Object startDateTimeExchangeItemObject = wandaSeawatModelRunIniFileDataObject.getDataObjectExchangeItem("startDateTime");
		assertTrue(startDateTimeExchangeItemObject instanceof DoubleExchangeItem);
		DoubleExchangeItem startDateTimeExchangeItem = (DoubleExchangeItem) startDateTimeExchangeItemObject;
		assertEquals("The start date time (20240101000000) is 60310.0", 60310.0, startDateTimeExchangeItem.getValue());

		Object endDateTimeExchangeItemObject = wandaSeawatModelRunIniFileDataObject.getDataObjectExchangeItem("endDateTime");
		assertTrue(endDateTimeExchangeItemObject instanceof DoubleExchangeItem);
		DoubleExchangeItem endDateTimeExchangeItem = (DoubleExchangeItem) endDateTimeExchangeItemObject;
		assertEquals("The end date time (20240102000000) is 60311.0", 60311.0, endDateTimeExchangeItem.getValue());

		Object bulkDensityExchangeItemObject = wandaSeawatModelRunIniFileDataObject.getDataObjectExchangeItem("HTGEOFIL H1.Geoformations.Bulk density formation.kleilaag 1");
		assertTrue(bulkDensityExchangeItemObject instanceof DoubleExchangeItem);
		DoubleExchangeItem bulkDensityExchangeItem = (DoubleExchangeItem) bulkDensityExchangeItemObject;
		assertEquals("The bulk density is 0.998754", 0.998754, bulkDensityExchangeItem.getValue());

		Object porosityExchangeItemObject = wandaSeawatModelRunIniFileDataObject.getDataObjectExchangeItem("HTGEOFIL H1.Geoformations.Porosity formation.kleilaag 1");
		assertTrue(porosityExchangeItemObject instanceof DoubleExchangeItem);
		DoubleExchangeItem porosityExchangeItem = (DoubleExchangeItem) porosityExchangeItemObject;
		assertEquals("The porosity is 0.6", 0.6, porosityExchangeItem.getValue());

		Object maxYearlyWaterVolumeExchangeItemObject = wandaSeawatModelRunIniFileDataObject.getDataObjectExchangeItem("HTGEOFIL H1.Max yearly water volume");
		assertTrue(maxYearlyWaterVolumeExchangeItemObject instanceof DoubleExchangeItem);
		DoubleExchangeItem maxYearlyWaterVolumeExchangeItem = (DoubleExchangeItem) maxYearlyWaterVolumeExchangeItemObject;
		assertEquals("The max yearly water volume is 65484", 65484.0, maxYearlyWaterVolumeExchangeItem.getValue());
		
		porosityExchangeItem.setValue(5.4);
		wandaSeawatModelRunIniFileDataObject.finish();

		try (Stream<String> stream = Files.lines(file.toPath())) {
			String lines = stream.collect(Collectors.joining("\n"));
			assertTrue("Updated value present in ini file", lines.contains("HTGEOFIL H1.Geoformations.Porosity formation.kleilaag 1 = 5.4"));
		}
	}
}
