package org.openda.model_wanda_seawat;

import junit.framework.TestCase;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.IExchangeItem;
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
		// TODO: write start date time end date time according to dateformat
		// TODO prevent new line wandaModel = .....
		File file = new File(testRunDataDir, "example.ini");
		WandaSeawatModelRunIniFileDataObject wandaSeawatModelRunIniFileDataObject = new WandaSeawatModelRunIniFileDataObject();
		wandaSeawatModelRunIniFileDataObject.initialize(testRunDataDir, new String[]{file.getName()});
		assertEquals("The 6 exchange items exist", 6, wandaSeawatModelRunIniFileDataObject.getExchangeItemIDs().length);

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

		IExchangeItem timeSeriesEI = wandaSeawatModelRunIniFileDataObject.getDataObjectExchangeItem("timeseries");
		assertNotNull(timeSeriesEI);
		assertTrue(timeSeriesEI instanceof TimeSeries);
		double[] times = timeSeriesEI.getTimes();
		assertEquals(49, times.length);
		assertEquals(60310, times[0], 0.0001);
		assertEquals(60310.04166666667, times[1], 0.0001);
		assertEquals(60311.95833333333, times[47], 0.0001);
		assertEquals(60312, times[48], 0.0001);
		double[] values = timeSeriesEI.getValuesAsDoubles();
		assertEquals(49, values.length);
		assertEquals(0.752464259, values[0], 0.0001);
		assertEquals(0.747373312, values[1], 0.0001);
		assertEquals(0.865188971, values[47], 0.0001);
		assertEquals(0.356379411, values[48], 0.0001);

		values[0] = 1;
		values[1] = 1.1;
		values[47] = 1.47;
		values[48] = 1.48;
		timeSeriesEI.setValuesAsDoubles(values);

		porosityExchangeItem.setValue(5.4);
		wandaSeawatModelRunIniFileDataObject.finish();

		try (Stream<String> stream = Files.lines(file.toPath())) {
			String lines = stream.collect(Collectors.joining("\n"));
			assertTrue("Updated value present in ini file", lines.contains("HTGEOFIL H1.Geoformations.Porosity formation.kleilaag 1 = 5.4"));
		}

		WandaSeawatModelRunIniFileDataObject wandaSeawatModelRunIniFileDataObject2 = new WandaSeawatModelRunIniFileDataObject();
		wandaSeawatModelRunIniFileDataObject2.initialize(testRunDataDir, new String[]{file.getName()});

		IExchangeItem timeSeriesEIRewritten = wandaSeawatModelRunIniFileDataObject.getDataObjectExchangeItem("timeseries");
		assertNotNull(timeSeriesEIRewritten);
		assertTrue(timeSeriesEIRewritten instanceof TimeSeries);
		double[] timesRewritten = timeSeriesEIRewritten.getTimes();
		assertEquals(49, timesRewritten.length);
		assertEquals(60310, timesRewritten[0], 0.0001);
		assertEquals(60310.04166666667, timesRewritten[1], 0.0001);
		assertEquals(60311.95833333333, timesRewritten[47], 0.0001);
		assertEquals(60312, timesRewritten[48], 0.0001);
		double[] valuesRewritten = timeSeriesEIRewritten.getValuesAsDoubles();
		assertEquals(49, valuesRewritten.length);
		assertEquals(1.0, valuesRewritten[0], 0.0001);
		assertEquals(1.1, valuesRewritten[1], 0.0001);
		assertEquals(1.47, valuesRewritten[47], 0.0001);
		assertEquals(1.48, valuesRewritten[48], 0.0001);
	}
}
