package org.openda.model_wanda_seawat;

import junit.framework.TestCase;
import org.openda.exchange.DoublesExchangeItem;
import org.openda.interfaces.ITimeInfo;
import org.openda.utils.FileComparer;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class WandaSeawatGridDataObjectTest extends TestCase {
	OpenDaTestSupport testData = null;
	private File testRunDataDir;

	protected void setUp() {
		testData = new OpenDaTestSupport(WandaSeawatGridDataObjectTest.class, "model_wanda_seawat");
		testRunDataDir = new File(testData.getTestRunDataDir(), "WandaSeawatGridDataObject");
	}

	public void testRealCase() {
		WandaSeawatGridDataObject wandaSeawatGridDataObject = new WandaSeawatGridDataObject();
		wandaSeawatGridDataObject.initialize(testRunDataDir, new String[]{"HTO_001", "HTO_TEMP_"});
		assertEquals("Exchange items all loaded", 1, wandaSeawatGridDataObject.getExchangeItemIDs().length);

		Object dataExchangeItemObject = wandaSeawatGridDataObject.getDataObjectExchangeItem("HTO_TEMP_Grid");
		assertTrue(dataExchangeItemObject instanceof DoublesExchangeItem);
		DoublesExchangeItem dataExchangeItem = (DoublesExchangeItem) dataExchangeItemObject;
		double[] values = dataExchangeItem.getValuesAsDoubles();
		assertEquals("Data for 30 rows and 72 columns", 3654, values.length);

		ITimeInfo timeInfo = dataExchangeItem.getTimeInfo();
		assertNotNull(timeInfo);
		double[] times = timeInfo.getTimes();
		assertEquals(1, times.length);
		assertEquals(60766, times[0], 0.0000001d);

		double[] axpyValues = new double[values.length];
		Arrays.fill(axpyValues, 0.5);
		dataExchangeItem.axpyOnValues(1, axpyValues);

		wandaSeawatGridDataObject.finish();

		File output = new File(testRunDataDir, "HTO_001/HTO_TEMP_20250401000000.ASC");
		File expected = new File(testRunDataDir, "HTO_001_expected/HTO_TEMP_20250401000000.ASC");
		assertTrue(testData.FilesAreIdentical(output, expected));
	}
}
