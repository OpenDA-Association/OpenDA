package org.openda.model_wanda_seawat;

import junit.framework.TestCase;
import org.openda.exchange.DoublesExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class WandaSeawatGridDataObjectTest extends TestCase {
	OpenDaTestSupport testData = null;
	private File testRunDataDir;

	protected void setUp() {
		testData = new OpenDaTestSupport(WandaSeawatGridDataObjectTest.class, "model_wanda_seawat");
		testRunDataDir = new File(testData.getTestRunDataDir(), "WandaSeawatGridDataObject");
	}

	public void test_givenWandaSeawatData_whenInitialized_thenExchangeItemsLoaded() throws IOException {
		File file = new File(testRunDataDir, "HTO_TEMP_");
		WandaSeawatGridDataObject wandaSeawatGridDataObject = new WandaSeawatGridDataObject();
		wandaSeawatGridDataObject.initialize(testRunDataDir, new String[]{file.getName()});
		assertEquals("Exchange items all loaded", 1, wandaSeawatGridDataObject.getExchangeItemIDs().length);

		Object dataExchangeItemObject = wandaSeawatGridDataObject.getDataObjectExchangeItem("data");
		assertTrue(dataExchangeItemObject instanceof DoublesExchangeItem);
		DoublesExchangeItem dataExchangeItem = (DoublesExchangeItem) dataExchangeItemObject;
		double[] values = dataExchangeItem.getValuesAsDoubles();
		assertEquals("Data for 30 rows and 72 columns", 2160, values.length);
		assertEquals("Get value from 20th row of 30 and 2nd column of 72", 0.10554802E+02, values[19 * 72 + 1]);
		
		values[1] = 123456.0;
		dataExchangeItem.setValues(values);
		wandaSeawatGridDataObject.finish();

		File output = new File(testRunDataDir, "HTO_TEMP_20220531000100.ASC");
		try (Stream<String> stream = Files.lines(output.toPath())) {
			String lines = stream.collect(Collectors.joining("\n"));
			assertTrue("Updated value present in ini file", lines.contains(" -222.0000          0.10000001E+02          0.12345600E+06          0.10000000E+02"));
			assertTrue("Existing values present in ini file", lines.contains(" -290.0000          0.16336172E+02          0.10554955E+02          0.10028170E+02"));
		}
	}
}
