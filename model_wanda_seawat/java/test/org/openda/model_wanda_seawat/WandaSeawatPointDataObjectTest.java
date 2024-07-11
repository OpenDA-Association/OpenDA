package org.openda.model_wanda_seawat;

import junit.framework.TestCase;
import org.openda.exchange.DoublesExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class WandaSeawatPointDataObjectTest extends TestCase {
	OpenDaTestSupport testData = null;
	private File testRunDataDir;

	protected void setUp() {
		testData = new OpenDaTestSupport(WandaSeawatPointDataObjectTest.class, "model_wanda_seawat");
		testRunDataDir = new File(testData.getTestRunDataDir(), "WandaSeawatPointDataObject");
	}

	public void test_givenWandaSeawatData_whenInitialized_thenExchangeItemsLoaded() throws IOException {
		File file = new File(testRunDataDir, "HTO_TEMP_");
		WandaSeawatPointDataObject wandaSeawatPointDataObject = new WandaSeawatPointDataObject();
		wandaSeawatPointDataObject.initialize(testRunDataDir, new String[]{file.getName()});
		assertEquals("Exchange items all loaded", 2160, wandaSeawatPointDataObject.getExchangeItemIDs().length);
		String key = wandaSeawatPointDataObject.getExchangeItemIDs()[19 * 72 + 1];
		assertEquals("Get key from 20th row of 30 and 2nd column of 72", "depth-298.0_radius0.7624999881", key);
		Object exchangeItemObject = wandaSeawatPointDataObject.getDataObjectExchangeItem(key);
		assertTrue(exchangeItemObject instanceof DoublesExchangeItem);
		DoublesExchangeItem doublesExchangeItem = (DoublesExchangeItem) exchangeItemObject;
		assertEquals("Get first value from 20th row of 30 and 2nd column of 72", 0.10554802E+02, doublesExchangeItem.getValuesAsDoubles()[0]);
		assertEquals("Get second value from 20th row of 30 and 2nd column of 72", 0.12345600E+02, doublesExchangeItem.getValuesAsDoubles()[1]);
		
		doublesExchangeItem.setValuesAsDoubles(new double[]{0.10554802E+02, 0.65432100E+02});
		wandaSeawatPointDataObject.finish();

		File output = new File(testRunDataDir, "HTO_TEMP_20220531000200.ASC");
		try (Stream<String> stream = Files.lines(output.toPath())) {
			String lines = stream.collect(Collectors.joining("\n"));
			assertTrue("Updated value present in ini file", lines.contains(" -298.0000          0.16334927E+02          0.65432100E+02          0.10028160E+02"));
			assertTrue("Existing values present in ini file", lines.contains(" -302.0000          0.10001448E+02          0.10000043E+02          0.10000001E+02"));
		}
	}
}
