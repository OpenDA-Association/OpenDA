package org.openda.model_hec_hms;

import junit.framework.TestCase;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.List;

public class ControlFileTest extends TestCase {
	private static final File TEST_RESOURCES_DIRECTORY = new File("java/testResources");
	private static final double DELTA = 0.001;

	public void testControlFile() throws IOException {
		// Given input file
		File temporaryInputFile = File.createTempFile("temp-exchange-items", ".control");
		File sourceInputFile = new File(TEST_RESOURCES_DIRECTORY, "exchange-items.control");
		try {
			Files.copy(sourceInputFile.toPath(), temporaryInputFile.toPath(), StandardCopyOption.REPLACE_EXISTING);

			// Given control file
			IDataObject dataObject = new ControlFile();
			String[] args = {temporaryInputFile.getName()};

			// When initialized
			dataObject.initialize(new File(temporaryInputFile.getParent()), args);

			// Then exchange items are found
			String[] exchangeItemIds = dataObject.getExchangeItemIDs();
			assertEquals(1, exchangeItemIds.length);

			// Then exchange items contains values for:
			IExchangeItem exchangeItem = dataObject.getDataObjectExchangeItem("Control_File");
			assertNotNull(exchangeItem);
			assertTrue(exchangeItem instanceof ControlFileExchangeItem);
			ControlFileExchangeItem controlFileExchangeItem = (ControlFileExchangeItem) exchangeItem;

			// Then the exchange item has an array of values
			double[] values = controlFileExchangeItem.getValuesAsDoubles();
			assertEquals(2, values.length);
			assertEquals(60611.958, values[0], DELTA);
			assertEquals(60612.166, values[1], DELTA);

			// When the values are updated
			double[] newTimes = new double[2];
			newTimes[0] = 60637.875; // 23 November 2024 21:00 UTC
			newTimes[1] = 60678.25; // 03 January 2025 06:00 UTC

			exchangeItem.setTimes(newTimes);

			// And the file is output
			dataObject.finish();

			// Then the values are updated
			List<String> expectedLines = Files.readAllLines(new File(TEST_RESOURCES_DIRECTORY, "expected-exchange-items.control").toPath());
			List<String> actualLines = Files.readAllLines(temporaryInputFile.toPath());
			assertEquals(expectedLines, actualLines);
		} finally {
			Files.deleteIfExists(temporaryInputFile.toPath());
		}
	}
}
