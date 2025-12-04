/* MOD_V2.0
 * Copyright (c) 2012 OpenDA Association
 * All rights reserved.
 *
 * This file is part of OpenDA.
 *
 * OpenDA is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of
 * the License, or (at your option) any later version.
 *
 * OpenDA is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.openda.model_hec_hms;

import junit.framework.TestCase;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.util.List;

public class StateFileTest extends TestCase {
	private static final File TEST_RESOURCES_DIRECTORY = new File("java/testResources");

	public void testStateFileWithNoExchangeItems() throws IOException {
		// Given input file
		File temporaryInputFile = File.createTempFile("temp-no-exchange-items", ".state");
		File sourceInputFile = new File(TEST_RESOURCES_DIRECTORY, "no-exchange-items.state");
		try {
			Files.copy(sourceInputFile.toPath(), temporaryInputFile.toPath(), StandardCopyOption.REPLACE_EXISTING);

			// Given state file
			IDataObject dataObject = new StateFile();
			String[] args = {temporaryInputFile.getName()};

			// When initialized
			dataObject.initialize(new File(temporaryInputFile.getParent()), args);

			// Then no exchange items are found
			String[] exchangeItemIds = dataObject.getExchangeItemIDs();
			assertEquals(0, exchangeItemIds.length);

			// When finished
			dataObject.finish();

			// Then output file matches input file
			List<String> expectedLines = Files.readAllLines(new File(TEST_RESOURCES_DIRECTORY, "expected-no-exchange-items.state").toPath());
			List<String> actualLines = Files.readAllLines(temporaryInputFile.toPath());
			assertEquals(expectedLines, actualLines);
		} finally {
			Files.deleteIfExists(temporaryInputFile.toPath());
		}
	}

	public void testStateFile() throws IOException {
		// Given input file
		File temporaryInputFile = File.createTempFile("temp-exchange-items", ".state");
		File sourceInputFile = new File(TEST_RESOURCES_DIRECTORY, "exchange-items.state");
		try {
			Files.copy(sourceInputFile.toPath(), temporaryInputFile.toPath(), StandardCopyOption.REPLACE_EXISTING);

			// Given state file
			IDataObject dataObject = new StateFile();
			String[] args = {temporaryInputFile.getName()};

			// When initialized
			dataObject.initialize(new File(temporaryInputFile.getParent()), args);

			// Then exchange items are found
			String[] exchangeItemIds = dataObject.getExchangeItemIDs();
			assertEquals(2, exchangeItemIds.length);

			// Then exchange items contains values for: Subbasin: Subbasin-16, Loss: Soil Moisture Account, Groundwater Storage: 1
			IExchangeItem exchangeItem = dataObject.getDataObjectExchangeItem("Subbasin-16_Soil Moisture Account_1");
			assertNotNull(exchangeItem);
			assertTrue(exchangeItem instanceof StateFileExchangeItem);
			StateFileExchangeItem stateFileExchangeItem = (StateFileExchangeItem) exchangeItem;

			// Then the exchange item has an aray of values
			double[] values = stateFileExchangeItem.getValuesAsDoubles();
			assertEquals(12, values.length);
			assertEquals(0.238, values[2], 0.001);

			// When the values are updated
			for (int i = 0; i < values.length; i++) {
				values[i] = i / 2.0;
			}

			// And the file is output
			dataObject.finish();

			// Then the values are updated
			List<String> expectedLines = Files.readAllLines(new File(TEST_RESOURCES_DIRECTORY, "expected-exchange-items.state").toPath());
			List<String> actualLines = Files.readAllLines(temporaryInputFile.toPath());
			assertEquals(expectedLines, actualLines);
		} finally {
			Files.deleteIfExists(temporaryInputFile.toPath());
		}
	}
}
