/*
* Copyright (c) 2021 OpenDA Association 
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
package org.openda.model_metaswap;
import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;

public class SwapResultFileTest extends TestCase {
	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() {
		testData = new OpenDaTestSupport(SwapResultFileTest.class, "model_metaswap");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testRead() {
		SwapResultFile swapResultFile = new SwapResultFile();
		swapResultFile.initialize(testRunDataDir, new String[]{"svat_dtgw_0000207106.csv"});
		String[] exchangeItemIDs = swapResultFile.getExchangeItemIDs();
		assertNotNull(exchangeItemIDs);
		assertEquals(1, exchangeItemIDs.length);
		String exchangeItemID = exchangeItemIDs[0];
		assertEquals("0000207106.soilmoisture", exchangeItemID);
		IExchangeItem exchangeItem = swapResultFile.getDataObjectExchangeItem(exchangeItemID);
		assertNotNull(exchangeItem);
		assertTrue(exchangeItem instanceof SwapResultExchangeItem);
		double[] values = exchangeItem.getValuesAsDoubles();
		double[] times = exchangeItem.getTimes();
		assertEquals(125, values.length);
		assertEquals(125, times.length);
		assertEquals(124.0,times[times.length - 1] - times[0], 0.00001);

		assertEquals(329.8, values[0], 0.00001);
		assertEquals(57143.0, times[0], 0.00001);

		assertEquals(273.075, values[values.length - 1], 0.00001);
	}
}
