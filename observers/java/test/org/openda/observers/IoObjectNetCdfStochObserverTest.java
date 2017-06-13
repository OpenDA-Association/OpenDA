/* OpenDA v2.4 
* Copyright (c) 2017 OpenDA Association 
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
package org.openda.observers;

import junit.framework.TestCase;
import org.openda.interfaces.*;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.util.List;

public class IoObjectNetCdfStochObserverTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(IoObjectNetCdfStochObserverTest.class, "observers");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testIoObjectStochObserverWithNetcdfGridTimeSeriesExchangeItem() {
		IStochObserver stochObserver = new IoObjectStochObserver();
		stochObserver.initialize(testRunDataDir, new String[]{"ioObjectNetcdfStochObsConfig.xml"});
		IObservationDescriptions observationDescriptions = stochObserver.getObservationDescriptions();
		List<IPrevExchangeItem> exchangeItems = observationDescriptions.getExchangeItems();
		String var_40_gds4_name = "VAR_40_GDS4_SFC";
		IPrevExchangeItem var_40_GDS4_item = null;
		for (int i = 0; i < exchangeItems.size(); i++) {
			IPrevExchangeItem exchangeItem = exchangeItems.get(i);
			String exchangeItemId = exchangeItem.getId();
			if (exchangeItemId.equalsIgnoreCase(var_40_gds4_name)) {
				var_40_GDS4_item = exchangeItem;
			}
		}
		assertNotNull("exchangeItem not found", var_40_GDS4_item);
		assertTrue("incorrect exchangeItem type", var_40_GDS4_item instanceof IExchangeItem);

		IVector realization1 = stochObserver.getRealizations();
		IVector realization2 = stochObserver.getRealizations();
		IVector expectations = stochObserver.getExpectations();
		int numberOfObservationPointsWithNonMissingValues = realization1.getSize();
		assertEquals("incorrect size", 441786, numberOfObservationPointsWithNonMissingValues);

		int numSameRandomValues = 0;
		for (int i = 0 ; i < numberOfObservationPointsWithNonMissingValues; i++) {
			double val1 = realization1.getValue(i);
			double val2 = realization2.getValue(i);
			if (!Double.isNaN(val1) && !Double.isNaN(val2)) {
				int valCompare = new Double(val1).compareTo(val2);
				if (valCompare == 0) {
					numSameRandomValues++;
				}
			}
		}
		assertEquals("#same random values", 0, numSameRandomValues);
	}
}
