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
package org.openda.exchange.dataobjects;

import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * @author johan
 *
 */
public class TimeSeriesFormatterDataObjectTest extends TestCase {
	private File              testRunDataDir;

   protected void setUp() throws IOException {
	   OpenDaTestSupport testData = new OpenDaTestSupport(TimeSeriesFormatterDataObjectTest.class, "core");
		this.testRunDataDir = testData.getTestRunDataDir();
	}

	@SuppressWarnings("boxing")
   public void testRead1() {

		// read noos file and create object
		TimeSeriesFormatterDataObject dataObject = new TimeSeriesFormatterDataObject();

		dataObject.initialize(this.testRunDataDir, new String[]{"TimeSeriesFormatter.xml"});

		String[] iDs = dataObject.getExchangeItemIDs();
		assertEquals(2, iDs.length);
		IExchangeItem exchangeItem = dataObject.getDataObjectExchangeItem("waterlevel@denhelder");
		exchangeItem.getValues();
	}

}
