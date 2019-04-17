package org.openda.exchange.dataobjects;
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


import junit.framework.TestCase;

import java.io.File;
import java.io.IOException;

import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.OpenDaTestSupport;

public class AsciiVectorDataObjectTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(AsciiVectorDataObject.class, "core");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testIoCopySeriesFromObject(){
		System.out.println("==============================================================================");
		System.out.println(" Basic test for AsciiVectorDataObject");
		System.out.println("==============================================================================");

		double eps=1.0e-8;

		AsciiVectorDataObject vec1 = new AsciiVectorDataObject();
		vec1.initialize(this.testRunDataDir, new String[] {"vector1.txt"});
		IPrevExchangeItem[] exchange1 = vec1.getExchangeItems();
		assertEquals(1,exchange1.length);
		double[] vals1 = exchange1[0].getValuesAsDoubles();
		assertEquals(5, vals1.length);
		assertEquals(1.0, vals1[0], eps);
		assertEquals(2.0, vals1[1], eps);
		assertEquals(3.0, vals1[2], eps);
		assertEquals(4.0, vals1[3], eps);
		assertEquals(5.0, vals1[4], eps);

		for (int i=0; i<vals1.length; i++){
			vals1[i]=vals1[i]+0.001;
		}
		exchange1[0].setValuesAsDoubles(vals1);

		vec1.finish();

		AsciiVectorDataObject vec2 = new AsciiVectorDataObject();
		vec2.initialize(this.testRunDataDir, new String[] {"vector1.txt"});
		IPrevExchangeItem [] exchange2 = vec2.getExchangeItems();
		assertEquals(1,exchange2.length);
		double[] vals2 = exchange2[0].getValuesAsDoubles();
		assertEquals(5, vals1.length);
		assertEquals(1.001, vals2[0], eps);
		assertEquals(2.001, vals2[1], eps);
		assertEquals(3.001, vals2[2], eps);
		assertEquals(4.001, vals2[3], eps);
		assertEquals(5.001, vals2[4], eps);
	}

	public void testIoCopySingleValuesFromFile() {

		double eps=1.0e-8;
		AsciiVectorDataObject vec1 = new AsciiVectorDataObject();
		vec1.initialize(this.testRunDataDir, new String[] {"vector1.txt", "as_separate_exchange_items"});

		IExchangeItem[] exchange_items = vec1.getExchangeItems();
		assertEquals(5, exchange_items.length);
		assertEquals(1.0, exchange_items[0].getValuesAsDoubles()[0], eps);
		assertEquals(2.0, exchange_items[1].getValuesAsDoubles()[0], eps);
		assertEquals(3.0, exchange_items[2].getValuesAsDoubles()[0], eps);
		assertEquals(4.0, exchange_items[3].getValuesAsDoubles()[0], eps);
		assertEquals(5.0, exchange_items[4].getValuesAsDoubles()[0], eps);

		for (IExchangeItem exchange_item : exchange_items) {
			double[] temp_value = new double[1];
			temp_value[0] = exchange_item.getValuesAsDoubles()[0] + 0.001;
			exchange_item.setValuesAsDoubles(temp_value);
		}

		// Write to file
		vec1.finish();

		AsciiVectorDataObject vec2 = new AsciiVectorDataObject();
		vec2.initialize(this.testRunDataDir, new String[] {"vector1.txt", "as_separate_exchange_items"});

		IExchangeItem[] exchange_items2 = vec2.getExchangeItems();
		assertEquals(5, exchange_items.length);
		assertEquals(1.001, exchange_items2[0].getValuesAsDoubles()[0], eps);
		assertEquals(2.001, exchange_items2[1].getValuesAsDoubles()[0], eps);
		assertEquals(3.001, exchange_items2[2].getValuesAsDoubles()[0], eps);
		assertEquals(4.001, exchange_items2[3].getValuesAsDoubles()[0], eps);
		assertEquals(5.001, exchange_items2[4].getValuesAsDoubles()[0], eps);

	}

}




