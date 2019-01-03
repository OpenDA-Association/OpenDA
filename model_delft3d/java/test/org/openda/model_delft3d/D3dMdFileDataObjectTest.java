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
package org.openda.model_delft3d;
import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;

/**
 * Created by Theo on 12.04.2016.
 */
public class D3dMdFileDataObjectTest extends TestCase {

	OpenDaTestSupport testData = null;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(D3dMdFileDataObjectTest.class,"model_delft3d");
	}

	public void testGetDataObjectExchangeItem() {

		D3dMdFileDataObject mdFile = new D3dMdFileDataObject();
		mdFile.initialize(testData.getTestRunDataDir(), new String[] {"simple-mdfile.mdf","simple-mdfile-out.mdf"});
		String[] exchangeItemIDs = mdFile.getExchangeItemIDs();
//		assertEquals("#exch.items", 4, exchangeItemIDs.length);
		IExchangeItem exchangeItem = mdFile.getDataObjectExchangeItem("Stantn");
		assertNotNull("stantn must be there", exchangeItem);
		double[] valuesAsDoubles = exchangeItem.getValuesAsDoubles();
		assertEquals("#values", 1, valuesAsDoubles.length);
		assertEquals("stantn value", 0.0013d, valuesAsDoubles[0]);
		exchangeItem.multiplyValues(new double[] {2d});

		IExchangeItem exchangeItemTstop = mdFile.getDataObjectExchangeItem("Tstop");
		exchangeItemTstop.multiplyValues(new double[] {2d});

		IExchangeItem exchangeItemTstart = mdFile.getDataObjectExchangeItem("Tstart");
		exchangeItemTstart.multiplyValues(new double[] {3d});

		IExchangeItem exchangeItemD1 = mdFile.getDataObjectExchangeItem("Wstres_D1");
		assertNotNull("First coeff of Wstres must be there", exchangeItemD1);
		double[] valuesAsDoublesD1 = exchangeItemD1.getValuesAsDoubles();
		assertEquals("#values", 0.016d, valuesAsDoublesD1[0]);

		mdFile.finish();


		File mdFileOrg = new File(testData.getTestRunDataDir(), "simple-mdfile-out.mdf");
		File mdFileExpected = new File(testData.getTestRunDataDir(), "simple-mdfile-expected.mdf");
		assertTrue("Ajdusted file OK", testData.FilesAreIdentical(mdFileOrg, mdFileExpected, 0, 1.0e-8));
	}
}
