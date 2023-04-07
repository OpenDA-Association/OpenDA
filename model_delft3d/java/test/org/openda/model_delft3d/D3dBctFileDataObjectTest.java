/*
* Copyright (c) 2023 OpenDA Association 
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

public class D3dBctFileDataObjectTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(D3dBctFileDataObjectTest.class, "model_delft3d");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testBctFile() {

		File dataDir = new File(testRunDataDir, "D3dBctFileDataObject");
		D3dBctFileDataObject bctFile = new D3dBctFileDataObject();
		bctFile.initialize(dataDir,  new String[]{"Est1D.bct"});


		String[] exchangeItemIDs = bctFile.getExchangeItemIDs();
		assertEquals(2, exchangeItemIDs.length);

		for (int i = 0; i < exchangeItemIDs.length; i++) {
			String exchangeItemID = exchangeItemIDs[i];
			IExchangeItem exchangeItem = bctFile.getDataObjectExchangeItem(exchangeItemID);
			double[] multiplicationFactors = new double[exchangeItem.getTimes().length];
			Arrays.fill(multiplicationFactors, 1 + i / 10d);
			exchangeItem.multiplyValues(multiplicationFactors);
		}

		bctFile.finish();

		// check output
		File outputFile = new File(dataDir, "Est1D.bct");
		File referenceFile = new File(dataDir, "Est1D_expected.bct");
		assertTrue(outputFile.exists());
		assertTrue(referenceFile.exists());

		assertTrue(testData.FilesAreIdentical(outputFile, referenceFile));

	}
}
