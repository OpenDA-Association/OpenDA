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
package org.openda.blackbox.io;

import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Tests for IoObject based on treevector xml-file
 */
public class TreeVectorIoObjectTest extends TestCase {

    private File treeVectorDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
        testData = new OpenDaTestSupport(TreeVectorIoObjectTest.class, "core");
        treeVectorDataDir = new File(testData.getTestRunDataDir(), "treeVector");
    }

    public void testSingleExchangeItem() {
    	TreeVectorDataObject treeVectorObject = new TreeVectorDataObject();
    	String fileName = "rtcParameterConfig.xml";
    	String[] arguments = {fileName,"OneExchangeItem"};
    	treeVectorObject.initialize(treeVectorDataDir, arguments);
		String[] exchangeItemIDs = treeVectorObject.getExchangeItemIDs();
		IExchangeItem exchangeItem = treeVectorObject.getDataObjectExchangeItem(exchangeItemIDs[0]);
		assertEquals("Tree vector id", "TV", exchangeItem.getId());
		double[] values = exchangeItem.getValuesAsDoubles();
		for (int i = 0, valuesLength = values.length; i < valuesLength; i++) {
			values[i] += 10.d;
		}
		exchangeItem.setValuesAsDoubles(values);
		treeVectorObject.finish();
		testData.FilesAreIdentical(new File(treeVectorDataDir,"rtcParameterConfig.xml"),
				new File(treeVectorDataDir, "rtcParameterConfig_ref_1.xml"));
	}

	public void testMultiExchangeItems() {
		TreeVectorDataObject treeVectorObject = new TreeVectorDataObject();
		String fileName = "rtcParameterConfig.xml";
		String[] arguments = {fileName};
		treeVectorObject.initialize(treeVectorDataDir, arguments);
		String[] exchangeItemIDs = treeVectorObject.getExchangeItemIDs();
		IExchangeItem firstExchangeItem = treeVectorObject.getDataObjectExchangeItem(exchangeItemIDs[0]);
		assertEquals("B01.01", "B01.01", firstExchangeItem.getId());
		IExchangeItem lastExchangeItem = treeVectorObject.getDataObjectExchangeItem(exchangeItemIDs[19]);
		assertEquals("endTime", "B13.03", lastExchangeItem.getId());
		for (int ei = 0; ei<20 ; ei+=19) {
			IExchangeItem exchangeItem = treeVectorObject.getDataObjectExchangeItem(exchangeItemIDs[ei]);
			double[] values = exchangeItem.getValuesAsDoubles();
			for (int i = 0, valuesLength = values.length; i < valuesLength; i++) {
				values[i] += 1.0d + (double) ei;
			}
			exchangeItem.setValuesAsDoubles(values);
		}
		treeVectorObject.finish();
		testData.FilesAreIdentical(new File(treeVectorDataDir, "rtcParameterConfig.xml"),
				new File(treeVectorDataDir, "rtcParameterConfig_ref_2.xml"));
	}

	public void testReadRtcStateImport() {
		try {
			TreeVectorDataObject treeVectorObject = new TreeVectorDataObject();
			String fileName = "state_import.xml";
			String[] arguments = {fileName};
			treeVectorObject.initialize(treeVectorDataDir, arguments);
			String[] exchangeItemIDs = treeVectorObject.getExchangeItemIDs();
			IExchangeItem exchangeItem = treeVectorObject.getDataObjectExchangeItem(exchangeItemIDs[0]);
			assertEquals("Main_P[0]", "Main_P[0]", exchangeItem.getId());
		} catch (Exception e) {
			if (!e.getMessage().contains("Parsing Error : Content is not allowed in prolog")) {
				throw e;
			}
		}
	}
}
