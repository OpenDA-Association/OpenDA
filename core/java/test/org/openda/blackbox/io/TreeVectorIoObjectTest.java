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
package org.openda.blackbox.io;
import junit.framework.TestCase;
import org.openda.blackbox.config.BBUtils;
import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.interfaces.IPrevExchangeItem;
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
		IoObjectInterface treeVectorIoObject = BBUtils.createIoObjectInstance(
				treeVectorDataDir, TreeVectorIoObject.class.getName(),
				"rtcParameterConfig.xml", new String[]{"OneExchangeItem"});
		IPrevExchangeItem[] exchangeItems = treeVectorIoObject.getExchangeItems();
		assertTrue("Tree vector id", exchangeItems[0].getId().equals("TV"));
		double[] values = exchangeItems[0].getValuesAsDoubles();
		for (int i = 0, valuesLength = values.length; i < valuesLength; i++) {
			values[i] += 10.d;
		}
		exchangeItems[0].setValuesAsDoubles(values);
		treeVectorIoObject.finish();
		testData.FilesAreIdentical(new File(treeVectorDataDir,"rtcParameterConfig.xml"),
				new File(treeVectorDataDir, "rtcParameterConfig_ref_1.xml"));
	}

	public void testMultiExchangeItems() {
		IoObjectInterface treeVectorIoObject = BBUtils.createIoObjectInstance(
				treeVectorDataDir, TreeVectorIoObject.class.getName(),
				"rtcParameterConfig.xml", new String[]{});
		IPrevExchangeItem[] exchangeItems = treeVectorIoObject.getExchangeItems();
		assertTrue("B01.01", exchangeItems[0].getId().equals("B01.01"));
		assertTrue("endTime", exchangeItems[19].getId().equals("B13.03"));
		for (int ei = 0; ei<20 ; ei+=19) {
			double[] values = exchangeItems[ei].getValuesAsDoubles();
			for (int i = 0, valuesLength = values.length; i < valuesLength; i++) {
				values[i] += 1.0d + (double) ei;
			}
			exchangeItems[ei].setValuesAsDoubles(values);
		}
		treeVectorIoObject.finish();
		testData.FilesAreIdentical(new File(treeVectorDataDir, "rtcParameterConfig.xml"),
				new File(treeVectorDataDir, "rtcParameterConfig_ref_2.xml"));
	}
}
