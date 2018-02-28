/* OpenDA v2.4.3 
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


package org.openda.blackbox.wrapper;

import org.openda.blackbox.config.AliasDefinitions;
import org.openda.blackbox.config.BBModelVectorConfig;
import org.openda.blackbox.config.BBStochModelVectorConfig;
import org.openda.blackbox.config.IoObjectConfig;
import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.blackbox.interfaces.SelectorInterface;
import org.openda.exchange.ArrayExchangeItem;
import org.openda.exchange.ArrayTimeInfo;
import org.openda.interfaces.IDimensionIndex;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.Array;
import junit.framework.TestCase;
import org.openda.utils.DimensionIndex;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;

/**
 * Test for Black Box ExchangeItem
 */
public class BBExchangeItemTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(BBApplicationTest.class,"core");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testSelectFrom2DArrayExchangeItem() {
		// initialize fullsize 2D array, numbered [0,..,N]
		int[] dims = {23,11};
		double[] vals = new double[dims[0]*dims[1]];
		for (int i = 0; i < dims[0] ; i++) {
			for (int j = 0; j < dims[1] ; j++) {
				vals[j+i*dims[1]] = j+i*dims[1];
			}
		}
		Array ar = new Array(vals, dims, true);

		// initialize ExchangeItem with values from array
		ArrayExchangeItem arrayEI = new ArrayExchangeItem("2Darray", IPrevExchangeItem.Role.InOut);
		arrayEI.setArray(ar);

		// specify indices for the selection
		IDimensionIndex[] dimensionIndices = new IDimensionIndex[2];
		dimensionIndices[0] = new DimensionIndex(7, 13);
		dimensionIndices[1] = new DimensionIndex(2, 5);

		// initialize dummy/minimal objects in order to initialize the BBExchangeItem
		HashMap<String, SelectorInterface> selectors = null;
		IoObjectConfig ioObjectConfig = new IoObjectConfig("ioObject1",
				"org.openda.blackbox.wrapper.BBExchangeItemTest.DummyIoObject", "not used", new AliasDefinitions(), new String[0]);
		BBStochModelVectorConfig vectorConfig = new BBModelVectorConfig("item1", ioObjectConfig, "itemA",
				dimensionIndices, null, IPrevExchangeItem.Role.Output, null);

		// create the BBExchangeItem and ask for values
		BBExchangeItem bbExchangeItem = new BBExchangeItem("test", vectorConfig, arrayEI, selectors, testRunDataDir);
		double[] selectedValues = bbExchangeItem.getValuesAsDoubles();

		// check size of returned array
		assertEquals(selectedValues.length, dimensionIndices[0].getSize()*dimensionIndices[1].getSize());
		// check that values of selection match the item numbering of vals
		int count = 0;
		for (int j = dimensionIndices[1].getStart(); j < dimensionIndices[1].getEnd(); j++) {
			for (int i = dimensionIndices[0].getStart(); i < dimensionIndices[0].getEnd() ; i++) {
				int index = j + i * dims[1];
				assertEquals((int) selectedValues[count], index);
				count ++;
			}
		}
	}

	public void testSelectFrom3DArrayExchangeItem() {
		// initialize fullsize 3D array, numbered 0,..,N
		ArrayTimeInfo times = new ArrayTimeInfo(new double[3], 3);
		int[] dims = {3,5,7};
		int dimsize = dims[0]*dims[1]*dims[2];
		double[] vals = new double[dimsize];
		for (int i = 0; i < dimsize; i++) {
			vals[i] = i;
		}
		Array ar = new Array(vals, dims, true);

		// initialize ExchangeItem with values from array
		ArrayExchangeItem arrayEI = new ArrayExchangeItem("2Darray", IPrevExchangeItem.Role.InOut);
		arrayEI.setArray(ar);
		arrayEI.setTimeInfo(times);

		// specify indices for the selection: return all time values, selection in space (2D)
		IDimensionIndex[] dimensionIndices = new IDimensionIndex[2];
		dimensionIndices[0] = new DimensionIndex(2, 4);
		dimensionIndices[1] = new DimensionIndex(4, 7);

		// initialize dummy/minimal objects in order to initialize the BBExchangeItem
		HashMap<String, SelectorInterface> selectors = null;
		IoObjectConfig ioObjectConfig = new IoObjectConfig("ioObject1",
				"org.openda.blackbox.wrapper.BBExchangeItemTest.DummyIoObject", "not used", new AliasDefinitions(), new String[0]);
		BBStochModelVectorConfig vectorConfig = new BBModelVectorConfig("item1", ioObjectConfig, "itemA",
				dimensionIndices, null, IPrevExchangeItem.Role.Output, null);

		// create the BBExchangeItem and ask for values
		BBExchangeItem bbExchangeItem = new BBExchangeItem("test", vectorConfig, arrayEI, selectors, testRunDataDir);
		double[] selectedValues = bbExchangeItem.getValuesAsDoubles();

		// check size of returned array
		assertEquals(selectedValues.length, dims[0]*dimensionIndices[0].getSize()*dimensionIndices[1].getSize());
	}

	private class DummyIoObject implements IoObjectInterface{

		private IPrevExchangeItem[] exchangeItems;

		public void initialize(File workingDir, String fileName, String[] arguments) {
			exchangeItems = new IPrevExchangeItem[1];
			ArrayExchangeItem item1 = new ArrayExchangeItem("item1", IPrevExchangeItem.Role.Output);
			exchangeItems[0] = null;
		}

		public IPrevExchangeItem[] getExchangeItems() {
			return exchangeItems;
		}

		public void finish() {
			// no action
		}
	}
}

