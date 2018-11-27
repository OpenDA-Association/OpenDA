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
import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IArray;
import org.openda.interfaces.IDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.utils.Array;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Tests for Swan Results IoObject
 */
public class PythonDataObjectTest extends TestCase {

    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(PythonDataObject.class,"core");
    }

	public void testObservations1() throws Exception {
        File testDir = new File(testData.getTestRunDataDir(),"MyPython");
        //IDataObject ioObject = BBUtils.createDataObject(testDir,
		//	PythonDataObject.class.getName(), "swanObservations.txt", new String[]{"swanDataObject", "/v3/Stage/Rick/openda/openda_public/core/java/test/org/openda/exchange/dataobjects/testData/python"});
		IDataObject ioObject = BBUtils.createDataObject(testDir,
			PythonDataObject.class.getName(), "swanObservations.txt", new String[]{"swanDataObject"});
		String[] actual = ioObject.getExchangeItemIDs();
		String[] expected = new String[]{"Depth @ 206767.0,622696.0"};
		assertEquals("swanDataObject ids length", actual.length, 84);
		assertEquals("id[0] swanDataObject", actual[0] , "Depth @ 206767.0,622696.0");
		assertEquals("id[83] swanDataObject", actual[83] , "Tpm @ 205996.0,604802.0");

		IExchangeItem exchangeItem = ioObject.getDataObjectExchangeItem("Depth @ 206767.0,622696.0");
		assertEquals("exchangeItem valueType", IArray.class, exchangeItem.getValueType() );
		assertEquals("exchangeItem value", new Array(new double[]{-999.0}), exchangeItem.getValues() );
		exchangeItem = ioObject.getDataObjectExchangeItem("Tpm @ 205996.0,604802.0");
		assertEquals("exchangeItem value", new Array(new double[]{3.794}), exchangeItem.getValues() );
	}

}
