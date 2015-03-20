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
package org.openda.model_swan;

import junit.framework.TestCase;
import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IDataObject;
import org.openda.model_swan.SwanResults;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Tests for Swan Results IoObject
 */
public class SwanResultsTest extends TestCase {

    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(SwanResultsTest.class,"model_swan");
    }

    public void testObservations_1() throws Exception {
        File test_1_dir = new File(testData.getTestRunDataDir(), "swanObsTest1");
        IDataObject ioObject = BBUtils.createDataObject(test_1_dir,
                SwanResults.class.getName(), "observations.txt", new String[]{});
        checkValues(ioObject);
    }

    public void testNestObservations_1() {
        File test_1_dir = new File(testData.getTestRunDataDir(), "swanObsNestedTest1");
        IDataObject ioObject = BBUtils.createDataObject(test_1_dir,
                SwanResults.class.getName(), "observations_loc.tab", new String[]{});
        checkValues(ioObject);
    }

    private void checkValues(IDataObject ioObject) {

        String[] exchangeItemsIDs = ioObject.getExchangeItemIDs();
        IExchangeItem[] exchangeItems = new IExchangeItem[exchangeItemsIDs.length];
        for (int i=0; i<exchangeItemsIDs.length; i++){
            exchangeItems[i] = ioObject.getDataObjectExchangeItem(exchangeItemsIDs[i]);
        }
        assertEquals("#exchangeItems", 78, exchangeItems.length);
        assertEquals("exchangeItems[0].getId()", "Hsig @ 206767.0,622696.0", exchangeItems[0].getId());
        assertEquals("exchangeItems[1].getId()", "RTpeak @ 206767.0,622696.0", exchangeItems[1].getId());
        assertEquals("exchangeItems[33].getId()", "Tm-2-1 @ 196992.0,612714.0", exchangeItems[33].getId());
        assertEquals("exchangeItems[61].getId()", "Tpeq @ 200738.0,607693.0", exchangeItems[61].getId());
        assertEquals("exchangeItems[76].getId()", "Tpbeq @ 205996.0,604802.0", exchangeItems[76].getId());
        assertEquals("exchangeItems[77].getId()", "Tpm @ 205996.0,604802.0", exchangeItems[77].getId());

        assertEquals("exchangeItems[0].getValuesAsDoubles()[0]", 2.261, exchangeItems[0].getValuesAsDoubles()[0]);
        assertEquals("exchangeItems[1].getValuesAsDoubles()[0]", 7.143, exchangeItems[1].getValuesAsDoubles()[0]);
        assertEquals("exchangeItems[33].getValuesAsDoubles()[0]", 6.701, exchangeItems[33].getValuesAsDoubles()[0]);
        assertEquals("exchangeItems[61].getValuesAsDoubles()[0]", 4.736, exchangeItems[61].getValuesAsDoubles()[0]);
        assertEquals("exchangeItems[76].getValuesAsDoubles()[0]", 3.794, exchangeItems[76].getValuesAsDoubles()[0]);
        assertEquals("exchangeItems[77].getValuesAsDoubles()[0]", 3.794, exchangeItems[77].getValuesAsDoubles()[0]);

    }
}
