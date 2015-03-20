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
import org.openda.model_swan.SwanResultsTimeDependent;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Tests for Swan Results IoObject
 */
public class SwanResultsTimeDependentTest extends TestCase {

    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(SwanResultsTimeDependentTest.class,"model_swan");
    }

    public void testObservations_1() throws Exception {
        File test_1_dir = new File(testData.getTestRunDataDir(), "swanObsTimeDependent");
        IDataObject ioObject = BBUtils.createDataObject(test_1_dir,
                SwanResultsTimeDependent.class.getName(), "out_P1.TAB", new String[]{});
        checkValues(ioObject);
        ioObject = BBUtils.createDataObject(test_1_dir,
                SwanResultsTimeDependent.class.getName(), "out_P2.TAB", new String[]{});
        checkValues2(ioObject);
        ioObject = BBUtils.createDataObject(test_1_dir,
                SwanResultsTimeDependent.class.getName(), "out_P3.TAB", new String[]{});
        checkValues3(ioObject);
    }

    private void checkValues(IDataObject ioObject) {

        String[] exchangeItemsIDs = ioObject.getExchangeItemIDs();
        IExchangeItem[] exchangeItems = new IExchangeItem[exchangeItemsIDs.length];
        for (int i=0; i<exchangeItemsIDs.length; i++){
            exchangeItems[i] = ioObject.getDataObjectExchangeItem(exchangeItemsIDs[i]);
        }

        assertEquals("#exchangeItems", 12, exchangeItems.length);
        assertEquals("exchangeItems[1].getId()", "Hsig @ 1000.,1000.", exchangeItems[1].getId());
        assertEquals("exchangeItems[3].getId()", "Hswell @ 1000.,1000.", exchangeItems[3].getId());
        assertEquals("exchangeItems[4].getId()", "Tm_10 @ 1000.,1000.", exchangeItems[4].getId());
        assertEquals("exchangeItems[6].getId()", "Dir @ 1000.,1000.", exchangeItems[6].getId());
        assertEquals("exchangeItems[7].getId()", "Dspr @ 1000.,1000.", exchangeItems[7].getId());

        assertEquals("exchangeItems[1].getTimes()[45]", 55197.291666666664, exchangeItems[1].getTimeInfo().getTimes()[7]);
        assertEquals("exchangeItems[1].getTimes()[45]", 55198.875, exchangeItems[1].getTimeInfo().getTimes()[45]);
        assertEquals("exchangeItems[7].getTimes()[45]", 55198.875, exchangeItems[7].getTimeInfo().getTimes()[45]);
        assertEquals("exchangeItems[1].getValuesAsDoubles()[13]", 0.69154, exchangeItems[1].getValuesAsDoubles()[13]);
        assertEquals("exchangeItems[3].getValuesAsDoubles()[32]", 0.20267, exchangeItems[3].getValuesAsDoubles()[32]);
    }

    private void checkValues2(IDataObject ioObject) {

        String[] exchangeItemsIDs = ioObject.getExchangeItemIDs();
		IExchangeItem[] exchangeItems = new IExchangeItem[exchangeItemsIDs.length];
        for (int i=0; i<exchangeItemsIDs.length; i++){
            exchangeItems[i] = ioObject.getDataObjectExchangeItem(exchangeItemsIDs[i]);
        }

        assertEquals("#exchangeItems", 12, exchangeItems.length);
        assertEquals("exchangeItems[1].getId()", "Hsig @ 3000.,2000.", exchangeItems[1].getId());
        assertEquals("exchangeItems[3].getId()", "Hswell @ 3000.,2000.", exchangeItems[3].getId());
        assertEquals("exchangeItems[4].getId()", "Tm_10 @ 3000.,2000.", exchangeItems[4].getId());
        assertEquals("exchangeItems[6].getId()", "Dir @ 3000.,2000.", exchangeItems[6].getId());
        assertEquals("exchangeItems[7].getId()", "Dspr @ 3000.,2000.", exchangeItems[7].getId());

        assertEquals("exchangeItems[1].getTimes()[45]", 55197.291666666664, exchangeItems[1].getTimeInfo().getTimes()[7]);
        assertEquals("exchangeItems[1].getTimes()[45]", 55198.875, exchangeItems[1].getTimeInfo().getTimes()[45]);
        assertEquals("exchangeItems[7].getTimes()[45]", 55198.875, exchangeItems[7].getTimeInfo().getTimes()[45]);
        assertEquals("exchangeItems[1].getValuesAsDoubles()[13]", 0.3693, exchangeItems[1].getValuesAsDoubles()[13]);
        assertEquals("exchangeItems[3].getValuesAsDoubles()[32]", 0.10597, exchangeItems[3].getValuesAsDoubles()[32]);
    }

    private void checkValues3(IDataObject ioObject) {

        String[] exchangeItemsIDs = ioObject.getExchangeItemIDs();
		IExchangeItem[] exchangeItems = new IExchangeItem[exchangeItemsIDs.length];
        for (int i=0; i<exchangeItemsIDs.length; i++){
            exchangeItems[i] = ioObject.getDataObjectExchangeItem(exchangeItemsIDs[i]);
        }
        System.out.println("exchangetItems.length: "+exchangeItems.length);
        for (int i=0;i<exchangeItems.length;i++){
            System.out.println("exchangeItems["+i+"].getId(): "+exchangeItems[i].getId());
        }

        int nLocations = 6;
        int nObservedVariable = 12;
        assertEquals("#exchangeItems", nLocations*nObservedVariable, exchangeItems.length);
        assertEquals("exchangeItems[1].getId()", "Hsig @ 1000.,1000.", exchangeItems[1].getId());
        assertEquals("exchangeItems[3].getId()", "Hswell @ 1000.,1000.", exchangeItems[3].getId());
        assertEquals("exchangeItems[4].getId()", "Tm_10 @ 1000.,1000.", exchangeItems[4].getId());
        assertEquals("exchangeItems[6].getId()", "Dir @ 1000.,1000.", exchangeItems[6].getId());
        assertEquals("exchangeItems[7].getId()", "Dspr @ 1000.,1000.", exchangeItems[7].getId());

        assertEquals("exchangeItems[nObservedVariable+1].getId()", "Hsig @ 3000.,1000.", exchangeItems[nObservedVariable+1].getId());
        assertEquals("exchangeItems[nObservedVariable+3].getId()", "Hswell @ 3000.,1000.", exchangeItems[nObservedVariable+3].getId());
        assertEquals("exchangeItems[nObservedVariable+4].getId()", "Tm_10 @ 3000.,1000.", exchangeItems[nObservedVariable+4].getId());
        assertEquals("exchangeItems[nObservedVariable+6].getId()", "Dir @ 3000.,1000.", exchangeItems[nObservedVariable+6].getId());
        assertEquals("exchangeItems[nObservedVariable+7].getId()", "Dspr @ 3000.,1000.", exchangeItems[nObservedVariable+7].getId());

        assertEquals("exchangeItems[1].getValuesAsDoubles().length:",49,exchangeItems[1].getValuesAsDoubles().length);
        assertEquals("exchangeItems[1].getTimes()[7]", 55197.291666666664, exchangeItems[1].getTimeInfo().getTimes()[7]);
        assertEquals("exchangeItems[1].getTimes()[45]", 55198.875, exchangeItems[1].getTimeInfo().getTimes()[45]);
        assertEquals("exchangeItems[71].getTimes()[45]", 55198.875, exchangeItems[7].getTimeInfo().getTimes()[45]);
        assertEquals("exchangeItems[1].getValuesAsDoubles()[2]", 0.66985, exchangeItems[1].getValuesAsDoubles()[2]);
        assertEquals("exchangeItems[9].getValuesAsDoubles()[30]", -15.0000, exchangeItems[9].getValuesAsDoubles()[30]);
        assertEquals("exchangeItems[66].getValuesAsDoubles()[48]", 40.631, exchangeItems[66].getValuesAsDoubles()[48]);
    }
}
