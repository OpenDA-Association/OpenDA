/* OpenDA v2.4 
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

package org.openda.model_swan;

import junit.framework.TestCase;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Test of exchange item for reading and writing simulation time information of SWAN.
 */
public class SwanTimeInfoTest extends TestCase {

    OpenDaTestSupport testData = null;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(SwanTimeInfoTest.class, "model_swan");
    }

    public void testSwanTimeInfo(){
        File testRunDataParamFilesDir = new File(testData.getTestRunDataDir(), "SwanParameters");
        SwanTimeInfo swanTimeInfo = new SwanTimeInfo();
        swanTimeInfo.initialize(testRunDataParamFilesDir,new String[]{"swantestunstruct_circle.swn"});

        IPrevExchangeItem[] swnTimeInfoExchangeItem = swanTimeInfo.getExchangeItems();

        assertEquals("swnTimeInfoExchangeItem.length: ",3,swnTimeInfoExchangeItem.length);
        assertEquals("swnTimeInfoExchangeItem[0].getId(): ","start_time",swnTimeInfoExchangeItem[0].getId());
        assertEquals("swnTimeInfoExchangeItem[1].getId(): ","end_time",swnTimeInfoExchangeItem[1].getId());
        assertEquals("swnTimeInfoExchangeItem[0].getValues(): ",55197.0,swnTimeInfoExchangeItem[0].getValues());
        assertEquals("swnTimeInfoExchangeItem[1].getValues(): ",55198.0,swnTimeInfoExchangeItem[1].getValues());

        Object times;
        times = new double[1];
        times = swnTimeInfoExchangeItem[0].getValues();
        double[] timestart = swnTimeInfoExchangeItem[0].getValuesAsDoubles();
        double[] timeend = swnTimeInfoExchangeItem[1].getValuesAsDoubles();
        timestart[0]++;
        timeend[0] = timeend[0]+2.0;
        swnTimeInfoExchangeItem[0].setValuesAsDoubles(timestart);
        swnTimeInfoExchangeItem[1].setValuesAsDoubles(timeend);
        swanTimeInfo.finish();

        // reread input file:
        SwanTimeInfo swanTimeInfo2 = new SwanTimeInfo();
        swanTimeInfo2.initialize(testRunDataParamFilesDir,new String[]{"swantestunstruct_circle.swn"});
        IPrevExchangeItem[] swnTimeInfoExchangeItem2 = swanTimeInfo2.getExchangeItems();
        assertEquals("swnTimeInfoExchangeItem2[0].getValues(): ",timestart[0],swnTimeInfoExchangeItem2[0].getValues());
        assertEquals("swnTimeInfoExchangeItem2[1].getValues(): ",timeend[0],swnTimeInfoExchangeItem2[1].getValues());
        swanTimeInfo2.finish();

    }

}

