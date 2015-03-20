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
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Test of exchange item for reading and writing SWAN input file.
 */
public class SwanInputFileTest extends TestCase {

    OpenDaTestSupport testData = null;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(SwanInputFileTest.class, "model_swan");
    }

    public void testSwanInputFile(){
        File testRunDataParamFilesDir = new File(testData.getTestRunDataDir(), "SwanParameters");
        SwanInputFile swanInputFile = new SwanInputFile();
        swanInputFile.initialize(testRunDataParamFilesDir,new String[]{"swantestunstruct_circle.swn"});

        IPrevExchangeItem swnInputExchangeItem = swanInputFile.getExchangeItem(swanInputFile.getExchangeItemIDs()[0]);
        double[] times = swnInputExchangeItem.getTimes();
        assertEquals("times[0]: ",55197.0,times[0]);
        assertEquals("times[1]: ",55198.0,times[1]);
        times[0]++;
        times[1] = times[1]+2.0;
        swnInputExchangeItem.setTimes(times);
        times = swnInputExchangeItem.getTimes();
        assertEquals("times[0]: ",55198.0,times[0]);
        assertEquals("times[1]: ",55200.0,times[1]);

        swanInputFile.finish();
    }

}
