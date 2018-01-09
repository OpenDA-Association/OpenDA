/* OpenDA v2.3.1 
* Copyright (c) 2016 OpenDA Association 
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

package org.openda.model_lhm;

import junit.framework.TestCase;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Test of exchange item for reading and writing LHM input file.
 */
public class LHMTimeSettingsTest extends TestCase {

    OpenDaTestSupport testData = null;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(LHMTimeSettingsTest.class, "model_lhm");
    }

    public void testlhmTimeSettings(){
        File testRunDataParamFilesDir = new File(testData.getTestRunDataDir(), "LHMTimeSettings");
        //System.out.println(testRunDataParamFilesDir);
        LHMTimeSettings lhmTimeSettings = new LHMTimeSettings();
        lhmTimeSettings.initialize(testRunDataParamFilesDir,new String[]{"NHI.bat","dates.txt"});

        IPrevExchangeItem[] lhmTimeSettingsExchangeItem = lhmTimeSettings.getExchangeItems();
		//System.out.println(lhmTimeSettingsExchangeItem[0].getId());
		//System.out.println(lhmTimeSettingsExchangeItem[1].getId());
        assertEquals("lhmTimeSettingsExchangeItem[0].getId(): ","start_time",lhmTimeSettingsExchangeItem[0].getId());
        assertEquals("lhmTimeSettingsExchangeItem[1].getId(): ","end_time",lhmTimeSettingsExchangeItem[1].getId());

        double[] startTime = lhmTimeSettingsExchangeItem[0].getValuesAsDoubles();
        double[] endTime = lhmTimeSettingsExchangeItem[1].getValuesAsDoubles();
        System.out.println("Read from dates.txt: "+startTime[0] +" - "+endTime[0]);

        assertEquals("start: ",57388.0,startTime[0]);
        assertEquals("end: ",57397.0,endTime[0]);

        startTime[0]++;
        endTime[0] = endTime[0]+3.0;
        lhmTimeSettingsExchangeItem[0].setValuesAsDoubles(startTime);
        lhmTimeSettingsExchangeItem[1].setValuesAsDoubles(endTime);

        lhmTimeSettings.finish( );

        startTime = lhmTimeSettingsExchangeItem[0].getValuesAsDoubles();
        endTime = lhmTimeSettingsExchangeItem[1].getValuesAsDoubles();

        //System.out.println(startTime[0]+" "+endTime[0]);
        //assertEquals("times[0]: ",57387.0,startTime[0]);
        //assertEquals("times[1]: ",57756.0,endTime[0]);
//*/
    }
}
