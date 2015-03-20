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
package org.openda.observers;
import junit.framework.TestCase;
import org.openda.interfaces.IStochObserver;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Test for ObserverUtils class.
 */
public class ObserverUtilsTest extends TestCase {
    private File testRunDataDir;

    protected void setUp() throws IOException {
        OpenDaTestSupport testData = new OpenDaTestSupport(ObserverUtilsTest.class, "observers");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testObserverUtils(){
        IStochObserver obs1 = new NoosTimeSeriesStochObserver();
        obs1.initialize(testRunDataDir,new String[]{"noosObservations.xml"});

        ObserverUtils obsUtils = new ObserverUtils(obs1);
        
        String[] obsIds = obsUtils.getObsIds();
        assertEquals(38, obsIds.length);
        assertEquals("den helder.waterlevel_astro",obsIds[0]);
        
        double refTime = 54466.0;
        double[] offsets = obsUtils.getObsTimeOffsets(refTime);
        assertEquals(38, offsets.length);
        assertEquals(0.0/1440.0,offsets[0],0.00001);
        assertEquals(10.0/1440.0,offsets[1],0.00001);
        assertEquals(180.0/1440.0,offsets[37],0.00001);
     }
}
