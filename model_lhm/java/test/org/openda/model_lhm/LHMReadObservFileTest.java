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
public class LHMReadObservFileTest extends TestCase {

    OpenDaTestSupport testData = null;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(LHMReadObservFileTest.class, "model_lhm");
    }

    public void testLhmReadObservFile(){
        File testRunDataParamFilesDir = new File(testData.getTestRunDataDir(), "LHMReadObservFile");

        LHMReadObservFile lhmReadObservFile = new LHMReadObservFile();
        lhmReadObservFile.initialize(testRunDataParamFilesDir,new String[]{"observ.dat",});

        double[] times = lhmReadObservFile.getTime();
        double[] values = lhmReadObservFile.getObsVal();

        System.out.println("\n First line from observ.dat: "+times[0] +" - "+values[0]);
        // test version contains day 91 of 2016 - and 5 timesteps of 1 day
        assertEquals("times[0]: ",54863.0,times[0]);
        assertEquals("values[0]: ",0.5,values[0]);



    }
}
