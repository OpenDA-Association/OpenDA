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
 * Test of reading and writing waterlevel data field of SWAN.
 */
public class SwanWLevelField2DFileTest extends TestCase {
    OpenDaTestSupport testData = null;
    private File testRunDataDir;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(SwanWLevelField2DFileTest.class, "model_swan");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testSwanWLevelField2DFile() throws IOException {
        File wLevelFilesTestDir = new File(testData.getTestRunDataDir(), "SwanWaterLevelFiles");
        File swnFile = new File(wLevelFilesTestDir, "swan_nautboom.swn");

        SwanField2DFile swanWLevelFile = new SwanField2DFile();
        swanWLevelFile.initialize(wLevelFilesTestDir,new String[] {"waterlevels.dat","swan_nautboom.swn","WLEV"});

        String exchangeItemID = swanWLevelFile.getExchangeItemIDs()[0];
        IPrevExchangeItem swnWLevelExchItem = swanWLevelFile.getExchangeItem(exchangeItemID);
//        System.out.println("Number of exchangeItems: "+ swnWLevelExchItem.length);
        System.out.println("swnWLevelExchItem[0].getRole(): "+ swnWLevelExchItem.getRole());
        System.out.println("swnWLevelExchItem[0].getId(): "+ swnWLevelExchItem.getId());
//        for (int i=0;i< swnWLevelExchItem.;i++){
//            System.out.println("swnWLevelExchItem["+i+"].id: "+ swnWLevelExchItem[i].getId());
//        }

        double[] wlevel = swnWLevelExchItem.getValuesAsDoubles();
        System.out.println("wlevel.length: "+wlevel.length);
        double[] wlevelorg = new double[wlevel.length];
        System.arraycopy(wlevel,0,wlevelorg,0,wlevel.length);
        double alpha = 1.0;
        for (int i=0;i<wlevel.length;i++){
            wlevel[i]=alpha*wlevel[i];
        }

        swnWLevelExchItem.setValuesAsDoubles(wlevel);
        ((SwanField2DFileExchangeItem)swnWLevelExchItem).resetTimeCounter();
        wlevel = swnWLevelExchItem.getValuesAsDoubles();
        for (int i=0;i<wlevel.length;i++){
            assertEquals("Waterlevel: ",wlevel[i],alpha*wlevelorg[i],1e-5);
        }

        double[] wlevelt2 = swnWLevelExchItem.getValuesAsDoubles();
        swnWLevelExchItem.setValuesAsDoubles(wlevel);

        File wlevelt1_reload = new File(testRunDataDir, "SwanWaterLevelFiles/water4swan/201003040400_WL.BLK");
        File wlevelt2_reload = new File(testRunDataDir, "SwanWaterLevelFiles/water4swan/201003040430_WL.BLK");
        assertTrue(testData.FilesAreIdentical(wlevelt1_reload, wlevelt2_reload));

        double[] wlevelt3 = swnWLevelExchItem.getValuesAsDoubles();
        alpha = 0.2;
        swnWLevelExchItem.axpyOnValues(alpha,wlevelt3);
        ((SwanField2DFileExchangeItem)swnWLevelExchItem).resetTimeCounter();
        double[] wlevelt3new = new double[wlevelt3.length];
        for (int i=0;i<3;i++){
            wlevelt3new = swnWLevelExchItem.getValuesAsDoubles();
        }
        for (int i=0;i<wlevelt3.length;i++){
            assertEquals("Waterlevel t3: ",wlevelt3new[i],alpha*wlevelt3[i]+wlevelt3[i],1e-5);
        }

        swanWLevelFile.finish();
    }
}
