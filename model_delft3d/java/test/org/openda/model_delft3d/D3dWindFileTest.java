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
package org.openda.model_delft3d;
import junit.framework.TestCase;
import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Tests for D3D flow black box wrapper wind field classes
 */
public class D3dWindFileTest extends TestCase {

    private OpenDaTestSupport testData = null;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(D3dWindFileTest.class,"model_delft3d");
    }

    public void testWindFiles() throws IOException {

        File testDir = new File(testData.getTestRunDataDir(), "test_3");

        // Place undisturbed copy of files
        File winduBase = new File(testDir, "curvi_ll_corner_gridrow-base.amu");
        File windu = new File(testDir, "curvi_ll_corner_gridrow.amu");
        BBUtils.copyFile(winduBase, windu);


        // Read wind file, check content
        D3dWindFile winduFile = new D3dWindFile();
        winduFile.initialize(testDir, new String[]{"test.mdf", "gu"});
		String theOneAndOnlyId = winduFile.getExchangeItemIDs()[0];
        IPrevExchangeItem windExchItem = winduFile.getDataObjectExchangeItem(theOneAndOnlyId);
        assertEquals("exchItemwinduFile[0].id", "windgu", windExchItem.getId());
        double[] windValues = windExchItem.getValuesAsDoubles();
        assertEquals("exchItemWindFile[0].values[25]", 6.0, windValues[25]);
        assertEquals("exchItemWindFile[0].values[26]", 7.0, windValues[26]);



        // Adjust values in wind file, write file
        for (int i = 0; i < windValues.length; i++) {
            windValues[i] += 0.01;
        }
        windExchItem.setValuesAsDoubles(windValues);
        winduFile.finish();

        // Re-read wind file, check changed values
        D3dWindFile adjustedWindFile = new D3dWindFile();
        adjustedWindFile.initialize(testDir, new String[] {"test.mdf", "gu"});
		theOneAndOnlyId = adjustedWindFile.getExchangeItemIDs()[0];
		IPrevExchangeItem adjustedWindExchItem = adjustedWindFile.getDataObjectExchangeItem(theOneAndOnlyId);
        double[] adjustedVValues = adjustedWindExchItem.getValuesAsDoubles();
        assertEquals("exchItemWindFile[0].values[6]", 7.01, adjustedVValues[6]);
        assertEquals("exchItemWindFile[0].values[7]", 8.01, adjustedVValues[7]);

        // New test for equidistant grid
        // TODO: the two test parts below fail due to absence of the test data files
//        D3dWindFile winduFileEqui = new D3dWindFile();
//        winduFileEqui.initialize(testDir, new String[]{"test_equidistant.mdf", "wu"});
//        String theNewOneAndOnlyId = winduFileEqui.getExchangeItemIDs()[0];
//        IPrevExchangeItem windExchItemEqui = winduFileEqui.getDataObjectExchangeItem(theNewOneAndOnlyId);
//        assertEquals("exchItemwinduFile[0].id", "windu", windExchItemEqui.getId());

        // New test for curvi grid with multiples "lines" (commented as tested only for reading the grid, a corresponding .amu file is needed).
//        D3dWindFile winduFileMulti = new D3dWindFile();
//        winduFileMulti.initialize(testDir, new String[]{"test_curvi_multi.mdf", "gu"});
//        String theMultiOneAndOnlyId = winduFileMulti.getExchangeItemIDs()[0];
//        IPrevExchangeItem windExchItemMulti = winduFileMulti.getDataObjectExchangeItem(theMultiOneAndOnlyId);
    }
}
