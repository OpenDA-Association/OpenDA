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
import org.openda.blackbox.interfaces.SelectorInterface;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IVector;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Tests for D3D flow black box wrapper wind field subselection classes
 */
public class D3dWindMaskTest extends TestCase {

    private OpenDaTestSupport testData = null;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(D3dWindMaskTest.class,"model_delft3d");
    }

    public void testField2DMask() throws IOException {

        File testDir = new File(testData.getTestRunDataDir(), "test_3");

        // Place undisturbed copy of files
        File windvBase = new File(testDir, "curvi_ll_corner_gridrow-base.amv");
        File windv = new File(testDir, "curvi_ll_corner_gridrow.amv");
        BBUtils.copyFile(windvBase, windv);

        // Read wind file, check original content
        D3dWindFile windFile = new D3dWindFile();
        windFile.initialize(testDir, new String[] {"test.mdf", "gv"});
        String theOneAndOnlyId = windFile.getExchangeItemIDs()[0];
        IPrevExchangeItem windExchItem = windFile.getDataObjectExchangeItem(theOneAndOnlyId);
        double[] vValues = windExchItem.getValuesAsDoubles();
        assertEquals("windFile[0].values[19]", 20.0, vValues[19]);
        assertEquals("windFile[0].values[20]", 1.0, vValues[20]);

        // get subset of v-values according to M,N slection
        SelectorInterface selector = new D3dWindMask();
        selector.initialize(testDir, new String[]{"mnselection.mns"});
        Object selectionResult = selector.select(windExchItem.getValues());
        assertTrue("selectionResult type", selectionResult instanceof IVector);

        // adjust the subset, write the adjusted file
        IVector resultVector = (IVector) selectionResult;
        resultVector.scale(-1);
        Object deSelectionResult = selector.deselect(resultVector);
        windExchItem.setValues(deSelectionResult);
        windFile.finish();

        // Re-read wind file, check changed v-values
        D3dWindFile adjustedWindFile = new D3dWindFile();
        adjustedWindFile.initialize(testDir, new String[] {"test.mdf", "gv"});
        theOneAndOnlyId = adjustedWindFile.getExchangeItemIDs()[0];
        IPrevExchangeItem adjustedWindExchItem = adjustedWindFile.getDataObjectExchangeItem(theOneAndOnlyId);
        double[] adjustedvValues = adjustedWindExchItem.getValuesAsDoubles();
        assertEquals("exchItemWindFile[0].values[5]", -6.0, adjustedvValues[5]);
        assertEquals("exchItemWindFile[0].values[6]",  7.0, adjustedvValues[6]);
        assertEquals("exchItemWindFile[0].values[0]",  -1.0, adjustedvValues[0]);
        assertEquals("exchItemWindFile[0].values[15]",  16.0, adjustedvValues[15]);
		adjustedWindFile.finish();
    }


	   public void testField2DMaskMirror() throws IOException {

        File testDir = new File(testData.getTestRunDataDir(), "test_3");

        // Place undisturbed copy of files
        File windvBase = new File(testDir, "curvi_ll_corner_gridrow-base.amv");
        File windv = new File(testDir, "curvi_ll_corner_gridrow.amv");
        BBUtils.copyFile(windvBase, windv);

        // Read wind file, check original content
        D3dWindFile windFile = new D3dWindFile();
        windFile.initialize(testDir, new String[] {"test.mdf", "gv"});
        String theOneAndOnlyId = windFile.getExchangeItemIDs()[0];
        IPrevExchangeItem windExchItem = windFile.getDataObjectExchangeItem(theOneAndOnlyId);
        double[] vValues = windExchItem.getValuesAsDoubles();
        assertEquals("windFile[0].values[19]", 20.0, vValues[19]);
        assertEquals("windFile[0].values[20]", 1.0, vValues[20]);

        // get subset of v-values according to M,N slection. Mirror this mns-selection!
        SelectorInterface selector = new D3dWindMask();
        selector.initialize(testDir, new String[]{"mnselection.mns","yes"});
        Object selectionResult = selector.select(windExchItem.getValues());
        assertTrue("selectionResult type", selectionResult instanceof IVector);

        // adjust the subset, write the adjusted file
        IVector resultVector = (IVector) selectionResult;
        resultVector.scale(-1);
        Object deSelectionResult = selector.deselect(resultVector);
        windExchItem.setValues(deSelectionResult);
        windFile.finish();

        // Re-read wind file, check changed v-values
        D3dWindFile adjustedWindFile = new D3dWindFile();
        adjustedWindFile.initialize(testDir, new String[] {"test.mdf", "gv"});
        theOneAndOnlyId = adjustedWindFile.getExchangeItemIDs()[0];
        IPrevExchangeItem adjustedWindExchItem = adjustedWindFile.getDataObjectExchangeItem(theOneAndOnlyId);
        double[] adjustedvValues = adjustedWindExchItem.getValuesAsDoubles();
        assertEquals("exchItemWindFile[0].values[5]", -6.0, adjustedvValues[5]);
        assertEquals("exchItemWindFile[0].values[6]",  7.0, adjustedvValues[6]);
        assertEquals("exchItemWindFile[0].values[0]",  1.0, adjustedvValues[0]);
        assertEquals("exchItemWindFile[0].values[15]",  -16.0, adjustedvValues[15]);
		adjustedWindFile.finish();
    }


}

