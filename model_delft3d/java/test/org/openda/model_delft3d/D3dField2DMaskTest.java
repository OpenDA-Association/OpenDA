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
 * Tests for D3D flow black box wrapper 2d-field subselection classes
 */
public class D3dField2DMaskTest extends TestCase {

    private OpenDaTestSupport testData = null;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(D3dField2DMaskTest.class,"model_delft3d");
    }

    public void testField2DMask() throws IOException {

        File testDir = new File(testData.getTestRunDataDir(), "test_1");

        // Place undisturbed copy of files
        File roughessBase = new File(testDir, "rough-base.rgh");
        File roughess = new File(testDir, "rough.rgh");
        BBUtils.copyFile(roughessBase, roughess);

        // Read roughness file, check original content
        D3dField2DFile roughnessFile = new D3dField2DFile();
        roughnessFile.initialize(testDir, "test.mdf", new String[] {"rgh"});
        IPrevExchangeItem uRoughExchItem = roughnessFile.getExchangeItems()[0];
        double[] uValues = uRoughExchItem.getValuesAsDoubles();
        assertEquals("exchItemRoughFile[1].values[185]", 7.012, uValues[185]);
        assertEquals("exchItemRoughFile[1].values[186]", 7.013, uValues[186]);

        // get subset of u-values according to M,N slection
        SelectorInterface selector = new D3dField2DMask();
        selector.initialize(testDir, new String[]{"mnselection.mns"});
        Object selectionResult = selector.select(uRoughExchItem.getValues());
        assertTrue("selectionResult type", selectionResult instanceof IVector);

        // adjust the subset, write the adjusted file
        IVector resultVector = (IVector) selectionResult;
        resultVector.scale(-1);
        Object deSelectionResult = selector.deselect(resultVector);
        uRoughExchItem.setValues(deSelectionResult);
        roughnessFile.finish();

        // Re-read roughness file, check changed u-values
        D3dField2DFile adjustedRoughnessFile = new D3dField2DFile();
        adjustedRoughnessFile.initialize(testDir, "test.mdf", new String[] {"rgh"});
        double[] adjustedUValues = adjustedRoughnessFile.getExchangeItems()[0].getValuesAsDoubles();
        assertEquals("exchItemRoughFile[1].values[185]", -7.012, adjustedUValues[185]);
        assertEquals("exchItemRoughFile[1].values[186]",  7.013, adjustedUValues[186]);
    }
}

