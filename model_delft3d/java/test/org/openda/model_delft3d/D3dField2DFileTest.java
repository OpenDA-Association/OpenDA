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
 * Tests for D3D flow black box wrapper 2d-field classes
 */
public class D3dField2DFileTest extends TestCase {

    private OpenDaTestSupport testData = null;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(D3dField2DFileTest.class,"model_delft3d");
    }

    public void testField2DFiles() throws IOException {

        File testDir = new File(testData.getTestRunDataDir(), "test_1");

        // Place undisturbed copy of files
        File roughessBase = new File(testDir, "rough-base.rgh");
        File roughess = new File(testDir, "rough.rgh");
        BBUtils.copyFile(roughessBase, roughess);
        File depthBase = new File(testDir, "depth-base.dep");
        File depth = new File(testDir, "depth.dep");
        BBUtils.copyFile(depthBase, depth);

        // Read depth file, check content
        D3dField2DFile depthFile = new D3dField2DFile();
        depthFile.initialize(testDir, "test.mdf", new String[] {"dep"});
        IPrevExchangeItem[] depExchItems = depthFile.getExchangeItems();
        assertEquals("depExchItems.length", 1, depExchItems.length);
        assertEquals("exchItemdepthFile[0].id", "depth", depExchItems[0].getId());
        double[] depthValues = depExchItems[0].getValuesAsDoubles();
        assertEquals("exchItemRoughFile[1].values[344]", 9.49, depthValues[344]);
        assertEquals("exchItemRoughFile[1].values[345]", -999.000, depthValues[345]);

        // Read roughness file, check content
        D3dField2DFile roughnessFile = new D3dField2DFile();
        roughnessFile.initialize(testDir, "test.mdf", new String[] {"rgh"});
        IPrevExchangeItem[] rghExchItems = roughnessFile.getExchangeItems();
        assertEquals("rghExchItems.length", 2, rghExchItems.length);
        assertEquals("exchItemRoughFile[0].id", "roughness-u", rghExchItems[0].getId());
        assertEquals("exchItemRoughFile[1].id", "roughness-v", rghExchItems[1].getId());
        double[] uValues = rghExchItems[0].getValuesAsDoubles();
        double[] vValues = rghExchItems[1].getValuesAsDoubles();
        assertEquals("exchItemRoughFile[0].values[325]", 12.007, uValues[325]);
        assertEquals("exchItemRoughFile[0].values[326]", 12.008, uValues[326]);
        assertEquals("exchItemRoughFile[1].values[6]", 101.007, vValues[6]);
        assertEquals("exchItemRoughFile[1].values[7]", 101.008, vValues[7]);

        // Adjust v-values in roughness file, write file
        for (int i = 0; i < vValues.length; i++) {
            vValues[i] += 0.001;
        }
        rghExchItems[1].setValuesAsDoubles(vValues);
        roughnessFile.finish();

        // Re-read roughness file, check changed vValues
        D3dField2DFile adjustedRoughnessFile = new D3dField2DFile();
        adjustedRoughnessFile.initialize(testDir, "test.mdf", new String[] {"rgh"});
        double[] adjustedVValues = adjustedRoughnessFile.getExchangeItems()[1].getValuesAsDoubles();
        assertEquals("exchItemRoughFile[1].values[6]", 101.008, adjustedVValues[6]);
        assertEquals("exchItemRoughFile[1].values[7]", 101.009, adjustedVValues[7]);
    }
}
