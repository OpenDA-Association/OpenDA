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

import java.io.File;

import junit.framework.TestCase;

import org.openda.utils.OpenDaTestSupport;

/**
 * Test for wind dependent parameter interpolation
 */
public class SwanWindDepParmsTest extends TestCase {

    private OpenDaTestSupport testData = null;

    protected void setUp() {
    	testData = new OpenDaTestSupport(SwanWindDepParmsTest.class, "model_swan");
    }

    public void testWindDirAndVel_1() {
        File unitTestDir = new File(testData.getTestRunDataDir(), "SwanWindDepParms");
        SwanWindDepParms windDepParms = new SwanWindDepParms(
                new File(unitTestDir, "windinterp.txt"));
        double actualWindDir = 345;
        double actualWindVel = 37;
        double actualHS = windDepParms.getHS(actualWindDir, actualWindVel);
        double actualTp = windDepParms.getTp(actualWindDir, actualWindVel);
        assertEquals("Actual HS", 12.0063, actualHS, 1.e-7);
        assertEquals("Actual Tp", 18.4599, actualTp, 1.e-7);
    }
}