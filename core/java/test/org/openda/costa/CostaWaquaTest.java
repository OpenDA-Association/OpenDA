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

package org.openda.costa;

import junit.framework.TestCase;
import org.openda.interfaces.IVector;

import java.io.File;

/**
 * Test for COSTA Components
 */
public class CostaWaquaTest extends TestCase {

    public void testDummy() {
        // No action. Test is just here to avoid "No tests found in ... " warning
    }

	public static void tstModel() {

		File modelConfigFile = new File(
				"waqua.xml");
		assertTrue("FILE existence check " + modelConfigFile.getAbsolutePath(),
				modelConfigFile.exists());

        File modelClsConfigFile = new File(
				"waqua_class.xml");
		assertTrue("FILE existence check " + modelClsConfigFile.getAbsolutePath(),
				modelConfigFile.exists());


        CtaOpenDaModel waqua = new CtaOpenDaModel(modelClsConfigFile.getAbsolutePath(), modelConfigFile.getAbsolutePath());

        for (int i=1; i<1000; i++){
           IVector x =  waqua.getState();
           double nrm=x.norm2();
           System.out.print("norm is "+nrm +"\n");
        }
    }
}
