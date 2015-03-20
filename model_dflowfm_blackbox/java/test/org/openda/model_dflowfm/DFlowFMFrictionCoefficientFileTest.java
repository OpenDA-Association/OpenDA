/* MOD_V1.0
* Copyright (c) 2010 OpenDA Association
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
package org.openda.model_dflowfm;

import junit.framework.TestCase;
import org.openda.interfaces.IExchangeItem;
import org.openda.model_dflowfm.DflowfmFrictionCoefficientFile;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 */
public class DFlowFMFrictionCoefficientFileTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(DflowfmFrictionCoefficientFile.class,"model_dflowfm_blackbox");
        testRunDataDir = new File(testData.getTestRunDataDir(), "Forcingfile");
    }

    public void testExistingfile(){

        // First write a test file
    	File dataDir=testRunDataDir;

		DflowfmFrictionCoefficientFile frictionCoefficientFile = new DflowfmFrictionCoefficientFile();
		String arg[] = new String[1];
		arg[0] = "frcfact.xyz";
        frictionCoefficientFile.initialize(testRunDataDir, arg);

		String ids[] = frictionCoefficientFile.getExchangeItemIDs();

        int n=ids.length;
        assertEquals(3, n);
		IExchangeItem[] items = new IExchangeItem[n];

        for(int i=0;i<n;i++){
        	String id=ids[i];
			items[i] = frictionCoefficientFile.getDataObjectExchangeItem(id);
			assertEquals(items[i].getValuesType(), IExchangeItem.ValueType.doubleType);
        }
        
        // change parameters
		items[0].setValues(1.1);
        items[1].setValues(0.9);
		items[2].setValues(1.05);
		// flush to file
        frictionCoefficientFile.finish();
        
        // check output
        File outputFile=new File(testRunDataDir,"frcfact.xyz");
        File referenceFile=new File(dataDir,"frcfact.xyz.ref");
        assertTrue(outputFile.exists());
        assertTrue(referenceFile.exists());
        assertTrue(testData.FilesAreIdentical(outputFile, referenceFile));
    }
}
