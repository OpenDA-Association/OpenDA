/* OpenDA v2.4 
* Copyright (c) 2017 OpenDA Association 
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
import org.openda.interfaces.IVector;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 */
public class DFlowFMMeteoFileTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
        testData = new OpenDaTestSupport(DFlowFMMeteoFile.class,"model_dflowfm_blackbox");
        testRunDataDir = new File(testData.getTestRunDataDir(), "Meteofile");
    }

    public void testExistingfile(){

        // First write a test file
        File dataDir=testRunDataDir;

        DFlowFMMeteoFile frictionCoefficientFile = new DFlowFMMeteoFile();
        String arg[] = new String[3];
        arg[0] = "windx.amu";
        frictionCoefficientFile.initialize(testRunDataDir, arg);
        String ids[] = frictionCoefficientFile.getExchangeItemIDs();
        frictionCoefficientFile.finish();

        // check output
        File outputFile=new File(testRunDataDir,"windx.amu");
        File referenceFile=new File(testRunDataDir,"windx_ref.amu");
        assertTrue(outputFile.exists());
        assertTrue(referenceFile.exists());
        assertTrue(testData.FilesAreIdentical(outputFile, referenceFile));

    }
}
