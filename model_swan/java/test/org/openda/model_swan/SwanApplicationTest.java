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
import org.openda.application.ApplicationRunner;
import org.openda.application.OpenDaApplication;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 *
 */
public class SwanApplicationTest extends TestCase {
    private OpenDaTestSupport testData = null;
    private File testRunDataDir;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(SwanApplicationTest.class, "model_swan");
    	testRunDataDir = testData.getTestRunDataDir();
    }

    public void testSwanTriad() {
        //File applicationConfigDir = new File(testData.getTestRunDataDir(), "bb_test_1");
        //File applicationConfigDir = new File("d:\\sumihar\\Project\\OpenDA\\openda_1\\public\\tests\\swan_l21triad");
        File applicationConfigFile = new File(testRunDataDir, "l21triad"+File.separator+"swanDud.oda");
        ApplicationRunner.setRunningInTest(true);
        OpenDaApplication.main(new String[]{applicationConfigFile.getAbsolutePath()});

    }
}
