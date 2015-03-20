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
package org.openda.algorithms;
import junit.framework.TestCase;
import org.openda.application.ApplicationRunner;
import org.openda.application.OpenDaApplication;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

public class KalmanFilterApplicationTest extends TestCase {

    private File testRunDataDir;

    protected void setUp() throws IOException {
        OpenDaTestSupport testData = new OpenDaTestSupport(KalmanFilterApplicationTest.class, "application");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testEnsrOscill() throws IOException {
		   String fileName[] = new String[1];
		   fileName[0]= (new File(testRunDataDir,"oscillatorEnsrOpenDaConfig.xml")).getAbsolutePath();
           ApplicationRunner.setRunningInTest(true);
	       OpenDaApplication.main(fileName);
	   }

    public void testEnsrOscillRestart() throws IOException {
    	String fileName[] = new String[1];
    	fileName[0]= (new File(testRunDataDir,"ensr_write_restart.oda")).getAbsolutePath();
    	ApplicationRunner.setRunningInTest(true);
    	OpenDaApplication.main(fileName);
    	File restartFile = new File(testRunDataDir,"restart.zip"); //no time tagging
    	boolean restartExists = restartFile.exists();
    	assertTrue(restartExists);

    	fileName[0]= (new File(testRunDataDir,"ensr_read_restart.oda")).getAbsolutePath();
    	ApplicationRunner.setRunningInTest(true);
    	OpenDaApplication.main(fileName);
    	restartFile = new File(testRunDataDir,"ensr_read_empty_restart_results.m"); //no time tagging
    	restartExists = restartFile.exists();
    	assertTrue(restartExists);
    }

    
}
