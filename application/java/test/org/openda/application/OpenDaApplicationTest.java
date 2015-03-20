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

package org.openda.application;

import junit.framework.TestCase;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Tests for configured OpenDa applications
 */
public class OpenDaApplicationTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(OpenDaApplicationTest.class,"application");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testEnsrSimpleObsSimpleOscill() throws IOException {
    	String inputFile  = ( new File(testRunDataDir,"oscillatorEnkfOpenDaConfig.xml") ).getAbsolutePath();
    	System.out.println("path ="+inputFile);
    	String args[] = new String[1];
    	args[0] = inputFile;
        OpenDaApplication.main(args);
    }

    public void testEnsrOscillMultiResultWriters() throws IOException {
    	String inputFile  = ( new File(testRunDataDir,"oscillEnsrMultiRes_1.xml") ).getAbsolutePath();
    	System.out.println("path ="+inputFile);
    	String args[] = new String[1];
    	args[0] = inputFile;
        ApplicationRunner.setRunningInTest(true);
        OpenDaApplication.main(args);
    }

    public void testEnsrOscillGui() throws IOException {
    	String inputFile  = ( new File(testRunDataDir,"oscillEnsrMultiRes_1.xml") ).getAbsolutePath();
    	System.out.println("path ="+inputFile);
    	String args[] = new String[2];
    	args[0] = "-gui";
    	args[1] = inputFile;
        ApplicationRunner.setRunningInTest(true);
        OpenDaApplication.main(args);
        try{Thread.sleep(4000);}catch(InterruptedException e){/*do nothing*/}
    }

    public void testEmptyGui() throws IOException {
    	String args[] = {};
        OpenDaApplication.main(args);
        try{Thread.sleep(4000);}catch(InterruptedException e){/*do nothing*/}
    }
}
