/* OpenDA v2.4.1 
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
package org.openda.application;
import junit.framework.TestCase;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

public class ApplicationRunnerTest extends TestCase {
	
    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(ApplicationRunnerTest.class,"application");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public static void testDummy() {
        // No action. Test only exist to avoid warnings on empty test class when
        //            the test below is de-activated by renaming it to tst...()
    }

	public void testApplicationRunner() {
		System.out.println("==============================================================================");
		System.out.println("start and track the status of the thread");
		System.out.println("==============================================================================");

        ApplicationRunner app = new ApplicationRunner(testRunDataDir,"oscillatorDudOpenDaConfig.xml",true);
        while((app.getStatus()== ApplicationRunner.Status.INITIALIZED)
        		|(app.getStatus()==ApplicationRunner.Status.INITIALIZING)
        		|(app.getStatus()==ApplicationRunner.Status.RUNNING)){
			// show status
			System.out.println("thread status is "+app.getStatus());
			try{Thread.sleep(1000);}catch(InterruptedException e){/*do nothing*/}
		}
		ApplicationRunner.Status exitStatus = app.getStatus();
		System.out.println("exitStatus ="+exitStatus);
		System.out.println("Should be exitStatus = "+ApplicationRunner.Status.FINISHED);
		assertEquals("exitStatus",ApplicationRunner.Status.FINISHED,exitStatus);
	}

	public void testApplicationRunnerSingleThreaded() {
		System.out.println("==============================================================================");
		System.out.println("start and track the status of the thread");
		System.out.println("==============================================================================");

		ApplicationRunnerSingleThreaded runnerSingleThreaded =
				new ApplicationRunnerSingleThreaded();
		runnerSingleThreaded.initialize(testRunDataDir, "oscillatorDudOpenDaConfig.xml");
		runnerSingleThreaded.runSingleThreaded();
	}

	public void testApplicationRunnerWithControl() {
		System.out.println("==============================================================================");
		System.out.println("start and control the thread");
		System.out.println("==============================================================================");

        ApplicationRunner app = new ApplicationRunner(testRunDataDir,"oscillatorEnkfOpenDaConfig.xml",true);
		for(int i=0;i<2;i++){ // let it run for some seconds
            System.out.println("thread status is "+app.getStatus());
			try{Thread.sleep(150);}catch(InterruptedException e){/*do nothing*/}
		}
		// stop the thread
		System.out.println("Request pause for thread");
		app.pause();
		for(int i=0;i<4;i++){ // check status (PAUSED) for some seconds
            System.out.println("thread status is "+app.getStatus());
			try{Thread.sleep(500);}catch(InterruptedException e){/*do nothing*/}
		}
		// start it again the thread
		System.out.println("Request continue for thread");
		app.resume();
		try{Thread.sleep(200);}catch(InterruptedException e){/*do nothing*/}
		while(app.getStatus()==ApplicationRunner.Status.RUNNING){ // let it run until the end
            System.out.println("thread status is "+app.getStatus());
			try{Thread.sleep(500);}catch(InterruptedException e){/*do nothing*/}
		}
		ApplicationRunner.Status exitStatus = app.getStatus();
		System.out.println("exitStatus ="+exitStatus);
		System.out.println("Should be exitStatus = "+ApplicationRunner.Status.FINISHED);
		assertEquals("exitStatus",exitStatus,ApplicationRunner.Status.FINISHED);
	}

}
