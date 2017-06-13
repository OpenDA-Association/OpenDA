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


package org.openda.costa;

import junit.framework.TestCase;
import org.openda.application.ApplicationRunner;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.performance.OdaTiming;

import java.io.File;
import java.io.IOException;

/**
 * Appilication tests with native oscillator model
 */
public class CostaOpendaAppTest extends TestCase {

    private File testRunDataDir;

    protected void setUp() throws IOException {
        OpenDaTestSupport testData = new OpenDaTestSupport(CostaOpendaAppTest.class, "application");
        testRunDataDir = testData.getTestRunDataDir();
    }

	public static void testDummy() {
		// No action. Test only exist to avoid warnings on empty test class when
		//            the test below is de-activated by renaming it to tst...()
	}

	public void testOscill_EnKF() throws IOException {
        ApplicationRunner.setRunningInTest(true);
        File config = new File(testRunDataDir, "OscillEnKFOpenDaConfig.xml");
        String args[] = new String[1];
        args[0] = config.getAbsolutePath();
        org.openda.application.OpenDaApplication.main(args);
        System.out.println("testOscill_EnKF DONE");
    }

    public void tstOscill_Simplex() throws IOException {
        ApplicationRunner.setRunningInTest(true);
        File config = new File(testRunDataDir, "OscillSimplexOpenDaConfig.xml");
        String args[] = new String[1];
        args[0] = config.getAbsolutePath();
        org.openda.application.OpenDaApplication.main(args);
    }

    // Test with a java observer

    public void tstOscill_EnKF2() throws IOException {
        ApplicationRunner.setRunningInTest(true);
        File config = new File(testRunDataDir, "OscillEnKFOpenDaConfig_javaobs.xml");
        String args[] = new String[1];
        args[0] = config.getAbsolutePath();
        // TODO fails on announce obs. now.
        org.openda.application.OpenDaApplication.main(args);
    }


}
