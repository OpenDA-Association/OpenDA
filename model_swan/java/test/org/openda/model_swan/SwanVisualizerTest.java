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
import org.openda.interfaces.IStochModelFactory;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Tests for Swan Model Factory and visualizer
 */
public class SwanVisualizerTest extends TestCase {

    private OpenDaTestSupport testData = null;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(SwanVisualizerTest.class, "model_swan");
    }

    public void testDummy() {
        // No action. Test only exist to avoid warnings on empty test class when
        //            the test below is de-activated by renaming it to tst...()
    }

    public void tstVisualizerConfig() {

        // Create stoch model factory
//        File stochModelConfigDir = new File(testData.getTestRunDataDir(), "l21triad/swanModel/config");
//        if (!stochModelConfigDir.exists()) {
//            throw new RuntimeException("stoch model config dir does not exists: " + stochModelConfigDir);
//        }
//
//        IStochModelFactory stochModelFactory = new DaModelFactory();
//        stochModelFactory.initialize(stochModelConfigDir, new String[]{"openDaStochModelWithVis.xml"});
//
//        final int runCount = 5;
//        for (int i = 0; i < runCount; i++) {
//            // TODO: get instance id's/object from observer?
//            File instanceDir = new File(stochModelConfigDir.getParentFile(), "work"+String.valueOf(i));
//            stochModelFactory.getPostprocessorInstance(instanceDir);
//        }
    }
}
