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
import org.openda.interfaces.*;
import org.openda.observers.IoObjectStochObserver;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Tests for Swan Model Factory and model
 */
public class SwanCalibWrapperTest extends TestCase {

    private OpenDaTestSupport testData = null;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(SwanCalibWrapperTest.class, "model_swan");
    }

    public void testSwan_1() {

        // Create stoch observer instance
        File stochObserverConfigDir = new File(testData.getTestRunDataDir(), "l21triad/stochobserver");
        if (!stochObserverConfigDir.exists()) {
            throw new RuntimeException("stoch observer config dir does not exists: " + stochObserverConfigDir);
        }

        IStochObserver stochObserver = new IoObjectStochObserver();
        stochObserver.initialize(stochObserverConfigDir, new String[]{"swanStochObsConfig.xml"});
        IObservationDescriptions observationDescriptions = stochObserver.getObservationDescriptions();

        // Create stoch model factory
        File stochModelConfigDir = new File(testData.getTestRunDataDir(), "l21triad/swanModel/config");
        if (!stochModelConfigDir.exists()) {
            throw new RuntimeException("stoch model config dir does not exists: " + stochModelConfigDir);
        }

        IStochModelFactory stochModelFactory = new SwanCalibStochModelFactory();
        stochModelFactory.initialize(stochModelConfigDir, new String[]{"openDaStochModel.xml"});

        final int runCount = 5;
        for (int i = 0; i < runCount; i++) {

            // Create and stoch model instance
            IStochModelInstance instance = stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);

            // Adjust parameters
            IVector parameterVector = instance.getParameters();
            IStochVector stochParameterVector = instance.getParameterUncertainty();
            parameterVector.axpy(i * 0.1, stochParameterVector.getStandardDeviations());
            instance.setParameters(parameterVector);

            // compute and get values at observation points
            instance.announceObservedValues(observationDescriptions);
            instance.compute(instance.getCurrentTime());
            IVector observedValues = instance.getObservedValues(observationDescriptions);
            assertEquals("#observedValues", observedValues.getSize(), observationDescriptions.getObservationCount());

        }
    }

    public void testDudSwan() throws IOException {
        File applicationConfigFile = new File(testData.getTestRunDataDir(), "l21triadInOpenDa/swanDud.oda");
        ApplicationRunner.setRunningInTest(true);
        OpenDaApplication.main(new String[]{applicationConfigFile.getAbsolutePath()});
    }

    public void testDudSwanLogNormal() throws IOException {
        File applicationConfigFile = new File(testData.getTestRunDataDir(), "l21triadInOpenDa/swanDudLn.oda");
        ApplicationRunner.setRunningInTest(true);
        OpenDaApplication.main(new String[]{applicationConfigFile.getAbsolutePath()});
    }

    public void testDudSwanWithSwivtVis() throws IOException {
        File applicationConfigFile = new File(testData.getTestRunDataDir(), "l21triadInOpenDa/swanDud.oda");
        ApplicationRunner.setRunningInTest(true);
        OpenDaApplication.main(new String[]{applicationConfigFile.getAbsolutePath()});
    }
}
