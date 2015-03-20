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


package org.openda.blackbox.wrapper;

import junit.framework.TestCase;
import org.openda.interfaces.IStochModelFactory;
import org.openda.interfaces.IStochModelInstance;
import org.openda.interfaces.ITime;
import org.openda.interfaces.IVector;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;
import org.openda.utils.Vector;

import java.io.File;
import java.io.IOException;

/**
 * Tests for black box noise model factory
 */
public class BBNoiseModelTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
        testData = new OpenDaTestSupport(BBNoiseModelTest.class, "core");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testNoiseModel() {

        BBStochModelFactory bbStochModelFactory = new BBStochModelFactory();
        File noiseModelDir = new File(testRunDataDir, "noiseModel");
        bbStochModelFactory.initialize(noiseModelDir, new String[]{"bbStochModelConfig.xml"});

        int instanceCount = 4;
        IStochModelInstance[] stochModelInstances = new IStochModelInstance[instanceCount];
        for (int i = 0; i < instanceCount; i++) {
            stochModelInstances[i] = bbStochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
            stochModelInstances[i].setAutomaticNoiseGeneration(true);
        }

        int timeStepCount = 5;
        ITime timeHorizon = stochModelInstances[0].getTimeHorizon();
        ITime startTime = timeHorizon.getBeginTime();
        double deltaTasMJD = 1d / 24d / 60d * 10d; // 10 minutes (= bc.-timeseries steps)
        for (int t = 1; t <= timeStepCount; t++) {
            ITime targetTime = new Time(startTime.getMJD() + t * deltaTasMJD);
            for (int i = 0; i < instanceCount; i++) {
                IVector state = stochModelInstances[i].getState();
                assertEquals("state size", 8, state.getSize());
                IVector constantVector = new Vector(state.getSize());
                constantVector.setConstant(0.01 * i);
                stochModelInstances[i].axpyOnState(1.0, constantVector);
                stochModelInstances[i].compute(targetTime);
            }
        }

        File work_1_noiseLog = new File(noiseModelDir, "work1/ArmaNoiseModel-log.txt");
        File work_1_noiseLog_1 = new File(noiseModelDir, "work1/ArmaNoiseModel-log-1.txt");
        File work_3_noiseLog_2 = new File(noiseModelDir, "work3/ArmaNoiseModel-log-2.txt");

        File work_1_noiseLog_ref = new File(noiseModelDir, "ref/work1-ArmaNoiseModel-log.txt");
        File work_1_noiseLog_1_ref = new File(noiseModelDir, "ref/work1-ArmaNoiseModel-log-1.txt");
        File work_3_noiseLog_2_ref = new File(noiseModelDir, "ref/work3-ArmaNoiseModel-log-2.txt");

        testData.FilesAreIdentical(work_1_noiseLog, work_1_noiseLog_ref);
        testData.FilesAreIdentical(work_1_noiseLog_1, work_1_noiseLog_1_ref);
        testData.FilesAreIdentical(work_3_noiseLog_2, work_3_noiseLog_2_ref);

    }
}
