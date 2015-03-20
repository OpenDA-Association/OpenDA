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
import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.IStochObserver;
import org.openda.interfaces.IVector;
import org.openda.observers.NoosTimeSeriesStochObserver;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Test for the optional stopping criteria for optimization algorithms: absolute average of residual at each location.
 */
public class AbsAvrgStopCriterionTest extends TestCase {
    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
        testData = new OpenDaTestSupport(AbsAvrgStopCriterionTest.class,"algorithms");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testAbsAvrgStopCriterion_Overall(){
        IStochObserver obs1 = new NoosTimeSeriesStochObserver();
        obs1.initialize(testRunDataDir,new String[]{"noosObservations.xml"});

        IVector residual = obs1.getExpectations();
        IVector iParameter = null;
        double epsilon=1;
        double cost = 0.0d;

        AbsoluteAveragePerLocationStopCriterion stopCriterion = new AbsoluteAveragePerLocationStopCriterion();
        boolean isStop = stopCriterion.checkForStop(iParameter,residual,cost,epsilon);
        assertEquals("isStop: ",true,isStop);

        System.out.println("with epsilon="+epsilon+", "+stopCriterion.toString());
        epsilon = .1;
        isStop = stopCriterion.checkForStop(iParameter,residual,cost,epsilon);
        assertEquals("isStop: ",false,isStop);
        System.out.println("with epsilon="+epsilon+", "+stopCriterion.toString());
    }

    public void testAbsAvrgStopCriterion_Grouped(){
        IStochObserver obs1 = new NoosTimeSeriesStochObserver();
        obs1.initialize(testRunDataDir,new String[]{"noosObservations.xml"});
        IObservationDescriptions obsdescr = obs1.getObservationDescriptions();
        IVector residual = obs1.getExpectations();
        IVector iParameter = null;
        double epsilon=.5;
        double cost = 0.0d;

        AbsoluteAveragePerLocationStopCriterion stopCriterion = new AbsoluteAveragePerLocationStopCriterion();
        boolean isStop = stopCriterion.checkForStop(iParameter,residual,obsdescr,cost,epsilon);
        assertEquals("isStop: ",false,isStop);
        assertEquals("message: ",true,stopCriterion.toString().contains("satisfied at  den helder.waterlevel"));
        assertEquals("message: ",true,stopCriterion.toString().contains("not satisfied at  aberdeen.waterlevel"));
        System.out.println("with epsilon="+epsilon+", "+stopCriterion.toString());
    }

}
