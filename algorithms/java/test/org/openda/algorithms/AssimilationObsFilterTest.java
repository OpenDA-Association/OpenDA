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
import org.openda.interfaces.IVector;
import org.openda.observers.NoosTimeSeriesStochObserver;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;

/**
 * Test of observation selection based on type of observation (assimilation or validation). Assimilation station are
 * selected, while validation stations are filtered out.
 */
public class AssimilationObsFilterTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
       	testData = new OpenDaTestSupport(CalibrationTest.class,"algorithms");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testAssimilationObsFilter_1(){

        NoosTimeSeriesStochObserver stochObserver = new NoosTimeSeriesStochObserver();
        stochObserver.initialize(testRunDataDir,new String[]{"noosObservations.xml"});
        IVector fakeValuesFromModel = stochObserver.getExpectations();

        ObservationSpace obsSpace = new ObservationSpace();
        obsSpace.observer = stochObserver;
        obsSpace.predictedValues = fakeValuesFromModel;
        assertEquals("All observed data: ","[0.73,0.73,0.72,0.7,0.68,0.66,0.63,0.59,0.55,0.51,...,-0.97,-0.95,-0.92,-0.89,-0.85,-0.81,-0.77,-0.71,-0.66,-0.6]",
                obsSpace.observer.getExpectations().toString());
        assertEquals("All predicted data: ","[0.73,0.73,0.72,0.7,0.68,0.66,0.63,0.59,0.55,0.51,...,-0.97,-0.95,-0.92,-0.89,-0.85,-0.81,-0.77,-0.71,-0.66,-0.6]",
                obsSpace.predictedValues.toString());

        AssimilationObservationFilter assimFilter = new AssimilationObservationFilter();
        ObservationSpace assimSpace = assimFilter.applyFilter(obsSpace);
        assertEquals("Assimilation observed data: ","[0.73,0.73,0.72,0.7,0.68,0.66,0.63,0.59,0.55,0.51,0.46,0.4,0.35,0.29,0.23,0.17,0.12,0.06,0.0]",
                assimSpace.observer.getExpectations().toString());
        assertEquals("Assimilation predicted data: ","[0.73,0.73,0.72,0.7,0.68,0.66,0.63,0.59,0.55,0.51,0.46,0.4,0.35,0.29,0.23,0.17,0.12,0.06,0.0]",
                assimSpace.predictedValues.toString());

    }
}
