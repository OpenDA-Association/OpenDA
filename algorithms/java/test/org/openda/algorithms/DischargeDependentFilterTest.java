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
import java.util.ArrayList;
import java.util.List;

/**
 * Test of DischargeDependentFilter for selecting waterlevel observed data based on a user defined discharge range.
 */
public class DischargeDependentFilterTest extends TestCase {
    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
       	testData = new OpenDaTestSupport(CalibrationTest.class,"algorithms");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testDischargeDependentFilter_1(){

        NoosTimeSeriesStochObserver stochObserver = new NoosTimeSeriesStochObserver();
        stochObserver.initialize(testRunDataDir,new String[]{"noosObservationsWithDischarge.xml"});
        IVector fakeValuesFromModel = stochObserver.getExpectations();
        fakeValuesFromModel.scale(1.01);

        ObservationSpace obsSpace = new ObservationSpace();
        obsSpace.observer = stochObserver;
        obsSpace.predictedValues = fakeValuesFromModel;
        System.out.println("======================================================================");
        System.out.println("Original observed data: "+obsSpace.observer.getExpectations().toString());
        System.out.println("Original predicted data: "+obsSpace.predictedValues.toString());
        assertEquals("Original observed data: ","[0.73,0.73,0.72,0.7,0.68,0.66,0.63,0.59,0.55,0.51,...,510.0,460.0,400.0,350.0,290.0,230.0,170.0,120.0,60.0,0.0]",obsSpace.observer.getExpectations().toString());
        assertEquals("Original predicted data: ","[0.7373,0.7373,0.7272,0.707,0.6868000000000001,0.6666000000000001,0.6363,0.5959,0.5555000000000001,0.5151,...,515.1,464.6,404.0,353.5,292.9,232.3,171.7,121.2,60.6,0.0]",obsSpace.predictedValues.toString());

        DischargeDependentFilter dischDepFilter = new DischargeDependentFilter();
        dischDepFilter.initialize(testRunDataDir,new String[]{"dischargeDependentObservationFilter.xml", "ddf-log.csv"});
        ObservationSpace dischDepSpace = dischDepFilter.applyFilter(obsSpace);
//        assertEquals("Applying filter does not change the original: ","[0.73,0.73,0.72,0.7,0.68,0.66,0.63,0.59,0.55,0.51,...,510.0,460.0,400.0,350.0,290.0,230.0,170.0,120.0,60.0,0.0]",obsSpace.observer.getExpectations().toString());
        System.out.println("======================================================================");
        System.out.println("Applying discharge-dependent filter on observed data replaces also the validation observed data with the predicted one.");
        System.out.println("Discharge-dependent filtered observed data: "+dischDepSpace.observer.getExpectations().toString());
        System.out.println("Discharge-dependent filtered predicted data: "+dischDepSpace.predictedValues.toString());
        assertEquals("Discharge-dependent filtered observed data: ","[0.29,0.23,0.17,0.12,0.06,292.9,232.3,171.7,121.2,60.6,...,0.59,0.55,0.51,0.46,0.4,595.9,555.5,515.1,464.6,404.0]",dischDepSpace.observer.getExpectations().toString());
        assertEquals("Discharge-dependent filtered predicted data: ","[0.2929,0.2323,0.17170000000000002,0.1212,0.0606,292.9,232.3,171.7,121.2,60.6,...,0.5959,0.5555000000000001,0.5151,0.4646,0.404,595.9,555.5,515.1,464.6,404.0]",dischDepSpace.predictedValues.toString());

        AssimilationObservationFilter assimObsFilter = new AssimilationObservationFilter();
        ObservationSpace assimObsSpace = assimObsFilter.applyFilter(dischDepSpace);
        System.out.println("======================================================================");
        System.out.println("Assim-obs filtered observed data: "+assimObsSpace.observer.getExpectations().toString());
        System.out.println("Assim-obs filtered predicted data: "+assimObsSpace.predictedValues.toString());
        assertEquals("Assim-obs filtered observed data: ","[0.29,0.23,0.17,0.12,0.06,0.59,0.55,0.51,0.46,0.4]",assimObsSpace.observer.getExpectations().toString());
        assertEquals("Assim-obs filtered predicted data: ","[0.2929,0.2323,0.17170000000000002,0.1212,0.0606,0.5959,0.5555000000000001,0.5151,0.4646,0.404]",assimObsSpace.predictedValues.toString());

    }

    public void testDischargeDependentFilter_2(){

        NoosTimeSeriesStochObserver stochObserver = new NoosTimeSeriesStochObserver();
        stochObserver.initialize(testRunDataDir,new String[]{"noosObservationsWithDischarge.xml"});
        IVector fakeValuesFromModel = stochObserver.getExpectations();
        fakeValuesFromModel.scale(1.01);
        System.out.println("Number of obs data: "+fakeValuesFromModel.getSize());

        ObservationSpace obsSpace = new ObservationSpace();
        obsSpace.observer = stochObserver;
        obsSpace.predictedValues = fakeValuesFromModel;
        System.out.println("======================================================================");
        System.out.println("Original observed data: "+obsSpace.observer.getExpectations().toString());
        System.out.println("Original predicted data: "+obsSpace.predictedValues.toString());

        DischargeDependentFilter dischDepFilter = new DischargeDependentFilter();
        dischDepFilter.initialize(testRunDataDir,new String[]{"dischargeDependentObservationFilter.xml"});
        AssimilationObservationFilter assimObsFilter = new AssimilationObservationFilter();
        List<IObservationSpaceFilter> obsSpaceFilters = new ArrayList<IObservationSpaceFilter>();
        obsSpaceFilters.add(0,dischDepFilter);
        obsSpaceFilters.add(1,assimObsFilter);

        for (IObservationSpaceFilter obsSpaceFilter : obsSpaceFilters) {
            obsSpace = obsSpaceFilter.applyFilter(obsSpace);
        }

        System.out.println("Disch-dependent and Assim-obs filtered observed data: "+obsSpace.observer.getExpectations().toString());
        System.out.println("Disch-dependent and Assim-obs filtered predicted data: "+obsSpace.predictedValues.toString());
        assertEquals("Disch-dependent and Assim-obs filtered observed data: ","[0.29,0.23,0.17,0.12,0.06,0.59,0.55,0.51,0.46,0.4]",obsSpace.observer.getExpectations().toString());
        assertEquals("Disch-dependent and Assim-obs filtered predicted data: ","[0.2929,0.2323,0.17170000000000002,0.1212,0.0606,0.5959,0.5555000000000001,0.5151,0.4646,0.404]",obsSpace.predictedValues.toString());

    }

    public void testDischargeDependentFilter_3(){

        NoosTimeSeriesStochObserver stochObserver = new NoosTimeSeriesStochObserver();
        stochObserver.initialize(testRunDataDir,new String[]{"noosObservationsWithDischarge2.xml"});
        IVector fakeValuesFromModel = stochObserver.getExpectations();
        fakeValuesFromModel.scale(1.01);

        ObservationSpace obsSpace = new ObservationSpace();
        obsSpace.observer = stochObserver;
        obsSpace.predictedValues = fakeValuesFromModel;

        DischargeDependentFilter dischDepFilter = new DischargeDependentFilter();
        dischDepFilter.initialize(testRunDataDir,new String[]{"dischargeDependentObservationFilter2.xml"});
        AssimilationObservationFilter assimObsFilter = new AssimilationObservationFilter();
        List<IObservationSpaceFilter> obsSpaceFilters = new ArrayList<IObservationSpaceFilter>();
        obsSpaceFilters.add(0,dischDepFilter);
        obsSpaceFilters.add(1,assimObsFilter);

        for (IObservationSpaceFilter obsSpaceFilter : obsSpaceFilters) {
            try {
                obsSpace = obsSpaceFilter.applyFilter(obsSpace);
            } catch (Exception e){
                assertEquals("Error message: ","DischargeDependentFilter: no data on sheerness.waterlevel stations-pair is found to correspond to the specified discharge range. Please check your DischargeDependentFilter configuration file.",e.getMessage());
                System.out.println("ERROR is found in the obsSpaceFilter with the correct error message.");
            }
        }
    }

    public void testDischargeDependentFilter_4(){
        // test the case of when stations in dischargeDepFilter.xml input file do not match the ones in
        // stochObserver.xml input file.

        NoosTimeSeriesStochObserver stochObserver = new NoosTimeSeriesStochObserver();
        stochObserver.initialize(testRunDataDir,new String[]{"noosObservationsWithDischarge3.xml"});
        IVector fakeValuesFromModel = stochObserver.getExpectations();
        fakeValuesFromModel.scale(1.01);

        ObservationSpace obsSpace = new ObservationSpace();
        obsSpace.observer = stochObserver;
        obsSpace.predictedValues = fakeValuesFromModel;

        DischargeDependentFilter dischDepFilter = new DischargeDependentFilter();
        dischDepFilter.initialize(testRunDataDir,new String[]{"dischargeDependentObservationFilter3.xml"});
        AssimilationObservationFilter assimObsFilter = new AssimilationObservationFilter();
        List<IObservationSpaceFilter> obsSpaceFilters = new ArrayList<IObservationSpaceFilter>();
        obsSpaceFilters.add(0,dischDepFilter);
        obsSpaceFilters.add(1,assimObsFilter);

        for (IObservationSpaceFilter obsSpaceFilter : obsSpaceFilters) {
            try {
                obsSpace = obsSpaceFilter.applyFilter(obsSpace);
            } catch (Exception e){
                assertEquals("Error message: ",
                		e.getMessage(),"Error(s) in discharge dependent filter / the stochObserver input: \nValidation station vliss.discharge has no assimilation counterpart");
                System.out.println("ERROR is found in the obsSpaceFilter with the correct error message.");
            }
        }
    }

}
