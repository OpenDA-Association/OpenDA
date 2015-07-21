/* V2.2
 * Copyright (c) 2015 OpenDA Association
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
package org.openda.model_RainfallRunoffZhang;

import junit.framework.TestCase;

import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.*;
import org.openda.observers.DischargeDependentSelector;
import org.openda.utils.OpenDaTestSupport;
//import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

public class ASCIITimeSeriesStochObserverTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;
	
	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(ASCIITimeSeriesStochObserverTest.class,"model_RainfallRunoffZhang");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testStochObserver_1() {
		System.out.println("=========================================================");
		//System.out.println("" + extendedWorkingDirString + "");
		
		IStochObserver obs1 = new ASCIITimeSeriesStochObserver();
		obs1.initialize(testRunDataDir, new String[]{"ASCIIObservations1.xml"});

		String obs1String = obs1.toString();
		System.out.println(obs1String);
   		String reference ="StochObserver(\n" +
				"TimeSeries(\n" +
				"   Location = Catchment\n" +
				"   Position = (2.5,20.9)\n" +
				"   Height   = NaN\n" +
				"   Quantity = discharge\n" +
				"   Unit     = mm\n" +
				"   Source   = observed\n" +
				"   Id       = Catchment.discharge\n" +
				"   relativestandarddeviation  = 0.0\n" +
				"   timezone  = GMT\n" +
				"   analtime  = most recent\n" +
				"   standarddeviation  = 0.05\n" +
				"   status  = assimilation\n" +
				"   Values   = \n" +
				"   (25999.0=193001230000,NaN)\n" +
				"   (26000.0=193001240000,0.8)\n" +
				"   (26001.0=193001250000,0.962206)\n" +
				"   (26435.0=193104040000,NaN)\n" +
				"   \n" +
				"   Values.length()=4\n" +
				")\n" +
				");";
   		System.out.println(reference);

		String[] obs1String_parts = obs1String.split("\\n");
		String[] reference_parts = reference.split("\\n");
		Arrays.sort(obs1String_parts);
		Arrays.sort(reference_parts);
		
		int size = obs1String_parts.length;
		String[] obs1String_trimmed = new String[size];
		String[] reference_trimmed = new String[size];
		boolean testResult = false;
		for(int s = 0 ; s<size ; s++) {
			obs1String_trimmed[s] = obs1String_parts[s].trim();
			reference_trimmed[s] = reference_parts[s].trim();
			reference_trimmed[s] = reference_trimmed[s].replace("(", "\\(");
			reference_trimmed[s] = reference_trimmed[s].replace(")", "\\)");
			
			//System.out.println(obs1String_trimmed[s] + " : " + reference_trimmed[s]);
			testResult = obs1String_trimmed[s].matches(reference_trimmed[s]);
			//System.out.println(testResult);
			assertTrue(testResult);
		}

	}

	/*
	public void testStochObserver_2() {
		System.out.println("=========================================================");
		System.out.println("StochObserver selections");
		System.out.println("=========================================================");

		IStochObserver obs1 = new ASCIITimeSeriesStochObserver();
		obs1.initialize(testRunDataDir, new String[]{"noosSmallObs.xml"});

		String obs1String = obs1.toString();
		String reference = "StochObserver(\n" +
				"TimeSeries(\n" +
				"   Location = location1\n" +
				"   Position = (-2.0,53.3)\n" +
				"   Height   = NaN\n" +
				"   Quantity = pressure\n" +
				"   Unit     = m\n" +
				"   Source   = observed\n" +
				"   Id       = pressure@location-one\n" +
				"   relativestandarddeviation  = 0.0\n" +
				"   timezone  = GMT\n" +
				"   analtime  = most recent\n" +
				"   standarddeviation  = 0.05\n" +
				"   status  = assimilation\n" +
				"   Values   = \n" +
				"   (54466.020833333.*?=200801010.*?,-0.95)\n" +
				"   (54466.041666666.*?=200801010.*?,-0.99)\n" +
				"   (54466.0625=200801010.*?,-0.97)\n" +
				"\n" +
				"   Values.length()=3\n" +
				")\n" +
				"TimeSeries(\n" +
				"   Location = location2\n" +
				"   Position = (-2.0,43.3)\n" +
				"   Height   = NaN\n" +
				"   Quantity = cloud-fraction\n" +
				"   Unit     = m\n" +
				"   Source   = observed\n" +
				"   Id       = location2.cloud-fraction\n" +
				"   relativestandarddeviation  = 0.0\n" +
				"   timezone  = GMT\n" +
				"   analtime  = most recent\n" +
				"   standarddeviation  = 0.052\n" +
				"   status  = assimilation\n" +
				"   Values   = \n" +
				"   (54466.0=200801010000,-1.0)\n" +
				"   (54466.020833333.*?=200801010.*?,0.0)\n" +
				"   (54466.083333333.*?=200801010.*?,1.0)\n" +
				"\n" +
				"   Values.length()=3\n" +
				")\n" +
				"TimeSeries(\n" +
				"   Location = location1\n" +
				"   Position = (-2.0,53.3)\n" +
				"   Height   = NaN\n" +
				"   Quantity = pressure\n" +
				"   Unit     = m\n" +
				"   Source   = observed\n" +
				"   Id       = pressure2@location-one\n" +
				"   relativestandarddeviation  = 0.0\n" +
				"   timezone  = GMT\n" +
				"   analtime  = most recent\n" +
				"   standarddeviation  = 0.05\n" +
				"   status  = assimilation\n" +
				"   Values   = \n" +
				"()\n" +
				")\n" +
				");";

		String[] obs1String_parts = obs1String.split("\\n");
		String[] reference_parts = reference.split("\\n");
		Arrays.sort(obs1String_parts);
		Arrays.sort(reference_parts);

		StringBuilder obs1String_builder = new StringBuilder();
		for(String s : obs1String_parts) {
			obs1String_builder.append(s);
		}
		String obs1String_sorted =  obs1String_builder.toString();

		StringBuilder reference_builder = new StringBuilder();
		for(String s : reference_parts) {
			reference_builder.append(s);
		}
		String reference_sorted =  reference_builder.toString();

		obs1String_sorted = obs1String_sorted.replace("\\n", "\\\\n");
		reference_sorted = reference_sorted.replace("\\n", "\\\\n");
		reference_sorted = reference_sorted.replace("(", "\\(");
		reference_sorted = reference_sorted.replace(")", "\\)");

		boolean isOK=obs1String_sorted.matches(reference_sorted);
		assertTrue(isOK);


		// selection by time and values
		IVector mean = obs1.getExpectations();
		System.out.println("obs1.getExpectations()="+mean);
		System.out.println("Should be obs1.getExpectations()=[-0.95,-0.99,-0.97,-1.0,0.0,1.0]");
		assertEquals("obs1.getExpectations()","[-0.95,-0.99,-0.97,-1.0,0.0,1.0]",mean.toString());

		// overrule id
		IObservationDescriptions descr1 = obs1.getObservationDescriptions();
		String[] ids = descr1.getStringProperties("id");
		int m = ids.length-1;
		System.out.println("id[end]="+ids[m]);
		System.out.println("id[end]=location2.cloud-fraction");
		assertEquals("id[end]","location2.cloud-fraction",ids[m]);

	}

    public void testStochObserver_3(){
        System.out.println("=========================================================");
        System.out.println("StochObserver selections based of type/status");
        System.out.println("=========================================================");

        IStochObserver obs1 = new ASCIITimeSeriesStochObserver();
        obs1.initialize(testRunDataDir, new String[]{"noosObservations.xml"});

        IStochObserver assimObs = obs1.createSelection(IStochObserver.Type.Assimilation);
        IVector assim = assimObs.getExpectations();
        assertEquals("Data for assimilation: ","[0.73,0.73,0.72,0.7,0.68,0.66,0.63,0.59,0.55,0.51,0.46,0.4,0.35,0.29,0.23,0.17,0.12,0.06,0.0]",assim.toString());

        IStochObserver validObs = obs1.createSelection(IStochObserver.Type.Validation);
        IVector valid = validObs.getExpectations();
        assertEquals("Data for validation: ","[-0.83,-0.88,-0.91,-0.95,-0.97,-0.98,-0.99,-0.99,-0.98,-0.97,-0.95,-0.92,-0.89,-0.85,-0.81,-0.77,-0.71,-0.66,-0.6]",valid.toString());
    }

    public void testStochObserver_4(){
        System.out.println("=============================================================");
        System.out.println("StochObserver selections based of type/status using ISelector");
        System.out.println("=============================================================");

        IStochObserver obs1 = new ASCIITimeSeriesStochObserver();
        obs1.initialize(testRunDataDir, new String[]{"noosObservations.xml"});

        IVector fakeValuesFromModel = obs1.getExpectations();

        ISelector assimSelector = obs1.createSelector(IStochObserver.Type.Assimilation);
        IVector obsAssim = assimSelector.apply(fakeValuesFromModel);
        assertEquals("Data for assimilation: ","[0.73,0.73,0.72,0.7,0.68,0.66,0.63,0.59,0.55,0.51,0.46,0.4,0.35,0.29,0.23,0.17,0.12,0.06,0.0]",obsAssim.toString());

        // check if both ways of creating a selection lead to the same result
        IStochObserver assimStochObserver = obs1.createSelection(IStochObserver.Type.Assimilation);
        IVector expectationsFromObserverSelection = assimStochObserver.getExpectations();
        for (int i=0; i < expectationsFromObserverSelection.getSize();i++) {
            assertEquals(expectationsFromObserverSelection.getValue(i), obsAssim.getValue(i) );
        }

        ISelector validSelector = obs1.createSelector(IStochObserver.Type.Validation);
        IVector obsValid = validSelector.apply(fakeValuesFromModel);
        assertEquals("Data for validation: ","[-0.83,-0.88,-0.91,-0.95,-0.97,-0.98,-0.99,-0.99,-0.98,-0.97,-0.95,-0.92,-0.89,-0.85,-0.81,-0.77,-0.71,-0.66,-0.6]",obsValid.toString());
    }

    public void testStochObserver_5() {
        System.out.println("=============================================================");
        System.out.println("StochObserver selections based of max/min values using ISelector");
        System.out.println("=============================================================");

        ASCIITimeSeriesStochObserver obs1 = new ASCIITimeSeriesStochObserver();
        obs1.initialize(testRunDataDir, new String[]{"noosObservationsWithDischarge.xml"});

        IVector fakeValuesFromStochModel = obs1.getExpectations();

        double minValue = 60.0;
        double maxValue = 300.0;

        DischargeDependentSelector obsSelector = (DischargeDependentSelector) obs1.createSelector(fakeValuesFromStochModel,minValue,maxValue);
        IVector fromSelector = obsSelector.apply(fakeValuesFromStochModel);
        assertEquals("All values fromSelector: ","[0.29,0.23,0.17,0.12,0.06,290.0,230.0,170.0,120.0,60.0]",fromSelector.toString());

        assertEquals("All stations without selection: ","[0.73,0.73,0.72,0.7,0.68,0.66,0.63,0.59,0.55,0.51,...,510.0,460.0,400.0,350.0,290.0,230.0,170.0,120.0,60.0,0.0]",obs1.getExpectations().toString());
        IStochObserver stochObsAssim1 = obs1.createSelection(IStochObserver.Type.Assimilation);
        assertEquals("Assimilation station without disch-dependent selection: ","[0.73,0.73,0.72,0.7,0.68,0.66,0.63,0.59,0.55,0.51,0.46,0.4,0.35,0.29,0.23,0.17,0.12,0.06,0.0]",stochObsAssim1.getExpectations().toString());
        IStochObserver stochObsValid1 = obs1.createSelection(IStochObserver.Type.Validation);
        assertEquals("Validation station without disch-dependent selection: ","[730.0,730.0,720.0,700.0,680.0,660.0,630.0,590.0,550.0,510.0,460.0,400.0,350.0,290.0,230.0,170.0,120.0,60.0,0.0]",stochObsValid1.getExpectations().toString());
        IStochObserver stochObs = obs1.createSelection(fakeValuesFromStochModel,minValue,maxValue);
        assertEquals("All stations with disch-dependent selection: ","[0.29,0.23,0.17,0.12,0.06,290.0,230.0,170.0,120.0,60.0]",stochObs.getExpectations().toString());
        IStochObserver stochObsAssim = stochObs.createSelection(IStochObserver.Type.Assimilation);
        assertEquals("Assimilation station with disch-dependent selection: ","[0.29,0.23,0.17,0.12,0.06]",stochObsAssim.getExpectations().toString());
        IStochObserver stochObsValid = stochObs.createSelection(IStochObserver.Type.Validation);
        assertEquals("Validation station with disch-dependent selection: ","[290.0,230.0,170.0,120.0,60.0]",stochObsValid.getExpectations().toString());
    }
    */

	/*
	public void testStochObserver_missingData() {
		System.out.println("=========================================================");
		System.out.println("StochObserver missing data");
		System.out.println("=========================================================");
		double delta=0.0001;

		IStochObserver obs1 = new ASCIITimeSeriesStochObserver();
		obs1.initialize(testRunDataDir, new String[]{"noosSmallObs.xml"});

		// remove some data
		TimeSeries series1=((ASCIITimeSeriesStochObserver)obs1).series[1];
		//TimeSeries(
		//   Location = location2
		//   Position = (-2.0,43.3)
		//   Height   = NaN
		// ...
		//   standarddeviation  = 0.052
		//   Values   =
		//   (54466.0=200801010000,-1.0)
		//   (54466.020833333336=200801010030,0.0)
		//   (54466.083333333336=200801010200,1.0)
		series1=series1.selectValueSubset(-10.0, -1.0);
		((ASCIITimeSeriesStochObserver)obs1).series[1]= series1;

		// selection by time and values
		IStochObserver obs2 = obs1.createSelection(new Time(54466.010, 54466.030));
		String obs2String = obs2.toString();
        String reference="StochObserver(\n" +
				"TimeSeries(\n" +
				"   Location = location1\n" +
				"   Position = (-2.0,53.3)\n" +
				"   Height   = NaN\n" +
				"   Quantity = pressure\n" +
				"   Unit     = m\n" +
				"   Source   = observed\n" +
				"   Id       = pressure@location-one\n" +
				"   relativestandarddeviation  = 0.0\n" +
				"   timezone  = GMT\n" +
				"   analtime  = most recent\n" +
				"   standarddeviation  = 0.05\n" +
				"   status  = assimilation\n" +
				"   Values   = \n" +
				"   (54466.020833333.*?=200801010.*?,-0.95)\n" +
				"\n" +
				"   Values.length()=1\n" +
				")\n" +
				");";

		String[] obs2String_parts = obs2String.split("\\n");
		String[] reference_parts = reference.split("\\n");
		Arrays.sort(obs2String_parts);
		Arrays.sort(reference_parts);

		StringBuilder obs2String_builder = new StringBuilder();
		for(String s : obs2String_parts) {
			obs2String_builder.append(s);
		}
		String obs2String_sorted =  obs2String_builder.toString();

		StringBuilder reference_builder = new StringBuilder();
		for(String s : reference_parts) {
			reference_builder.append(s);
		}
		String reference_sorted =  reference_builder.toString();

		obs2String_sorted = obs2String_sorted.replace("\\n", "\\\\n");
		reference_sorted = reference_sorted.replace("\\n", "\\\\n");
		reference_sorted = reference_sorted.replace("(", "\\(");
		reference_sorted = reference_sorted.replace(")", "\\)");

		boolean isOK=obs2String_sorted.matches(reference_sorted);
		assertTrue(isOK);

		ITime[] times=obs2.getTimes();
		System.out.println("Number of times ="+times.length);
		assertEquals(1, times.length);

		IVector std = obs2.getStandardDeviations();
		System.out.println("std="+std.toString());
		System.out.println("Should be std=[0.05]");
		assertEquals("[0.05]",std.toString());

		IVector vals = obs2.getValues();
		System.out.println("vals="+vals.toString());
		System.out.println("Should be vals=[-0.95]");
		assertEquals("[-0.95]",vals.toString());

	}
	*/

	/*
	public void testStochObserver_noosObserver6() {
		File workingDir = new File(testRunDataDir, "noosObserver6");

		IStochObserver noosStochObserver_a = new ASCIITimeSeriesStochObserver();
		noosStochObserver_a.initialize(workingDir, new String[]{"noosStochObsConfig_a.xml"});
		List<IPrevExchangeItem> exchangeItems_a = noosStochObserver_a.getObservationDescriptions().getExchangeItems();
		assertEquals("First Series", "location 1 Measurement", exchangeItems_a.get(0).getId());
		assertEquals("First Series", "location 2 Measurement", exchangeItems_a.get(1).getId());

		IStochObserver noosStochObserver_b = new ASCIITimeSeriesStochObserver();
		noosStochObserver_b.initialize(workingDir, new String[]{"noosStochObsConfig_b.xml"});
		List<IPrevExchangeItem> exchangeItems_b = noosStochObserver_b.getObservationDescriptions().getExchangeItems();
		assertEquals("First Series", "ObsPoint1.waterlevel", exchangeItems_b.get(0).getId());
		assertEquals("First Series", "ObsPoint2.waterlevel", exchangeItems_b.get(1).getId());

		double[] times = exchangeItems_b.get(0).getTimes();
		double startTime = times[0];
		double endTime = times[times.length-1];
		ITime timeSpan = new Time(startTime, endTime);
		IStochObserver selection = noosStochObserver_b.createSelection(timeSpan);
		List<IPrevExchangeItem> exchangeItems_c = selection.getObservationDescriptions().getExchangeItems();
		assertEquals("First Series", "ObsPoint1.waterlevel", exchangeItems_c.get(0).getId());
		assertEquals("First Series", "ObsPoint2.waterlevel", exchangeItems_c.get(1).getId());
	}
	
	public void testStochObserver_RelativeErrors() {
		System.out.println("=========================================================");
		double delta=0.0001;

		IStochObserver obs1 = new ASCIITimeSeriesStochObserver();
		obs1.initialize(testRunDataDir, new String[]{"noosObsRelativeErrors.xml"});


		// test properties from file and overruled properties
		IVector mean = obs1.getExpectations();
		System.out.println("obs1.getExpectations()="+mean);
		System.out.println("Should be obs1.getExpectations()=[0.73,0.73,0.72,0.7,0.68,0.66,0.63,0.59,0.55,0.51,...,-0.97,-0.95,-0.92,-0.89,-0.85,-0.81,-0.77,-0.71,-0.66,-0.6]");
		assertEquals("obs1.getExpectations()","[0.73,0.73,0.72,0.7,0.68,0.66,0.63,0.59,0.55,0.51,...,-0.97,-0.95,-0.92,-0.89,-0.85,-0.81,-0.77,-0.71,-0.66,-0.6]",mean.toString());
		IVector std  = obs1.getStandardDeviations();
		System.out.println("obs1.getStandardDeviations()="+std);
		System.out.println("Should be obs1.getStandardDeviations()=[0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,...,0.052,0.052,0.052,0.052,0.052,0.052,0.052,0.052,0.052,0.052]");
		assertEquals("obs1.getStandardDeviations()","[0.273,0.273,0.272,0.27,0.268,0.266,0.263,0.259,0.255,0.251,...,0.097,0.095,0.09200000000000001,0.08900000000000001,0.085,0.08100000000000002,0.07700000000000001,0.071,0.066,0.06]",std.toString());
		int n = obs1.getCount();
		System.out.println("obs1.getCount()="+n);
		System.out.println("Should be obs1.getCount()=38");
		assertEquals("obs1.getCount()",38,n);

	}
	*/

}//end test-class


