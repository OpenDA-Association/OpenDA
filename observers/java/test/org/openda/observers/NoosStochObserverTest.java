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
package org.openda.observers;

import junit.framework.TestCase;

import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.*;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Time;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

public class NoosStochObserverTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(NoosStochObserverTest.class,"observers");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public void testStochObserver_1() {
		System.out.println("=========================================================");
		double delta=0.0001;

		IStochObserver obs1 = new NoosTimeSeriesStochObserver();
		obs1.initialize(testRunDataDir, new String[]{"noosObservations.xml"});

		String obs1String = obs1.toString();
   		String reference ="StochObserver(\n" +
				"TimeSeries(\n" +
				"   Location = den helder\n" +
				"   Position = (4.745356,52.966001)\n" +
				"   Height   = NaN\n" +
				"   Quantity = waterlevel_astro\n" +
				"   Unit     = m\n" +
				"   Source   = observed\n" +
				"   Id       = den helder.waterlevel_astro\n" +
				"   relativestandarddeviation  = 0.0\n" +
				"   timezone  = GMT\n" +
				"   analtime  = most recent\n" +
				"   standarddeviation  = 0.05\n" +
				"   status  = assimilation\n" +
				"   Values   = \n" +
				"   (54466.0=200801010.*?,0.73)\n" +
				"   (54466.006944444.*?=200801010.*?,0.73)\n" +
				"   (54466.01388888.*?=200801010.*?,0.72)\n" +
				"   (54466.020833333.*?=200801010.*?,0.7)\n" +
				"   (54466.02777777.*?=200801010.*?,0.68)\n" +
				"   ...\n" +
				"   (54466.09722222.*?=200801010.*?,0.23)\n" +
				"   (54466.104166666.*?=200801010.*?,0.17)\n" +
				"   (54466.11111111.*?=200801010.*?,0.12)\n" +
				"   (54466.118055555.*?=200801010.*?,0.06)\n" +
				"   (54466.125=200801010.*?,0.0)\n" +
				"\n" +
				"   Values.length()=19\n" +
				")\n" +
				"TimeSeries(\n" +
				"   Location = aberdeen2\n" +
				"   Position = (10.0,11.0)\n" +
				"   Height   = 0.02\n" +
				"   Quantity = waterlevel2\n" +
				"   Unit     = m2\n" +
				"   Source   = observed2\n" +
				"   Id       = aberdeen2.waterlevel2\n" +
				"   relativestandarddeviation  = 0.0\n" +
				"   timezone  = GMT\n" +
				"   analtime  = most recent\n" +
				"   standarddeviation  = 0.052\n" +
				"   status  = validation\n" +
				"   Values   = \n" +
				"   (54466.0=200801010.*?,-0.83)\n" +
				"   (54466.006944444.*?=200801010.*?,-0.88)\n" +
				"   (54466.01388888.*?=200801010.*?,-0.91)\n" +
				"   (54466.020833333.*?=200801010.*?,-0.95)\n" +
				"   (54466.02777777.*?=200801010.*?,-0.97)\n" +
				"   ...\n" +
				"   (54466.09722222.*?=200801010.*?,-0.81)\n" +
				"   (54466.104166666.*?=200801010.*?,-0.77)\n" +
				"   (54466.11111111.*?=200801010.*?,-0.71)\n" +
				"   (54466.118055555.*?=200801010.*?,-0.66)\n" +
				"   (54466.125=200801010.*?,-0.6)\n" +
				"\n" +
				"   Values.length()=19\n" +
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

//		System.out.println("obs1="+obs1String);
//		assertEquals(-1506344156,obs1String.hashCode());

		// test properties from file and overruled properties
		IVector mean = obs1.getExpectations();
		System.out.println("obs1.getExpectations()="+mean);
		System.out.println("Should be obs1.getExpectations()=[0.73,0.73,0.72,0.7,0.68,0.66,0.63,0.59,0.55,0.51,...,-0.97,-0.95,-0.92,-0.89,-0.85,-0.81,-0.77,-0.71,-0.66,-0.6]");
		assertEquals("obs1.getExpectations()","[0.73,0.73,0.72,0.7,0.68,0.66,0.63,0.59,0.55,0.51,...,-0.97,-0.95,-0.92,-0.89,-0.85,-0.81,-0.77,-0.71,-0.66,-0.6]",mean.toString());
		IVector std  = obs1.getStandardDeviations();
		System.out.println("obs1.getStandardDeviations()="+std);
		System.out.println("Should be obs1.getStandardDeviations()=[0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,...,0.052,0.052,0.052,0.052,0.052,0.052,0.052,0.052,0.052,0.052]");
		assertEquals("obs1.getStandardDeviations()","[0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,...,0.052,0.052,0.052,0.052,0.052,0.052,0.052,0.052,0.052,0.052]",std.toString());
		int n = obs1.getCount();
		System.out.println("obs1.getCount()="+n);
		System.out.println("Should be obs1.getCount()=38");
		assertEquals("obs1.getCount()",38,n);

		IObservationDescriptions descr2 = obs1.getObservationDescriptions();
		String propNames[] = descr2.getPropertyKeys();
		n = propNames.length;
		assertEquals("descr1.getPropertyKeys().length",13,n);
		System.out.print("keys = (");
		for(int i=0;i<n;i++){
			System.out.print(" "+propNames[i]);
		}
		System.out.println(")");
		String locations[] = descr2.getStringProperties("Location");
		assertEquals("descr2.getStringProperties(\"Location\").length",38,locations.length);
		System.out.println("descr2.getStringProperties(\"Location\")[0]="+locations[0]);
		System.out.println("Should be descr2.getStringProperties(\"Location\")[0]=den helder");
		assertEquals("descr2.getStringProperties(\"Location\")[0]","den helder",locations[0]);
		IVector stds = descr2.getValueProperties("standardDeviation");
		System.out.println("descr2.getValueProperties(\"standardDeviation\")="+stds);
		System.out.println("Should be descr2.getValueProperties(\"standardDeviation\")=[0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,...,0.052,0.052,0.052,0.052,0.052,0.052,0.052,0.052,0.052,0.052]");
		assertEquals("descr2.getValueProperties(\"standardDeviation\")","[0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,0.05,...,0.052,0.052,0.052,0.052,0.052,0.052,0.052,0.052,0.052,0.052]",stds.toString());

		// list of unique times
		ITime times[] = obs1.getTimes();
		int nTimes = times.length;
		assertEquals("times.length",19,nTimes);
		System.out.println("times[0]="+times[0].toString());

		//test overruling of properties
		//   - location --> Location
		locations = descr2.getStringProperties("Location");
		int m = locations.length-1;
		System.out.println("descr2.getStringProperties(\"Location\")[end]="+locations[m]);
		System.out.println("Should be descr2.getStringProperties(\"Location\")[end]=aberdeen2");
		assertEquals("descr2.getStringProperties(\"Location\")[end]","aberdeen2",locations[m]);
		//   - position
		IVector xPositions = descr2.getValueProperties("xPosition");
		System.out.println("descr2.getStringProperties(\"xPosition\")[end]="+xPositions.getValue(m));
		System.out.println("Should be descr2.getStringProperties(\"xPosition\")[end]=11.0");
		assertEquals("descr2.getStringProperties(\"xPosition\")[0]",10.0,xPositions.getValue(m),delta);
		IVector yPositions = descr2.getValueProperties("yPosition");
		System.out.println("descr2.getStringProperties(\"yPosition\")[end]="+yPositions.getValue(m));
		System.out.println("Should be descr2.getStringProperties(\"yPosition\")[end]=11.0");
		assertEquals("descr2.getStringProperties(\"yPosition\")[0]",11.0,yPositions.getValue(m),delta);
		//   - source
		String[] sources = descr2.getStringProperties("source");
		m = sources.length-1;
		System.out.println("descr2.getStringProperties(\"source\")[end]="+sources[m]);
		System.out.println("Should be descr2.getStringProperties(\"source\")[end]=observed2");
		assertEquals("descr2.getStringProperties(\"source\")[end]","observed2",sources[m]);
		//   - quantity
		String[] quantities = descr2.getStringProperties("quantity");
		m = quantities.length-1;
		System.out.println("descr2.getStringProperties(\"quantity\")[end]="+quantities[m]);
		System.out.println("Should be descr2.getStringProperties(\"quantity\")[end]=waterlevel2");
		assertEquals("descr2.getStringProperties(\"quantity\")[end]","waterlevel2",quantities[m]);
		//   - unit
		String[] units = descr2.getStringProperties("unit");
		m = units.length-1;
		System.out.println("descr2.getStringProperties(\"unit\")[end]="+units[m]);
		System.out.println("Should be descr2.getStringProperties(\"unit\")[end]=m2");
		assertEquals("descr2.getStringProperties(\"unit\")[end]","m2",units[m]);
		//   - timezone
		String[] timezones = descr2.getStringProperties("timezone");
		m = timezones.length-1;
		System.out.println("descr2.getStringProperties(\"timezone\")[end]="+timezones[m]);
		System.out.println("Should be descr2.getStringProperties(\"timezone\")[end]=GMT2");
		assertEquals("descr2.getStringProperties(\"timezone\")[end]","GMT",timezones[m]);
		//   - height
		IVector heights = descr2.getValueProperties("height");
		System.out.println("descr2.getStringProperties(\"height\")[end]="+heights.getValue(m));
		System.out.println("Should be descr2.getStringProperties(\"height\")[end]=0.02");
		assertEquals("descr2.getStringProperties(\"height\")[0]",0.02,heights.getValue(m),delta);
		//   - standardDeviation
		IVector standardDeviations = descr2.getValueProperties("standardDeviation");
		System.out.println("descr2.getStringProperties(\"standardDeviation\")[end]="+standardDeviations.getValue(m));
		System.out.println("Should be descr2.getStringProperties(\"standardDeviation\")[end]=0.052");
		assertEquals("descr2.getStringProperties(\"standardDeviation\")[0]",0.052,standardDeviations.getValue(m),delta);
		//   - status
		String[] status = descr2.getStringProperties("status");
		m = status.length-1;
		System.out.println("descr2.getStringProperties(\"status\")[end]="+status[m]);
		System.out.println("Should be descr2.getStringProperties(\"status\")[end]=validate");
		assertEquals("descr2.getStringProperties(\"status\")[end]","validation",status[m]);
	}

	public void testStochObserver_2() {
		System.out.println("=========================================================");
		System.out.println("StochObserver selections");
		System.out.println("=========================================================");

		IStochObserver obs1 = new NoosTimeSeriesStochObserver();
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

        IStochObserver obs1 = new NoosTimeSeriesStochObserver();
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

        IStochObserver obs1 = new NoosTimeSeriesStochObserver();
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

        NoosTimeSeriesStochObserver obs1 = new NoosTimeSeriesStochObserver();
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

	public void testStochObserver_missingData() {
		System.out.println("=========================================================");
		System.out.println("StochObserver missing data");
		System.out.println("=========================================================");
		double delta=0.0001;

		IStochObserver obs1 = new NoosTimeSeriesStochObserver();
		obs1.initialize(testRunDataDir, new String[]{"noosSmallObs.xml"});

		// remove some data
		TimeSeries series1=((NoosTimeSeriesStochObserver)obs1).series[1];
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
		((NoosTimeSeriesStochObserver)obs1).series[1]= series1;

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

	public void testStochObserver_noosObserver6() {
		File workingDir = new File(testRunDataDir, "noosObserver6");

		IStochObserver noosStochObserver_a = new NoosTimeSeriesStochObserver();
		noosStochObserver_a.initialize(workingDir, new String[]{"noosStochObsConfig_a.xml"});
		List<IPrevExchangeItem> exchangeItems_a = noosStochObserver_a.getObservationDescriptions().getExchangeItems();
		assertEquals("First Series", "location 1 Measurement", exchangeItems_a.get(0).getId());
		assertEquals("First Series", "location 2 Measurement", exchangeItems_a.get(1).getId());

		IStochObserver noosStochObserver_b = new NoosTimeSeriesStochObserver();
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

		IStochObserver obs1 = new NoosTimeSeriesStochObserver();
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

	public void testStochObserverTimeZoneCheck() {
		IStochObserver obs1 = new NoosTimeSeriesStochObserver();
		try {
			obs1.initialize(testRunDataDir, new String[]{"noosObservationsTZcheck.xml"});
			assert false;
		} catch(RuntimeException e) {
			assertTrue(e.getMessage().startsWith("Inconsistent time series. All time series should have the same time zone."));
		}
	}
}//end test-class


