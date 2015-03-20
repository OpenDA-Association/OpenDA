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
package org.openda.observers;
import junit.framework.TestCase;

import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.IStochObserver;
import org.openda.interfaces.IVector;
import org.openda.utils.OpenDaTestSupport;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

public class GroupStochObserverTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(GroupStochObserverTest.class,"observers");
        testRunDataDir = testData.getTestRunDataDir();
    }

	public static void testGroupStochObserver_1() {
	   System.out.println("=========================================================");
	   System.out.println("GroupStochObser: create and decompose (Array constructor)");
	   System.out.println("=========================================================");
	   // two separate observers
	   IStochObserver obs1 = getSO(0.0, 1);
	   IStochObserver obs2 = getSO(1.0, 3);
	   // create a group
	   IStochObserver obsArray[] = new IStochObserver[2];
	   obsArray[0] = obs1;
	   obsArray[1] = obs2;
	   String ids[] = {"observer1","observer2"};
	   GroupStochObserver obsGroup = new GroupStochObserver(obsArray,ids);
	   System.out.println("obsGroup = "+obsGroup.toString());

	   String idsFromGroup[] = obsGroup.getIds();
	   assertEquals("obsGroup.getIds()","observer1", idsFromGroup[0]);
	   IStochObserver obsFromGroup1 = obsGroup.getChild(0);
	   System.out.println("obsGroup.getChild(0) = "+obsFromGroup1.toString());
	   assertEquals("obsGroup.getChild(0)",1375209275, obsFromGroup1.toString().hashCode());
	   IStochObserver obsFromGroup2 = obsGroup.getChild("OBSERVER2");
	   System.out.println("obsGroup.getChild(\"OBSERVER2\") = "+obsFromGroup2.toString());
	   assertEquals("obsGroup.getChild(\"OBSERVER2\")",253263575, obsFromGroup2.toString().hashCode());
    }

	public static void testGroupStochObserver_2() {
		   System.out.println("=========================================================");
		   System.out.println("GroupStochObser: create and decompose (List constructor)");
		   System.out.println("=========================================================");
		   // two separate observers
		   IStochObserver obs1 = getSO(0.0, 1);
		   IStochObserver obs2 = getSO(1.0, 3);
		   // create a group
		   ArrayList<IStochObserver> obsArray = new ArrayList<IStochObserver>();
		   obsArray.add(obs1);
		   obsArray.add(obs2);
		   ArrayList<String> ids = new ArrayList<String>();
		   ids.add("observer1");
		   ids.add("observer2");
		   GroupStochObserver obsGroup = new GroupStochObserver(obsArray,ids);
		   System.out.println("obsGroup = "+obsGroup.toString());

		   String idsFromGroup[] = obsGroup.getIds();
		   assertEquals("obsGroup.getIds()","observer1", idsFromGroup[0]);
		   IStochObserver obsFromGroup1 = obsGroup.getChild(0);
		   System.out.println("obsGroup.getChild(0) = "+obsFromGroup1.toString());
		   assertEquals("obsGroup.getChild(0)",1375209275, obsFromGroup1.toString().hashCode());
		   IStochObserver obsFromGroup2 = obsGroup.getChild("OBSERVER2");
		   System.out.println("obsGroup.getChild(\"OBSERVER2\") = "+obsFromGroup2.toString());
		   assertEquals("obsGroup.getChild(\"OBSERVER2\")",253263575, obsFromGroup2.toString().hashCode());
	    }


	public static void testGroupStochObserver_3() {
		   System.out.println("=========================================================");
		   System.out.println("GroupStochObser: create and decompose (String constructor)");
		   System.out.println("=========================================================");
		   String config[] = {
				 "<stochObserver>"
			   + "   <stochObserver id=\"location1\" className=\"org.openda.utils.CsvStochObserver\">"
			   + "   time,i,value,std\n"
			   + "   0.0,1.0,1.1,0.1\n"
			   + "   0.1,1.0,1.2,0.1\n"
			   + "   0.2,1.0,1.3,0.1"
			   + "   </stochObserver>"
			   + "   <stochObserver id=\"location2\" className=\"org.openda.utils.CsvStochObserver\">"
			   + "   time,i,value,std\n"
			   + "   0.0,2.0,2.1,0.1\n"
			   + "   0.1,2.0,2.2,0.1\n"
			   + "   0.2,2.0,2.3,0.1"
			   + "   </stochObserver>"
			   + "</stochObserver>"};
		   GroupStochObserver obsGroup = new GroupStochObserver();
		   obsGroup.initialize(null, config);
		   System.out.println("obsGroup = "+obsGroup.toString());

		   IVector expectations = obsGroup.getExpectations();
		   double val0 = expectations.getValue(0);
		   System.out.println("obsGroup.getValue(0) = "+val0);
		   System.out.println("Should be obsGroup.getValue(0) = 1.1");
		   assertEquals("obsGroup.getValue(0)",1.1, val0);
		   double val3 = expectations.getValue(3);
		   System.out.println("obsGroup.getValue(3) = "+val3);
		   System.out.println("Should be obsGroup.getValue(3) = 2.1");
		   assertEquals("obsGroup.getValue(3)",2.1, val3);
	    }

	public void testGroupStochObserver_4() {
		   System.out.println("=========================================================");
		   System.out.println("GroupStochObser: create and decompose (File constructor)");
		   System.out.println("=========================================================");
		   String fileName[] = {"groupObserver.xml"};
		   GroupStochObserver obsGroup = new GroupStochObserver();
		   obsGroup.initialize(testRunDataDir, fileName);

		   System.out.println("obsGroup = "+obsGroup.toString());

		   IVector expectations = obsGroup.getExpectations();
		   double val0 = expectations.getValue(0);
		   System.out.println("obsGroup.getValue(0) = "+val0);
		   System.out.println("Should be obsGroup.getValue(0) = 0.73");
		   assertEquals("obsGroup.getValue(0)",0.73, val0);
		   double val3 = expectations.getValue(3);
		   System.out.println("obsGroup.getValue(3) = "+val3);
		   System.out.println("Should be obsGroup.getValue(3) = 0.7");
		   assertEquals("obsGroup.getValue(3)",0.7, val3);
	    }

	public void testGroupStochObserver_5() {
		   System.out.println("=========================================================");
		   System.out.println("GroupStochObser: create and decompose (File constructor)");
		   System.out.println("=========================================================");
		   String fileName[] = {"groupObserver.xml"};
		   GroupStochObserver obsGroup = new GroupStochObserver();
		   obsGroup.initialize(testRunDataDir, fileName);

		   System.out.println("obsGroup = "+obsGroup.toString());
			IObservationDescriptions descr = obsGroup.getObservationDescriptions();
			String descrString = descr.toString();
			System.out.println("descr="+descrString);
			assertTrue(descrString.contains("den_helder_group"));
			assertTrue(descrString.contains("aberdeen_group"));
			assertTrue(descrString.contains("Source   = observed2"));
			assertTrue(descrString.contains("standarddeviation  = 0.05"));

			if(descr instanceof GroupObservationDesrciptions){
				IObservationDescriptions descrPart1 = ((GroupObservationDesrciptions) descr).getChild("aberdeen_group");
				int nObsPart1=descrPart1.getObservationCount();
				System.out.println("descrPart1="+descrPart1.toString());
				assertEquals("descrPart1.getObservationCount()",19, nObsPart1);
			}else{
				assertTrue(false);
			}
		}

	public void testGroupStochObserver_6() {
		   System.out.println("=========================================================");
		   System.out.println("GroupStochObser: retrieve content");
		   System.out.println("=========================================================");
		   String fileName[] = {"groupObserver.xml"};
		   GroupStochObserver obsGroup = new GroupStochObserver();
		   obsGroup.initialize(testRunDataDir, fileName);

		   //System.out.println("obsGroup = "+obsGroup.toString());

		   IVector mean=obsGroup.getExpectations();
		   System.out.println("mean="+mean.toString());
		   assertTrue(mean.toString().contains("TreeVector combined, SubTreeVectors:"));
		   assertTrue(mean.toString().contains("den_helder_group [0.73,0.73,0.72,0.7,0.68"));
		   assertTrue(mean.toString().contains("aberdeen_group [-0.83,-0.88,-0.91,-0.95"));

		   IVector std=obsGroup.getStandardDeviations();
		   assertTrue(std.toString().contains("TreeVector combined, SubTreeVectors:"));
		   assertTrue(std.toString().contains("den_helder_group [0.05,0.05,0.05,0.05,0.05"));
		   assertTrue(std.toString().contains("aberdeen_group [0.052,0.052,0.052,0.052,0.052"));
		   System.out.println("std="+std.toString());
		   IVector values=obsGroup.getValues();
		   System.out.println("values="+values.toString());
		   assertTrue(mean.toString().contains("TreeVector combined, SubTreeVectors:"));
		   assertTrue(mean.toString().contains("den_helder_group [0.73,0.73,0.72,0.7,0.68"));
		   assertTrue(mean.toString().contains("aberdeen_group [-0.83,-0.88,-0.91,-0.95"));


		   //obsGroup.createSelection(selectionTimes);
	}




	/*
	 *  support routines
	 */
	public static IStochObserver getSO(double bias,int offset){
		TimeSeries series[] = new TimeSeries[2];
		double times[]  = {0.0, 1.0, 2.0, 3.0};
		double values[] = {0.0, 0.1, 0.2, 0.3};
		for(int i=0;i<values.length;i++){
			values[i]+=bias;
		}
		series[0] = new TimeSeries(times,values);
		series[0].setLocation("location"+offset);
		series[0].setHeight(0.0);
		series[0].setPosition(0.0, 0.0);
		series[0].setQuantity("quantity"+offset);
		series[0].setUnit("unit"+offset);
		series[0].setSource("source"+offset);
		series[0].setProperty("standardDeviation", ""+0.010);
		series[0].setProperty("String", "bla"+(offset+1));
		double times2[]  = {0.0, 1.0, 2.0, 4.0};
		double values2[] = {1.0, 1.1, 1.2, 1.4};
		for(int i=0;i<values2.length;i++){
			values2[i]+=bias;
		}
		series[1] = new TimeSeries(times2,values2);
		series[1].setLocation("location"+(offset+1));
		series[1].setHeight(1.0);
		series[1].setPosition(1.0, 0.0);
		series[1].setQuantity("quantity"+(offset+1));
		series[1].setUnit("unit"+(offset+1));
		series[1].setSource("source"+(offset+1));
		series[1].setProperty("standardDeviation", ""+0.011);
		series[1].setProperty("String", "bla"+(offset+1));
		IStochObserver obs1 = new TimeSeriesStochObserver(series);
		return obs1;
	}


}//end test-class


