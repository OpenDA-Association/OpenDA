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
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.interfaces.*;
import org.openda.utils.Time;
import org.openda.utils.Vector;

import junit.framework.TestCase;

public class TimeSeriesStochObserverTest extends TestCase {

	public static void testTimeSeriesStochObserver_1() {
		System.out.println("=========================================================");
		// public TimeSeriesStochObserver(TimeSeries series[]){
		IStochObserver obs1 = getSO();
		// String toString(){
		String obs1String = obs1.toString();
		System.out.println("obs1="+obs1String);
		System.out.println("Should be obs1=StochObserver(...");
		assertEquals("StochObserver(",obs1String.substring(0,14));
		// public void free(){ //does nothing here

	}	

	public static void testTimeSeriesStochObserver_2() {
		System.out.println("=========================================================");
		// public StochObserver(String content){
		IStochObserver obs2 = getSO();
		//  public int getCount(){
		int noVals = obs2.getCount();
		System.out.println("obs2.getCount()="+noVals);
		System.out.println("Should be obs2.getCount()=8");
		assertEquals("obs2.getCount()",noVals,8);		
		//  public Vector getValues(){
		IVector values = obs2.getValues();
		String valueString = values.toString();
		System.out.println("obs2.getValues()="+valueString);
		System.out.println("Should be obs2.getValues()=[0.0,0.1,0.2,0.3,1.0,1.1,1.2,1.4]");
		assertEquals("obs2.getValues()",valueString,"[0.0,0.1,0.2,0.3,1.0,1.1,1.2,1.4]");		
		//  public Vector getRealizations(){
		IVector sample = obs2.getRealizations();
		String sampleString = sample.toString();
		System.out.println("obs2.getRealizations()="+sampleString);
		System.out.println("Should be APPROXIMATELY obs2.getRealizations()=[0.0,0.1,0.2,0.3,1.0,1.1,1.2,1.4]");
		//  public Vector getExpectations(){
		IVector mean = obs2.getExpectations();
		String meanString = mean.toString();
		System.out.println("obs2.getExpectations()="+meanString);
		System.out.println("Should be obs2.getExpectations()=[0.0,0.1,0.2,0.3,1.0,1.1,1.2,1.4]");
		assertEquals("obs2.getExpectations()",meanString,"[0.0,0.1,0.2,0.3,1.0,1.1,1.2,1.4]");		
		//  public Vector getStandardDeviations(){
		IVector std = obs2.getStandardDeviations();
		String stdString = std.toString();
		System.out.println("obs2.getStandardDeviations()="+stdString);
		System.out.println("Should be obs2.getStandardDeviations()=[0.01,0.01,0.01,0.01,0.011,0.011,0.011,0.011]");
		assertEquals("obs2.getStandardDeviations()",stdString,"[0.01,0.01,0.01,0.01,0.011,0.011,0.011,0.011]");
		//  public Time [] getTimes(){
	}

	public static void testTimeSeriesStochObserver_3() {
		System.out.println("=========================================================");
		// public StochObserver(String content){
		IStochObserver obs3 = getSO();
		//  public Vector evaluateMarginalPDFs(Vector values){
		IVector testValue = new Vector("[0.0,0.1,0.2,0.3,1.0,1.1,1.2,1.4]");
		double p = obs3.evaluatePDF(testValue);
		String pString = ""+p;
		System.out.println("obs3.evaluatePDF()="+pString);
		System.out.println("Should be obs3.evaluatePDF=9.394015386927088E-20");
		assertEquals("obs3.evaluatePDF()",pString,"9.394015386927088E-20");
		// public Vector getSqrtCovariance(){
		ISqrtCovariance l = obs3.getSqrtCovariance();
		String lString = l.toString();
		System.out.println("obs3.getSqrtCovariance()="+lString);
		System.out.println("Should be obs3.getSqrtCovariance=diag([0.01,0.01,0.01,0.01,0.011,0.011,0.011,0.011])");
		assertEquals("obs3.getSqrtCovariance()",lString,"diag([0.01,0.01,0.01,0.01,0.011,0.011,0.011,0.011])");		
	}

	public static void testTimeSeriesStochObserver_4() {
		System.out.println("=========================================================");
		IStochObserver obs4 = getSO();
		//  public ObservationDescriptions getObservationDescriptions(){
		IObservationDescriptions descr4 = obs4.getObservationDescriptions();
		String descr4String = descr4.toString();
		System.out.println("descr4="+descr4String);
		System.out.println("Should be descr4=ObservationDescriptions(...");
		assertEquals("descr4.tostring()",descr4String.substring(0,24),"ObservationDescriptions(");
		
		String keys[] = descr4.getPropertyKeys();
		printStrings("keys",keys);
		String locs[] = descr4.getStringProperties("Location");
		printStrings("locations",locs);
		IVector heights = descr4.getValueProperties("Height");
		System.out.println("heights = "+heights.toString());
		System.out.println("Should be heights =[0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0]");
		assertEquals("heights",heights.toString(),"[0.0,0.0,0.0,0.0,1.0,1.0,1.0,1.0]");
	}

	public static void testTimeSeriesStochObserver_5() {
		System.out.println("=========================================================");
		IStochObserver obs5 = getSO();
		System.out.println("selection="+obs5.toString());
		//  public IStochObserver createSelection(String selection){
		//  public IStochObserver createSelection(String selection, RelationTable reltab){
		//  public IStochObserver createSelection(Time selectionTimes, RelationTable reltab){
		//  public IStochObserver createSelection(Time selectionTimes){
		Time obsTimeFrame = new Time(0.9, 2.1); // between times in series
		obsTimeFrame.setStep(0.05);
		IStochObserver obs5sel = obs5.createSelection(obsTimeFrame);
		int obs5selLength = obs5sel.getCount();
		System.out.println("obs5.createSelection().getCount()="+obs5selLength);
		System.out.println("Should be obs5.createSelection.getCount()=4");
		System.out.println("selection="+obs5sel.toString());
		assertEquals("obs5.createSelection().getCount()",4,obs5selLength);		

		obsTimeFrame = new Time(0.0, 4.0); // at times from series
		obsTimeFrame.setStep(0.05);
		obs5sel = obs5.createSelection(obsTimeFrame);
		obs5selLength = obs5sel.getCount();
		System.out.println("obs5.createSelection().getCount()="+obs5selLength);
		System.out.println("Should be obs5.createSelection.getCount()=4");
		System.out.println("selection="+obs5sel.toString());
		assertEquals("obs5.createSelection().getCount()",8,obs5selLength);		

		obsTimeFrame = new Time(7.0, 40.0); //empty series selection after times
		obsTimeFrame.setStep(0.05);
		obs5sel = obs5.createSelection(obsTimeFrame);
		obs5selLength = obs5sel.getCount();
		System.out.println("obs5.createSelection().getCount()="+obs5selLength);
		System.out.println("Should be obs5.createSelection.getCount()=4");
		System.out.println("selection="+obs5sel.toString());
		assertEquals("obs5.createSelection().getCount()",0,obs5selLength);		
		
		obsTimeFrame = new Time(-7.0, -1.0); //empty series selection before times
		obsTimeFrame.setStep(0.05);
		obs5sel = obs5.createSelection(obsTimeFrame);
		obs5selLength = obs5sel.getCount();
		System.out.println("obs5.createSelection().getCount()="+obs5selLength);
		System.out.println("Should be obs5.createSelection.getCount()=4");
		System.out.println("selection="+obs5sel.toString());
		assertEquals("obs5.createSelection().getCount()",0,obs5selLength);		

	}

	public static void testTimeSeriesStochObserver_6() {
		System.out.println("=========================================================");
		IStochObserver obs6 = getSO();
		ITime[] times = obs6.getTimes();
		for(int i=0;i<times.length;i++){
			System.out.print(times[i].toString()+" ");
		}
		System.out.println();
		System.out.println("times.length="+times.length);
		System.out.println("Should be times.length=5");
		assertEquals("times.length=",times.length,5);
	}

	public static IStochObserver getSO(){
		TimeSeries series[] = new TimeSeries[2];
		double times[]  = {0.0, 1.0, 2.0, 3.0};
		double values[] = {0.0, 0.1, 0.2, 0.3};
		series[0] = new TimeSeries(times,values);
		series[0].setLocation("location0");
		series[0].setHeight(0.0);
		series[0].setPosition(0.0, 0.0);
		series[0].setQuantity("quantity0");
		series[0].setUnit("unit0");
		series[0].setSource("source0");
		series[0].setProperty("standardDeviation", ""+0.010);
		series[0].setProperty("String", "bla");
		double times2[]  = {0.0, 1.0, 2.0, 4.0};
		double values2[] = {1.0, 1.1, 1.2, 1.4};
		series[1] = new TimeSeries(times2,values2);
		series[1].setLocation("location1");
		series[1].setHeight(1.0);
		series[1].setPosition(1.0, 0.0);
		series[1].setQuantity("quantity1");
		series[1].setUnit("unit1");
		series[1].setSource("source1");
		series[1].setProperty("standardDeviation", ""+0.011);
		series[1].setProperty("String", "bla1");
		IStochObserver obs1 = new TimeSeriesStochObserver(series);
		return obs1;
	}

	public static void printStrings(String label,String strings[]){
		System.out.print(label+"=(");
		for(int i=0;i<strings.length;i++){
			System.out.print("'"+strings[i]+"'");
		}
		System.out.println(")");

	}
	
}
