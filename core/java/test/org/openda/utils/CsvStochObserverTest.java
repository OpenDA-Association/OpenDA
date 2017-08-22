/* OpenDA v2.4.1 
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
package org.openda.utils;
import junit.framework.TestCase;
import org.openda.interfaces.*;

import java.io.File;
import java.io.IOException;

public class CsvStochObserverTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
    	testData = new OpenDaTestSupport(CsvStochObserverTest.class,"core");
        testRunDataDir = testData.getTestRunDataDir();
    }
	
	public static void testStochObserver_1() {
	   System.out.println("=========================================================");
    // public CsvStochObserver(String content){
		String content = "time,i,value,std\n"
			+ "0.0,1.0,8.1,0.1\n"
			+ "0.1,1.0,8.2,0.1\n"
			+ "0.2,1.0,8.3,0.1";
		IStochObserver obs1 = new CsvStochObserver(content);
    // String toString(){
		String obs1String = obs1.toString();
		System.out.println("obs1="+obs1String);
		System.out.println("Should be obs1=time,i,value,std\n0.0,1.0,8.1,0.1...");
		assertEquals("obs1.tostring()",obs1String.substring(0,32),"time,i,value,std\n0.0,1.0,8.1,0.1");
    // public void free(){ //does nothing here
		
    }	

    public static void testStochObserver_2() {
 	   System.out.println("=========================================================");
        // public CsvStochObserver(String content){
		String content = "time,i,value,std\n"
			+ "0.0,1.0,8.1,0.1\n"
			+ "0.1,1.0,8.2,0.1\n"
			+ "0.2,1.0,8.3,0.1";
		IStochObserver obs2 = new CsvStochObserver(content);
    //  public int getCount(){
		int noVals = obs2.getCount();
		System.out.println("obs2.getCount()="+noVals);
		System.out.println("Should be obs2.getCount()=3");
		assertEquals("obs2.getCount()",noVals,3);		
    //  public Vector getValues(){
		IVector values = obs2.getValues();
		String valueString = values.toString();
		System.out.println("obs2.getValues()="+valueString);
		System.out.println("Should be obs2.getValues()=[8.1,8.2,8.3]");
		assertEquals("obs2.getValues()",valueString,"[8.1,8.2,8.3]");		
    //  public Vector getRealizations(){
		IVector sample = obs2.getRealizations();
		String sampleString = sample.toString();
		System.out.println("obs2.getRealizations()="+sampleString);
		System.out.println("Should be APPROXIMATELY obs2.getRealizations()=[8.1,8.2,8.3]");
    //  public Vector getExpectations(){
		IVector mean = obs2.getExpectations();
		String meanString = mean.toString();
		System.out.println("obs2.getExpectations()="+meanString);
		System.out.println("Should be obs2.getExpectations()=[8.1,8.2,8.3]");
		assertEquals("obs2.getExpectations()",meanString,"[8.1,8.2,8.3]");		
    //  public Vector getStandardDeviations(){
		IVector std = obs2.getStandardDeviations();
		String stdString = std.toString();
		System.out.println("obs2.getStandardDeviations()="+stdString);
		System.out.println("Should be obs2.getStandardDeviations()=[0.1,0.1,0.1]");
		assertEquals("obs2.getStandardDeviations()",stdString,"[0.1,0.1,0.1]");
    //  public Time [] getTimes(){
    }

    public static void testStochObserver_3() {
  	   System.out.println("=========================================================");
         // public CsvStochObserver(String content){
 		String content = "time,i,value,std\n"
 			+ "0.0,1.0,8.1,0.1\n"
 			+ "0.1,1.0,8.2,0.1\n"
 			+ "0.2,1.0,8.3,0.1";
 		IStochObserver obs3 = new CsvStochObserver(content);
    //  public Vector evaluateMarginalPDFs(Vector values){
 		IVector testValue = new Vector("[8.0,8.0,8.0]");
		double p = obs3.evaluatePDF(testValue);
		System.out.println("obs3.evaluateMarginalPDFs()="+p);
		System.out.println("Should be obs3.evaluateMarginalPDFs=5.789870153591831E-8");
		assertEquals("obs3.evaluateMarginalPDFs()",5.789870153591831E-8, p, 1.e-20);
    // public Vector getSqrtCovariance(){
		ISqrtCovariance l = obs3.getSqrtCovariance();
		String lString = l.toString();
		System.out.println("obs3.getSqrtCovariance()="+lString);
		System.out.println("Should be obs3.getSqrtCovariance=diag([0.1,0.1,0.1])");
		assertEquals("obs3.getSqrtCovariance()",lString,"diag([0.1,0.1,0.1])");		
    }
    
    public static void testStochObserver_4() {
 	   System.out.println("=========================================================");
		String content = "time,i,value,std\n"
 			+ "0.0,1.0,8.1,0.1\n"
 			+ "0.1,1.0,8.2,0.1\n"
 			+ "0.2,1.0,8.3,0.1";
 		IStochObserver obs4 = new CsvStochObserver(content);
 	   //  public ObservationDescriptions getObservationDescriptions(){
 		IObservationDescriptions descr4 = obs4.getObservationDescriptions();
		String descr4String = descr4.toString();
		System.out.println("descr4="+descr4String);
		System.out.println("Should be descr4=time,i,value,std\n0.0,1.0,8.1,0.1...");
		assertTrue(descr4String.contains("ObservationDescriptions{time,i,value,std"));
		assertTrue(descr4String.contains("0.0,1.0,8.1,0.1"));
    }

    public static void testStochObserver_5() {
 	   System.out.println("=========================================================");
		String content = "time,i,value,std\n"
 			+ "0.0,1.0,8.1,0.1\n"
 			+ "0.1,1.0,8.2,0.1\n"
 			+ "0.2,1.0,8.3,0.1";
 		IStochObserver obs5 = new CsvStochObserver(content);
 	   //  public IStochObserver createSelection(String selection){
 	   //  public IStochObserver createSelection(String selection, RelationTable reltab){
 	   //  public IStochObserver createSelection(Time selectionTimes, RelationTable reltab){
 	   //  public IStochObserver createSelection(Time selectionTimes){
        Time obsTimeFrame = new Time(0.05, 0.15);
        obsTimeFrame.setStep(0.05);
        IStochObserver obs5sel = obs5.createSelection(obsTimeFrame);
  	    String obs5selString = obs5sel.toString();
		System.out.println("obs5.createSelection()="+obs5selString);
		System.out.println("Should be obs5.createSelection=time,i,value,std\n0.1,1.0,8.2,0.1");
		assertEquals("obs5.createSelection()",obs5selString,"time,i,value,std\n0.1,1.0,8.2,0.1");		
  	}

    public static void testStochObserver_6() {
  	   System.out.println("=========================================================");
       IVector time   = Vector.range(0.0, 10.0, 1.0);
       int n         = time.getSize();
       IVector values = new Vector(n);
       IVector stdVal = new Vector(n); stdVal.setConstant(0.1);
       IVector indVal = new Vector(n); indVal.setConstant(1.0);
       IVector[] columns= new IVector[4];
       columns[0] = time;
       columns[1] = indVal;
       columns[2] = values;
       columns[3] = stdVal;
       String[] keys = {"time","index","value","std"};
 	   IStochObserver obs6 = new CsvStochObserver(columns,keys);
     //  public int getCount(){
 		int noVals = obs6.getCount();
 		System.out.println("obs6.getCount()="+noVals);
 		System.out.println("Should be obs6.getCount()=11");
 		assertEquals("obs2.getCount()",noVals,11);		
     //  public Vector getValues(){
 		IVector val6 = obs6.getValues();
 		String valueString = val6.toString();
 		System.out.println("obs6.getValues()="+valueString);
 		System.out.println("Should be obs6.getValues()=[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]");
 		assertEquals("obs6.getValues()",valueString,"[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]");		
     //  public Vector getStandardDeviations(){
 		IVector std = obs6.getStandardDeviations();
 		String stdString = std.toString();
 		System.out.println("obs6.getStandardDeviations()="+stdString);
 		System.out.println("Should be obs6.getStandardDeviations()=[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1]");
 		assertEquals("obs6.getStandardDeviations()",stdString,"[0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1]");
     //  public Time [] getTimes(){
     }

    public void testStochObserver_7() {
  	   System.out.println("=========================================================");
	   String content = "time,i,value,std\n"
			+ "0.0,1.0,8.1,0.1\n"
			+ "0.1,1.0,8.2,0.1\n"
			+ "0.2,1.0,8.3,0.1";
		CsvStochObserver obs7 = new CsvStochObserver(content);
        obs7.toFile(testRunDataDir,"test_observations.csv");
        IStochObserver obs7b = new CsvStochObserver();
        obs7b.initialize(testRunDataDir,new String[]{"test_observations.csv"});
        //  public Vector getStandardDeviations(){
 		IVector std7 = obs7.getStandardDeviations();
 		IVector std7b = obs7b.getStandardDeviations();
 		System.out.println("std7="+std7);
 		System.out.println("std7b="+std7b);
 		assertEquals("obs7.getStandardDeviations()",std7.toString(),std7b.toString());
}

    
    public static void testStochObserver_8() {
   	   System.out.println("=========================================================");
 	   String content = "time,i,value,std\n"
 			+ "0.0,1.0,8.1,0.1\n"
 			+ "0.1,1.0,8.2,0.1\n"
 			+ "0.1,2.0,8.2,0.1\n"
 			+ "0.2,1.0,8.3,0.1";
 		CsvStochObserver obs8 = new CsvStochObserver(content);
        ITime[] times = obs8.getTimes();
        for(int i=0;i<times.length;i++){
        	System.out.print(times[i].toString()+" ");
        }
        System.out.println();
 		System.out.println("times.length="+times.length);
 		System.out.println("Should be times.length=3");
  		assertEquals("times.length=",times.length,3);
 }

    public static void testStochObserver_9() {
    	   System.out.println("=========================================================");
  	   String content = "time,i,value,std\n"
  			+ "0.0,1.0,8.1,0.1\n"
  			+ "0.1,1.0,8.2,0.1\n"
  			+ "0.1,2.0,8.2,0.1\n"
  			+ "0.2,1.0,8.3,0.1";
  		CsvStochObserver obs9 = new CsvStochObserver(content);

        IObservationDescriptions descr9 = obs9.getObservationDescriptions();
        
        String keys[] = descr9.getPropertyKeys();
        assertEquals("time", keys[0]);
        
        IVector times = descr9.getValueProperties("time");
        assertEquals(0.1, times.getValue(1), 1e-5);
        
        IVector nonexisting = descr9.getValueProperties("blubblub");
        boolean exists = !(nonexisting==null);
        assertEquals(false, exists);
    }

    public static void testStochObserver_10() {
  	   System.out.println("=========================================================");
  	   // kwadratic observations
 		String content = "time,i,value,std, transform\n"
  			+ "0.0,1.0,8.1,0.1,2.0\n"
  			+ "0.1,1.0,8.2,0.1,2.0\n"
  			+ "0.2,1.0,8.3,0.1,2.0";
  		IStochObserver obs4 = new CsvStochObserver(content);
  	   //  public ObservationDescriptions getObservationDescriptions(){
  		IObservationDescriptions descr4 = obs4.getObservationDescriptions();
 		String descr4String = descr4.toString();
 		System.out.println("descr4="+descr4String);
 		System.out.println("Should be descr4=time,i,value,std\n0.0,1.0,8.1,0.1,2.0...");
 		assertTrue(descr4String.contains("ObservationDescriptions{time,i,value,std,transform"));
 		assertTrue(descr4String.contains("0.0,1.0,8.1,0.1,2.0"));
     }

}//end test-class


