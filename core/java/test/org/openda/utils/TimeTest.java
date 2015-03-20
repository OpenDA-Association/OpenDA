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
package org.openda.utils;

/**
 * TODO: class description
 * Main class for testing vector class
 */


import junit.framework.TestCase;
import org.openda.interfaces.*;
import org.openda.utils.Time;

import java.util.Date;

/**
 * Test for simple vector
 */
public class TimeTest extends TestCase {

	public static void testConversions() {
		double mjd = 51544; // 2000-01-01 00:00:00 GMT
		Date date = new Time(mjd).getDate();
		double mjd2 = new Time(date).getMJD();
		assertEquals("converson", mjd, mjd2);
	}

	public static void testSimpleTime_1() {
		System.out.println("==============================================================================");
		//  public Time(double time){
		ITime t1 = new Time(1.0);
		//  public String toString(){
		String t1_string = t1.toString();
		System.out.println("t1 = "+t1);
		System.out.println("Shoudl be t1 = 1.0");
		assertEquals("t1.toString() ",t1_string,"1.0");
		//  public boolean isSpan() {
		boolean t1IsSpan = t1.isSpan();
		assertEquals("t1.isSpan() ",t1IsSpan,false);
		//  public boolean isStamp() {
		boolean t1IsStamp = t1.isStamp();
		assertEquals("t1.isStamp() ",t1IsStamp,true);
		//  public double getUserTime() {
		String t1_uts = ""+t1.getMJD();
		assertEquals("it1.getUserTime()", t1_uts, "1.0");
		System.out.println("t1.getUserTime() = "+t1_uts);
		System.out.println("Shoudl be t1.getUserTime() = 1.0");
		//  public Time clone(){
		ITime t1_clone = new Time(t1, 0);
		String t2_clone_string = t1_clone.toString();
		System.out.println("t1_clone = "+ t1_clone);
		System.out.println("Should be t1_clone = 1.0");
		assertEquals("t1_clone.toString() ",t2_clone_string,"1.0");
	}

	public static void testSimpleTime_2() {
		System.out.println("==============================================================================");
		//  public Time(double startTime, double endTime, double step){
		Time t2 = new Time(0.0,10.0);
		t2.setStep(0.5);
		String t2_string = t2.toString();
		System.out.println("t2 = "+t2);
		System.out.println("Should be t2 = 0.0:0.5:10.0");
		assertEquals("t2.toString() ",t2_string,"0.0:0.5:10.0");
		//  public Time clone(){
		Time t2_clone = new Time(0.0,10.0);
		t2_clone.setStep(0.5);
		String t2_clone_string = t2_clone.toString();
		System.out.println("t2_clone = "+t2_clone);
		System.out.println("Should be t2_clone = 0.0:0.5:10.0");
		assertEquals("t2_clone.toString() ",t2_clone_string,"0.0:0.5:10.0");
		//  public boolean isSpan() {
		boolean t2IsSpan = t2.isSpan();
		assertEquals("t2.isSpan() ",t2IsSpan,true);
		//  public boolean isStamp() {
		boolean t2IsStamp = t2.isStamp();
		assertEquals("t2.isStamp() ",t2IsStamp,false);
		//  public double getBeginUserTime() {
		double t2_start = t2.getBeginMJD();
		System.out.println("t2.getBeginMJD = "+t2_start);
		System.out.println("Should be t2.getBeginMJD = 0.0");
		assertEquals("t2.getBeginMJD() ",""+t2_start,"0.0");
		//  public double getEndUserTime() {
		double t2_end = t2.getEndMJD();
		System.out.println("t2.getEndUserTime = "+t2_end);
		System.out.println("Should be t2.getEndUserTime = 10.0");
		assertEquals("t2.getEndUserTime() ",""+t2_end,"10.0");
	}

	public static void testSimpleTime_3() {
		System.out.println("==============================================================================");
		ITime t3a = new Time(1.0);
		ITime t3b = new Time(2.0);
		//  public boolean afterEquals(Time toCheck) {
		boolean t3bAfter3a = t3a.after(t3b);
		if (t3bAfter3a) System.out.println("true");
		assertEquals("t3a.after(t3b) ",t3bAfter3a,false);
		boolean t3aAfter3b = t3b.after(t3a);
		assertEquals("t3b.after(t3a) ",t3aAfter3b,true);
		//  public boolean before(Time toCheck) {
		boolean t3bBefore3a = t3a.beforeEquals(t3b);
		assertEquals("t3a.beforeEquals(t3b) ",t3bBefore3a,true);
		boolean t3aBefore3b = t3b.beforeEquals(t3a);
		assertEquals("t3b.beforeEquals(t3a) ",t3aBefore3b,false);
	}

	public static void testSimpleTime_4() {
		System.out.println("==============================================================================");
		Time t4 = new Time(0.0,10.0);
		t4.setStep(0.5);
		ITime t4b= new Time(5.0);
		ITime t4c= new Time(15.0);
		//  public long getStepCount() {
		long stepCount = t4.getStepCount();
		System.out.println("t4.getStepCount() = "+stepCount);
		System.out.println("Should be t4.getStepCount() = 20"); 
		assertEquals("t4.getStepCount() ",stepCount,20);
		//  public long getTimeStep(Time toReach) {
		long timeStep = t4.getTimeStep(t4b);
		System.out.println("t4.getTimeStep(t4b) = "+timeStep);
		System.out.println("Should be t4.getTimeStep(t4b) = 10"); 
		assertEquals("t4.getTimeStep(t4b) ",timeStep,10);
		//  public boolean inSpan(Time toCheck) {
		boolean t4bInSpan = t4b.inSpan(t4);
		assertEquals("t4b.inSpan(t4) ",t4bInSpan,true);
		boolean t4cInSpan = t4c.inSpan(t4);
		assertEquals("t4c.inSpan(t4) ",t4cInSpan,false);
	}


	public static void testSimpleTime_5() {
		System.out.println("==============================================================================");
		Time t5b = new Time(0.0,10.0);
		t5b.setStep(0.5);
		ITime t5a = new Time(2.0);
		String t5a_string = t5a.toString();
		System.out.println("t5a = "+t5a);
		System.out.println("Should be t5a = 2.0");
		assertEquals("t5a.toString() ",t5a_string,"2.0");
		t5b = new Time(10.0,20.0);
		t5b.setStep(0.1);
		String t5b_string = t5b.toString();
		System.out.println("t5b = "+t5b);
		System.out.println("Should be t5b = 10.0:0.1:20.0");
		assertEquals("t5b.toString() ",t5b_string,"10.0:0.1:20.0");
	}

	public static void testSimpleTime_6() {
		System.out.println("==============================================================================");
		Time t6 = new Time(10.0,20.0);
		t6.setStep(0.5);
		ITime t6End = t6.getEndTime();
		String t6End_string = t6End.toString();
		System.out.println("t6.getEndDateTime() = "+t6End);
		System.out.println("Should be t6.getEndDateTime() = 20.0");
		assertEquals("t6.getEndDateTime() ",t6End_string,"20.0");
		ITime t6Begin = t6.getBeginTime();
		String t6Begin_string = t6Begin.toString();
		System.out.println("t6.getBeginDateTime() = "+t6Begin);
		System.out.println("Should be t6.getBeginDateTime() = 10.0");
		assertEquals("t6.getBeginDateTime() ",t6Begin_string,"10.0");
	}

	public static void testSimpleTime_7() {
		System.out.println("==============================================================================");
		double mjds[] = {55432.0,55432.25,55432.5};
		ITime times[] = Time.Mjds2Times(mjds);
		System.out.println("t[0]= "+times[0]);
		System.out.println("t[1]= "+times[1]);
		System.out.println("t[2]= "+times[2]);
		assertEquals("t[0]",55432.0,times[0].getMJD());
		assertEquals("t[1]",55432.25,times[1].getMJD());
		assertEquals("t[2]",55432.5,times[2].getMJD());
	}

	public static void testSimpleTime_8() {
		System.out.println("==============================================================================");
		double mjds[] = {55432.0,55432.25,55432.5,55432.75,55433.0};
		ITime times[] = Time.Mjds2Times(mjds);
		ITime selectionSpan = new Time(55432.25,55432.75);
		ITime selectedTimes[] = Time.createSelection(times, selectionSpan);
		System.out.println("t[0]= "+selectedTimes[0]);
		System.out.println("t[1]= "+selectedTimes[1]);
		System.out.println("t[2]= "+selectedTimes[2]);
		assertEquals("t[0]",55432.25,selectedTimes[0].getMJD());
		assertEquals("t[1]",55432.5,selectedTimes[1].getMJD());
		assertEquals("t[2]",55432.75,selectedTimes[2].getMJD());
	}
}//end class



