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
package org.openda.exchange.timeseries;

import junit.framework.TestCase;
//import java.util.TimeZone;


public class TimeUtilsTest extends TestCase {
	public static void testTimeUtils() {
		double delta=0.000001;
		//public static double date2Mjd(String date) throws ParseException{
		String date1 = "197001010000";
		double mjd1;
		try {
			mjd1  = TimeUtils.date2Mjd(date1);
		} catch (Exception e) {
			mjd1  = Double.NaN;
		}
		assertEquals(40587.0,mjd1, delta);
		//public static String mjdToString(double mjd){
		String date2 = TimeUtils.mjdToString(mjd1);
		assertEquals("197001010000",date2);
		double[] mjd;
		mjd = new double[] {40587.0,40588.0,40589.0,40589.5};
		String dates[] = TimeUtils.mjdToString(mjd);
		assertEquals("197001010000",dates[0]);
		assertEquals("197001020000",dates[1]);
		//public static String[] mjdToString(double[] mjd){
		double mjdCopy[] = TimeUtils.date2Mjd(dates, 9999.99);
		assertEquals("mjdCopy[0]", 40587.0,mjdCopy[0]);
		assertEquals("mjdCopy[1]", 40588.0,mjdCopy[1]);

	}

	public static void testTimeMjdOffset() {
		double delta=0.000001;
		//public static double date2Mjd(String date) throws ParseException{
		String date1 = "200701140000";
		double mjd1;
		try {
			mjd1  = TimeUtils.date2Mjd(date1);
		} catch (Exception e) {
			mjd1  = Double.NaN;
		}
		assertEquals(54114.0,mjd1, delta);
		//public static String mjdToString(double mjd){
		String date2 = TimeUtils.mjdToString(mjd1);
		assertEquals("200701140000",date2);

		//public static double date2Mjd(String date) throws ParseException{
		String date3 = "14 JAN 2007";
		double mjd3;
		try {
			mjd3  = TimeUtils.date2Mjd(date3,"dd MMM yyyy");
		} catch (Exception e) {
			mjd3  = Double.NaN;
		}
		assertEquals("mjd3", 54114.0,mjd3, delta);
		//public static String mjdToString(double mjd){
		String date4 = TimeUtils.mjdToString(mjd1);
		assertEquals("200701140000",date4);
	}

	public static void testTimeUtils_FormatProblem() {
		double delta=0.000001;
		//public static double date2Mjd(String date) throws ParseException{
		String date1 = "197001010010";
		double mjd1;
		try {
			mjd1  = TimeUtils.date2Mjd(date1);
		} catch (Exception e) {
			mjd1  = Double.NaN;
		}
		assertEquals(40587.0+10.0/1440.0,mjd1, delta);

		String date2 = "19700101001000";
		double mjd2;
		try {
			mjd2  = TimeUtils.date2Mjd(date2);
		} catch (Exception e) {
			mjd2  = Double.NaN;
		}
		assertEquals(40587.0+10.0/1440.0,mjd2, delta);

		String date3 = "19700101"; //wrong format
		double mjd3;
		try {
			mjd3  = TimeUtils.date2Mjd(date3);
		} catch (Exception e) {
			mjd3  = Double.NaN;
			System.out.println(e.getMessage());
		}
		assertTrue(Double.isNaN(mjd3));

		String date4 = "01 JAN 1970 12:59"; //wrong format
		double mjd4;
		try {
			mjd4  = TimeUtils.date2Mjd(date4);
		} catch (Exception e) {
			mjd4  = Double.NaN;
			System.out.println(e.getMessage());
		}
		assertTrue(Double.isNaN(mjd4));
	}

	public static void testTimeUtils_sequence() {
		double delta=1e-6;
		String test1 = "201008240000,201008240600,201008241200"; //fixed list
		double out1[] = TimeUtils.dateTimeSequenceString2Mjd(test1);
		System.out.println("input  : "+test1);
		System.out.println("output : "+TimeUtils.joinStrings(TimeUtils.mjdToString(out1),","));
		System.out.println("out1[0]="+out1[0]);
		assertEquals("t.length",3,out1.length);
		assertEquals("t[0]",55432.0,out1[0],delta);
		assertEquals("t[1]",55432.25,out1[1],delta);
		assertEquals("t[2]",55432.5,out1[2],delta);

		String test2 = "201008240000,201008240100,...,201008241200"; //regular sequence
		double out2[] = TimeUtils.dateTimeSequenceString2Mjd(test2);
		System.out.println("input  : "+test2);
		System.out.println("output : "+TimeUtils.joinStrings(TimeUtils.mjdToString(out2),","));
		assertEquals("t.length",13,out2.length);
		assertEquals("t[0]",55432.0,out2[0],delta);
		assertEquals("t[1]",55432.0416666666,out2[1],delta);
		assertEquals("t[2]",55432.5,out2[12],delta);

		String test3 = "55432.0,55432.25,55432.5"; //fixed list as modified julians
		double out3[] = TimeUtils.MjdSequenceString2Mjd(test3);
		System.out.println("input  : "+test3);
		System.out.println("output : "+TimeUtils.joinStrings(TimeUtils.mjdToString(out3),","));
		System.out.println("out1[0]="+out3[0]);
		assertEquals("t.length",3,out3.length);
		assertEquals("t[0]",55432.0,out3[0],delta);
		assertEquals("t[1]",55432.25,out3[1],delta);
		assertEquals("t[2]",55432.5,out3[2],delta);

		String test4 = "55432.0,55432.0416666666,...,55432.5"; //regular sequence as modified julians
		double out4[] = TimeUtils.MjdSequenceString2Mjd(test4);
		System.out.println("input  : "+test4);
		System.out.println("output : "+TimeUtils.joinStrings(TimeUtils.mjdToString(out4),","));
		System.out.println("out1[0]="+out4[0]);
		assertEquals("t.length",13,out4.length);
		assertEquals("t[0]",55432.0,out4[0],delta);
		assertEquals("t[1]",55432.0416666666,out4[1],delta);
		assertEquals("t[2]",55432.5,out4[12],delta);
	}

}


