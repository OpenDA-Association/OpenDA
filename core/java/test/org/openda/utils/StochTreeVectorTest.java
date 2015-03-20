/* MOD_V1.0 
 * Copyright (c) 2010 OpenDA Association 
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

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import org.openda.interfaces.IStochVector;
import org.openda.interfaces.IVector;

import junit.framework.TestCase;

public class StochTreeVectorTest extends TestCase {

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(CsvStochObserverTest.class,"core");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public static void testStochTreeVector_1() {
		System.out.println("==============================================================================");
		System.out.println("Basics : leaves are not StochTreeVectors");
		System.out.println("==============================================================================");
		IVector mean1 = new Vector("[0.0,0.0]");
		IVector std1  = new Vector("[0.1,0.1]");
		StochVector sv1= new StochVector(mean1,std1);
		IVector mean2 = new Vector("[10.0,10.0,10.0]");
		IVector std2  = new Vector("[1.0,1.0,1.0]");
		StochVector sv2= new StochVector(mean2,std2);
		StochTreeVector stv = new StochTreeVector("StochTreeVector");
		stv.addChild(sv1);
		stv.addChild(sv2);
		System.out.println("StochTreeVector = "+stv.toString());

		IVector mean = stv.getExpectations();
		System.out.println("mean = "+mean.toString());
		double delta=0.00001;
		assertEquals(0.0, mean.getValue(0), delta);
		assertEquals(10.0, mean.getValue(4), delta);
		IVector mean_part1 = ((TreeVector)mean).getSubTreeVector("StochTreeVector_sub0");
		assertEquals(0.0, mean1.getValue(0), delta);
		
		IVector std = stv.getStandardDeviations();
		System.out.println("std = "+std.toString());
		assertEquals(0.1, std.getValue(0), delta);
		assertEquals(1.0, std.getValue(4), delta);
		
		StochVector.setSeed(1234567);
		IVector sample = stv.createRealization();
		System.out.println("sample = "+sample.toString());
		assertEquals(-0.08184836129163191, sample.getValue(0), delta);
		assertEquals(9.934785357659559, sample.getValue(4), delta);		
		
		ArrayList<IStochVector> children = stv.getChildren();
		IVector mean2_test = children.get(1).getExpectations();
		IVector difference = mean2_test;
		difference.axpy(-1.0, mean2);
		assertEquals(0.0,difference.norm2(),delta);
		
		double prob = stv.evaluatePdf(mean);
		System.out.println("evaluatePdf = "+prob);
		assertEquals(1.01053E-4, prob, delta);
		
		boolean correlated = stv.hasCorrelatedElements();
		assertFalse(correlated);
		
		String id = stv.getId();
		assertEquals("StochTreeVector",id);
		//TODO fix this
		// stv.getSqrtCovariance()
	}

	public static void testStochTreeVector_2() {
		System.out.println("==============================================================================");
		System.out.println("Basics : leaves are StochTreeVectors");
		System.out.println("==============================================================================");
		IVector mean1 = new Vector("[0.0,0.0]");
		IVector std1  = new Vector("[0.1,0.1]");
		StochVector sv1= new StochVector(mean1,std1);
		StochTreeVector stv1 = new StochTreeVector("part1");
		stv1.addChild(sv1);
		IVector mean2 = new Vector("[10.0,10.0,10.0]");
		IVector std2  = new Vector("[1.0,1.0,1.0]");
		StochVector sv2= new StochVector(mean2,std2);
		StochTreeVector stv2 = new StochTreeVector("part2");
		stv2.addChild(sv2);
		StochTreeVector stv = new StochTreeVector("StochTreeVector");
		stv.addChild(stv1);
		stv.addChild(stv2);
		System.out.println("StochTreeVector = "+stv.toString());

		IVector mean = stv.getExpectations();
		System.out.println("mean = "+mean.toString());
		double delta=0.00001;
		assertEquals(0.0, mean.getValue(0), delta);
		assertEquals(10.0, mean.getValue(4), delta);
		IVector mean_part1 = ((TreeVector)mean).getSubTreeVector("mean_part1");
		assertEquals(0.0, mean1.getValue(0), delta);
		
		IVector std = stv.getStandardDeviations();
		System.out.println("std = "+std.toString());
		assertEquals(0.1, std.getValue(0), delta);
		assertEquals(1.0, std.getValue(4), delta);
		
		StochVector.setSeed(1234567);
		IVector sample = stv.createRealization();
		System.out.println("sample = "+sample.toString());
		assertEquals(-0.08184836129163191, sample.getValue(0), delta);
		assertEquals(9.934785357659559, sample.getValue(4), delta);		
		
		ArrayList<IStochVector> children = stv.getChildren();
		IVector mean2_test = children.get(1).getExpectations();
		IVector difference = mean2_test;
		difference.axpy(-1.0, mean2);
		assertEquals(0.0,difference.norm2(),delta);
		
		double prob = stv.evaluatePdf(mean);
		System.out.println("evaluatePdf = "+prob);
		assertEquals(1.01053E-4, prob, delta);
		
		boolean correlated = stv.hasCorrelatedElements();
		assertFalse(correlated);
		
		String id = stv.getId();
		assertEquals("StochTreeVector",id);
		//TODO fix this
		// stv.getSqrtCovariance()
	}

}
