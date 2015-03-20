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

/**
 * TODO: class description
 * Main class for testing vector class
 */


package org.openda.utils;

import java.io.File;
import java.io.IOException;
import junit.framework.TestCase;
import org.openda.interfaces.*;
import org.openda.utils.StochVector;
import org.openda.utils.Vector;


/**
 * Main class for testing StochVector
 */
public class StochVectorTest extends TestCase{

	private File testRunDataDir;
	private OpenDaTestSupport testData;

	protected void setUp() throws IOException {
		testData = new OpenDaTestSupport(CsvStochObserverTest.class,"core");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public static void testStochVector_1() {

		//    public StochVector(Vector mean, Vector std){
		//    public String toString(){
		System.out.println("==============================================================================");
		IVector mean1 = new Vector("[0.0,0.0]");
		IVector std1  = new Vector("[0.1,0.1]");
		StochVector sv1= new StochVector(mean1,std1);
		StochVector.setSeed(1234567);
		System.out.println("sv1 = "+sv1.toString());  //explicit call to toString() is not needed
		System.out.println("should be sv1 = {[0.0,0.0],[0.1,0.1]}");
		//    public StochVector(String meanstring, String stdstring){
		System.out.println("==============================================================================");
		String mean2string = "[1.0,1.0]";
		String std2string  = "[0.2,0.3]";
		IStochVector sv2= new StochVector(mean2string,std2string);
		System.out.println("sv2 = "+sv2);
		System.out.println("should be sv2 = {[1.0,1.0],[0.2,0.3]}");
		//    public StochVector(String valueString){
		System.out.println("==============================================================================");
		IStochVector sv3= new StochVector("{[2.0,2.0],[0.4,0.5]}");
		System.out.println("sv3 = "+sv3); 
		System.out.println("should be sv3 = {[2.0,2.0],[0.4,0.5]}");
		//    public double evaluatePdf(Vector tv)
		System.out.println("==============================================================================");
		IVector v1= new Vector("[0.2,0.2]");
		IVector v2= new Vector("[-0.2,-0.2]");
		double p1 = sv1.evaluatePdf(v1);
		double p2 = sv1.evaluatePdf(v2);
		System.out.println("prob_sv1(v1) = "+p1); 
		System.out.println("prob_sv1(v2) = "+p2); 
		System.out.println("should be prob_sv1(v1) = 0.29150244650281937");
		System.out.println("should be prob_sv1(v2) = 0.29150244650281937"); //exactly same bacause of symmetry
		//    public Vector getExpectations()
		System.out.println("==============================================================================");
		System.out.println("mean(sv1) = "+sv1.getExpectations());
		System.out.println("Should be mean(sv1) = [0.0,0.0]"); 
		//    public Vector getStandardDeviations()
		System.out.println("==============================================================================");
		System.out.println("std(sv1) = "+sv1.getStandardDeviations());
		System.out.println("Should be std(sv1) = [0.1,0.1]"); 
		//    public boolean hasCorrelatedElements()
		System.out.println("==============================================================================");
		boolean test = sv1.hasCorrelatedElements();
		if(test){
			System.out.println("correlated(sv1)=true");
		}else{
			System.out.println("correlated(sv1)=false");
		}
		System.out.println("Should be uncorrelated(sv1)=true");
		//    public Vector createRealization()
		System.out.println("==============================================================================");
		IVector sample1 = sv1.createRealization();
		System.out.println("a sample of sv1 x = "+sample1);
		System.out.println("Should be [-0.08184836129163191,0.0802098628209642]"); 
		assertEquals("[-0.08184836129163191,0.0802098628209642]", sample1.toString());
		//    public SqrtCovariance getSqrtCovariance()
		System.out.println("==============================================================================");
		ISqrtCovariance sq = sv1.getSqrtCovariance();
		System.out.println("sqrt(cov(sv1)) = "+sq); 
		System.out.println("Should be sqrt(cov(sv1)) = diag([0.1,0.1])"); 
	}
	
	public static void testStochVector_2() {
		//    public StochVector(Vector mean, Vector std){
		//    public String toString(){
		System.out.println("==============================================================================");
		IVector mean1 = new Vector("[0.0,0.0]");
		IVector std1  = new Vector("[0.1,0.1]");
		StochVector sv1= new StochVector(mean1,std1);
		StochVector.setSeed(123456789);
		IVector sample1_sv1 = sv1.createRealization();
		IVector sample2_sv1 = sv1.createRealization();
		StochVector sv2= new StochVector(mean1,std1);
		StochVector.setSeed(123456789);
		IVector sample1_sv2 = sv2.createRealization();
		IVector sample2_sv2 = sv2.createRealization();
		
		System.out.println("sv1 1st sample = "+sample1_sv1.toString());
		System.out.println("Should be sv1 1st sample = [0.2011512265750413,-0.052787741959320233]");
		assertEquals("[0.2011512265750413,-0.052787741959320233]", sample1_sv1.toString());
		System.out.println("sv2 1st sample = "+sample1_sv2.toString());
		System.out.println("Should be sv2 1st sample = [0.2011512265750413,-0.052787741959320233]");
		assertEquals("[0.2011512265750413,-0.052787741959320233]", sample1_sv2.toString());
		
		System.out.println("sv1 2nd sample = "+sample2_sv1.toString());
		System.out.println("Should be sv1 2nd sample = [-0.024142246805715386,0.08672786586477418]");
		assertEquals("[-0.024142246805715386,0.08672786586477418]", sample2_sv1.toString());
		System.out.println("sv2 2nd sample = "+sample2_sv2.toString());
		System.out.println("Should be sv2 2nd sample = [-0.024142246805715386,0.08672786586477418]");
		assertEquals("[-0.024142246805715386,0.08672786586477418]", sample2_sv2.toString());
	}

	public static void testStochVector_3() {
		//    check statistics
		System.out.println("==============================================================================");
		IVector mean1 = new Vector("[0.0,0.0]");
		IVector std1  = new Vector("[1.0,0.1]");
		IVector sampleSum = new Vector(2);
		IVector sampleSumSqr = new Vector(2);
		StochVector sv1= new StochVector(mean1,std1);
		StochVector.setSeed(10);
		IVector sample=null;
		int n=10000; //sample size
		for(int i=0;i<n;i++){
			sample = sv1.createRealization();
			sampleSum.axpy(1.0, sample);
			sample.pointwiseMultiply(sample); // sample^2
			sampleSumSqr.axpy(1.0, sample);
		}
		
		IVector sampleMean = sampleSum.clone();
		sampleMean.scale(1.0/n);
		System.out.println("sample mean = "+sampleMean);
		System.out.println("Should be approx mean = [0.0,0.0]");
		assertEquals(0.0, sampleMean.getValue(0), 0.01);
		assertEquals(0.0, sampleMean.getValue(1), 0.001);
		IVector sampleRMS = sampleSumSqr.clone();
		sampleRMS.scale(1.0/n);
		sampleRMS.sqrt();
		System.out.println("sample rms = "+sampleRMS);
		System.out.println("Should be rms = [1.0,0.1]");
		assertEquals(1.0, sampleRMS.getValue(0), 0.01);
		assertEquals(0.1, sampleRMS.getValue(1), 0.001);
		
	}

	public static void testStochVector_4() {
		System.out.println("==============================================================================");
		System.out.println("Correlated elements");
		System.out.println("==============================================================================");

		IVector mean = new Vector("[0.0,0.0,0.0,0.0,0.0]");
		IMatrix cov = new Matrix("[1.0,0.6065506034577204,0.3679223128686224,0.6065506034577204,1.0;0.6065506034577204,1.0,0.6065506034577204,0.3679223128686224,0.6065506034577204;0.3679223128686224,0.6065506034577204,1.0,0.606642980320629,0.3679223128686224;0.6065506034577204,0.3679223128686224,0.606642980320629,1.0,0.6065506034577204;1.0,0.6065506034577204,0.3679223128686224,0.6065506034577204,1.0]");
		boolean isSquareRoot = false;
		StochVector sv= new StochVector(mean, cov, isSquareRoot);
		
    	IVector mean2 = sv.getExpectations();
    	System.out.println("mean="+mean2);
    	System.out.println("Should be mean=[0.0,0.0,0.0,0.0,0.0]");
    	assertEquals("[0.0,0.0,0.0,0.0,0.0]",mean2.toString());
    	
    	IVector std = sv.getStandardDeviations();
    	System.out.println("std="+std);
    	System.out.println("Should be std=[1.0,1.0,1.0,1.0,1.0]");
    	assertEquals("[1.0,1.0,1.0,1.0,1.0]",std.toString());
    	
    	ISqrtCovariance sqrtCov = sv.getSqrtCovariance();
    	System.out.println("sqrtCov="+sqrtCov);
    	System.out.println("Should be sqrtCov=full([0.6565208964327816,0.25334955112875074,0.09792247783227047,0.2533492024306464,0.6565208964327819;0.2533495511287502,0.8802206825124356,0.2993063802412671,0.08517775947942266,0.25334955112875024;0.09792247783227007,0.2993063802412674,0.8953317495585582,0.2993645363927716,0.09792247783227029;0.25334920243064585,0.08517775947942272,0.2993645363927717,0.8802011059388181,0.25334920243064596;0.6565208964327817,0.2533495511287503,0.0979224778322704,0.2533492024306458,0.656520896432782])");
    	assertEquals("full([0.6565208964327816,0.25334955112875074,0.09792247783227047,0.2533492024306464,0.6565208964327819;0.2533495511287502,0.8802206825124356,0.2993063802412671,0.08517775947942266,0.25334955112875024;0.09792247783227007,0.2993063802412674,0.8953317495585582,0.2993645363927716,0.09792247783227029;0.25334920243064585,0.08517775947942272,0.2993645363927717,0.8802011059388181,0.25334920243064596;0.6565208964327817,0.2533495511287503,0.0979224778322704,0.2533492024306458,0.656520896432782])",sqrtCov.toString());
    	
    	StochVector.setSeed(123456);
    	IVector random = sv.createRealization();
    	System.out.println("random="+random);
    	System.out.println("Should be random=[-0.3838887644308977,-0.3290483731175597,-0.7023584661001218,0.24635798307523635,-0.3838887644308983]");
    	assertEquals("[-0.3838887644308977,-0.3290483731175597,-0.7023584661001218,0.24635798307523635,-0.3838887644308983]",random.toString());

	}
}

