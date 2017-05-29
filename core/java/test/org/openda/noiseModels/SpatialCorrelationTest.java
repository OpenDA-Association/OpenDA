/* OpenDA v2.3.1 
* Copyright (c) 2016 OpenDA Association 
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
package org.openda.noiseModels;
import junit.framework.TestCase;
import org.openda.interfaces.ISqrtCovariance;
import org.openda.interfaces.IVector;
import org.openda.noiseModels.SpatialCorrelationStochVector.CoordinatesType;
import org.openda.utils.Matrix;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.StochVector;
import org.openda.utils.Vector;

import java.io.File;
import java.io.IOException;

public class SpatialCorrelationTest extends TestCase {

    //File testDir = null;
    private File testRunDataDir;

    protected void setUp() throws IOException {
        OpenDaTestSupport testData = new OpenDaTestSupport(TimeSeriesNoiseModelTest.class, "core");
        testRunDataDir = testData.getTestRunDataDir();
    }

    public void testSpatialCorrelation_xy(){
    	double standardDeviation=1.0; 
		double lengthscale=1.0; 
		double[] x= new double[]{0.0,1.0,1.0,0.0,0.0}; //counterclock square 
		double[] y= new double[]{0.0,0.0,1.0,1.0,0.0};
    	SpatialCorrelationStochVector sv = new SpatialCorrelationStochVector(CoordinatesType.XY
    			, standardDeviation, lengthscale, x, y, null);
    	IVector mean = sv.getExpectations();
    	System.out.println("mean="+mean);
    	System.out.println("Should be mean=[0.0,0.0,0.0,0.0,0.0]");
    	assertEquals("[0.0,0.0,0.0,0.0,0.0]",mean.toString());
    	
    	IVector std = sv.getStandardDeviations();
    	System.out.println("std="+std);
    	System.out.println("Should be std=[1.0,1.0,1.0,1.0,1.0]");
    	assertEquals("[1.0,1.0,1.0,1.0,1.0]",std.toString());
    	
    	ISqrtCovariance sqrtCov = sv.getSqrtCovariance();
    	System.out.println("sqrtCov="+sqrtCov);
    	System.out.println("Should be sqrtCov=full([0.6565246076963052,0.2533417300316849,0.09791228026928459,0.25334173003168486,0.6565246076963053;0.253341730031685,0.8802290355167743,0.2992975654761666,0.08516893789612522,0.25334173003168503;0.09791228026928409,0.29929756547616676,0.8953593163334019,0.2992975654761668,0.09791228026928411;0.2533417300316849,0.08516893789612498,0.2992975654761666,0.8802290355167746,0.25334173003168486;0.6565246076963053,0.2533417300316851,0.09791228026928445,0.25334173003168503,0.6565246076963053])");
    	assertEquals("full([0.6565246076963052,0.2533417300316849,0.09791228026928459,0.25334173003168486,0.6565246076963053;0.253341730031685,0.8802290355167743,0.2992975654761666,0.08516893789612522,0.25334173003168503;0.09791228026928409,0.29929756547616676,0.8953593163334019,0.2992975654761668,0.09791228026928411;0.2533417300316849,0.08516893789612498,0.2992975654761666,0.8802290355167746,0.25334173003168486;0.6565246076963053,0.2533417300316851,0.09791228026928445,0.25334173003168503,0.6565246076963053])",sqrtCov.toString());
    	
    	StochVector.setSeed(123456);
    	IVector random = sv.createRealization();
    	System.out.println("random="+random);
    	System.out.println("Should be random=[-0.3838886494002265,-0.32903950436630147,-0.7024362227358004,0.24645496286476012,-0.38388864940022616]");
    	assertEquals("[-0.3838886494002265,-0.32903950436630147,-0.7024362227358004,0.24645496286476012,-0.38388864940022616]",random.toString());
    	
    	IVector someVector1 = new Vector("[0.0,0.0,0.0,0.0,0.0]");
    	IVector someVector2 = new Vector("[1.0,0.0,0.0,0.0,0.0]");
    	double pdfVal1 = sv.evaluatePdf(someVector1);
    	double pdfVal2 = sv.evaluatePdf(someVector2);
    	assertEquals(1423371.952647495, pdfVal1, 0.000001);
    	assertEquals(1041016.2334227595, pdfVal2, 0.000001);
    }

    public void testSpatialCorrelation_latlon(){
    	double standardDeviation=1.0; 
		double lengthscale=1.1123e+05; //approx 1 degree at the equator
		double[] x= new double[]{0.0,1.0,1.0,0.0}; //counterclock square 
		double[] y= new double[]{0.0,0.0,1.0,1.0};
    	SpatialCorrelationStochVector sv = new SpatialCorrelationStochVector(CoordinatesType.WGS84
    			, standardDeviation, lengthscale, x, y, null);
    	IVector mean = sv.getExpectations();
    	System.out.println("mean="+mean);
    	System.out.println("Should be mean=[0.0,0.0,0.0,0.0]");
    	assertEquals("[0.0,0.0,0.0,0.0]",mean.toString());
    	
    	IVector std = sv.getStandardDeviations();
    	System.out.println("std="+std);
    	System.out.println("Should be std=[1.0,1.0,1.0,1.0]");
    	assertEquals("[1.0,1.0,1.0,1.0]",std.toString());
    	
    	ISqrtCovariance sqrtCov = sv.getSqrtCovariance();
    	System.out.println("sqrtCov          ="+sqrtCov.toString());
    	System.out.println("Should be sqrtCov=full([0.8975226901358058,0.30327456509234374,0.1024797387051972,0.3032745653930625;0.30327456509234385,0.897522690135806,0.30327456539306225,0.10247973870519705;0.10247973870519728,0.30327456539306197,0.8975030567707566,0.3033326626456696;0.3032745653930621,0.10247973870519714,0.3033326626456696,0.8975030567707568])");
		assertTrue(sqrtCov.toString().matches("(.*)0.8975(.*)0.3032(.*)0.89750(.*)"));
    	
    	StochVector.setSeed(123456);
    	IVector random = sv.createRealization();
    	System.out.println("random          ="+random);
    	System.out.println("Should be random=[0.17025813743181678,-0.11842521482517256,-0.623919347942582,0.45698089244182905]");
		assertTrue(random.toString().matches("(.*)0.1702(.*)-0.1184(.*)-0.6239(.*)0.4569(.*)"));


		//assertEquals("[-0.3838887644308977,-0.3290483731175597,-0.7023584661001218,0.24635798307523635,-0.3838887644308983]",random.toString());
    	
    	IVector someVector1 = new Vector("[0.0,0.0,0.0,0.0]");
    	IVector someVector2 = new Vector("[1.0,0.0,0.0,0.0]");
    	double pdfVal1 = sv.evaluatePdf(someVector1);
    	double pdfVal2 = sv.evaluatePdf(someVector2);
    	assertEquals(0.063404, pdfVal1, 0.0001);
    	assertEquals(0.018139, pdfVal2, 0.0001);
    }

    public void testSpatialCorrelation_xy_separable(){
    	double standardDeviation=1.0; 
		double lengthscale=1.0; 
		double[] x= new double[]{0.0,1.0,2.0}; //counterclock square 
		double[] y= new double[]{0.0,0.5,1.0,1.5,2.0};
    	Spatial2DCorrelationStochVector sv = new Spatial2DCorrelationStochVector(CoordinatesType.XY
    			, standardDeviation, lengthscale, x, y, null);
    	System.out.println("sv="+sv.toString());
    	
    	IVector mean = sv.getExpectations();
    	System.out.println("mean="+mean);
    	System.out.println("Should be mean=[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]");
    	assertEquals("[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]",mean.toString());
    	
    	IVector std = sv.getStandardDeviations();
    	System.out.println("std="+std);
    	System.out.println("Should be std=[1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0]");
    	assertEquals("[1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0]",std.toString());
//    	
//    	ISqrtCovariance sqrtCov = sv.getSqrtCovariance();
//    	System.out.println("sqrtCov="+sqrtCov);
//    	System.out.println("Should be sqrtCov=full([0.6565246076963052,0.2533417300316849,0.09791228026928459,0.25334173003168486,0.6565246076963053;0.253341730031685,0.8802290355167743,0.2992975654761666,0.08516893789612522,0.25334173003168503;0.09791228026928409,0.29929756547616676,0.8953593163334019,0.2992975654761668,0.09791228026928411;0.2533417300316849,0.08516893789612498,0.2992975654761666,0.8802290355167746,0.25334173003168486;0.6565246076963053,0.2533417300316851,0.09791228026928445,0.25334173003168503,0.6565246076963053])");
//    	assertEquals("full([0.6565246076963052,0.2533417300316849,0.09791228026928459,0.25334173003168486,0.6565246076963053;0.253341730031685,0.8802290355167743,0.2992975654761666,0.08516893789612522,0.25334173003168503;0.09791228026928409,0.29929756547616676,0.8953593163334019,0.2992975654761668,0.09791228026928411;0.2533417300316849,0.08516893789612498,0.2992975654761666,0.8802290355167746,0.25334173003168486;0.6565246076963053,0.2533417300316851,0.09791228026928445,0.25334173003168503,0.6565246076963053])",sqrtCov.toString());
    	
    	StochVector.setSeed(123456);
    	IVector random = sv.createRealization();
    	System.out.println("random="+random);
    	System.out.println("Should be random=[-0.18470148609243653,-0.4243025853012393,-0.7470415109511204,-0.6902814525267743,-0.48131443880609914,-0.41351114727154226,-1.1856708530898912,-0.4593820257330019,-0.27045679947294626,-1.1746816322632847,-0.410053720110155,-0.43345997651052937,-0.9621107293271047,-0.31130632688157717,-0.48220431615687154]");
    	assertEquals("[-0.18470148609243653,-0.4243025853012393,-0.7470415109511204,-0.6902814525267743,-0.48131443880609914,-0.41351114727154226,-1.1856708530898912,-0.4593820257330019,-0.27045679947294626,-1.1746816322632847,-0.410053720110155,-0.43345997651052937,-0.9621107293271047,-0.31130632688157717,-0.48220431615687154]",random.toString());
    	
//    	IVector someVector1 = new Vector("[0.0,0.0,0.0,0.0,0.0]");
//    	IVector someVector2 = new Vector("[1.0,0.0,0.0,0.0,0.0]");
//    	double pdfVal1 = sv.evaluatePdf(someVector1);
//    	double pdfVal2 = sv.evaluatePdf(someVector2);
//    	assertEquals(1423371.952647495, pdfVal1, 0.000001);
//    	assertEquals(1041016.2334227595, pdfVal2, 0.000001);
    }

    public void testSpatialCorrelation_latlon_separable(){
    	double standardDeviation=1.0; 
		double lengthscale=1.1123e+05; //approx 1 degree at the equator
		double[] x= new double[]{0.0,0.5,1.0}; //counterclock square 
		double[] y= new double[]{50.0,50.5,51.0,51.5,52.0};
    	Spatial2DCorrelationStochVector sv = new Spatial2DCorrelationStochVector(CoordinatesType.WGS84
    			, standardDeviation, lengthscale, x, y, null);
    	IVector mean = sv.getExpectations();
    	System.out.println("mean="+mean);
    	System.out.println("Should be mean=[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]");
    	assertEquals("[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]",mean.toString());
    	
    	IVector std = sv.getStandardDeviations();
    	System.out.println("std="+std);
    	System.out.println("Should be std=[1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0]");
    	assertEquals("[1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0,1.0]",std.toString());
    	
//    	ISqrtCovariance sqrtCov = sv.getSqrtCovariance();
//    	System.out.println("sqrtCov="+sqrtCov);
//    	System.out.println("Should be sqrtCov=full([0.6565208964327816,0.25334955112875074,0.09792247783227047,0.2533492024306464,0.6565208964327819;0.2533495511287502,0.8802206825124356,0.2993063802412671,0.08517775947942266,0.25334955112875024;0.09792247783227007,0.2993063802412674,0.8953317495585582,0.2993645363927716,0.09792247783227029;0.25334920243064585,0.08517775947942272,0.2993645363927717,0.8802011059388181,0.25334920243064596;0.6565208964327817,0.2533495511287503,0.0979224778322704,0.2533492024306458,0.656520896432782])");
//    	assertEquals("full([0.6565208964327816,0.25334955112875074,0.09792247783227047,0.2533492024306464,0.6565208964327819;0.2533495511287502,0.8802206825124356,0.2993063802412671,0.08517775947942266,0.25334955112875024;0.09792247783227007,0.2993063802412674,0.8953317495585582,0.2993645363927716,0.09792247783227029;0.25334920243064585,0.08517775947942272,0.2993645363927717,0.8802011059388181,0.25334920243064596;0.6565208964327817,0.2533495511287503,0.0979224778322704,0.2533492024306458,0.656520896432782])",sqrtCov.toString());
    	
    	StochVector.setSeed(123456);
    	IVector random = sv.createRealization();
    	System.out.println("random="+random);
    	System.out.println("Should be random=[-0.34224376463514894,-0.5471509120380801,-0.721557687563423,-0.7187376815392015,-0.6367791741607153,-0.5320408215409327,-1.0947516998846778,-0.7461838003077389,-0.47740724932538137,-1.0965698466890843,-0.7720753207277553,-0.5965704175276043,-0.9095678300091019,-0.6629700246540922,-0.5858321039255701]");
    	assertEquals("[-0.43522225921854135,-0.5736106703425949,-0.6915230368680412,-0.7249818124258252,-0.6713033884574707,-0.5988315513627083,-1.0365193667843033,-0.8178740048487827,-0.6193823908081495,-1.0582441686584545,-0.8657383563211645,-0.7203967162657682,-0.8934236761723015,-0.7555772266098759,-0.6746768458224316]",random.toString());
    	
//    	IVector someVector1 = new Vector("[0.0,0.0,0.0,0.0,0.0]");
//    	IVector someVector2 = new Vector("[1.0,0.0,0.0,0.0,0.0]");
//    	double pdfVal1 = sv.evaluatePdf(someVector1);
//    	double pdfVal2 = sv.evaluatePdf(someVector2);
//    	assertEquals(1343747.538109409, pdfVal1, 0.000001);
//    	assertEquals(982746.9501046452, pdfVal2, 0.000001);
    }

    public void testSpatialCorrelation_latlon_separable_stats(){
    	System.out.println("==================================================================");
    	System.out.println("test 2d stats for separable case");
    	System.out.println("==================================================================");

    	double standardDeviation=0.5; 
		double lengthscale=1.1123e+05; //approx 1 degree at the equator
		double[] x= new double[]{0.0,0.5,1.0}; //2 separate axis 
		double[] y= new double[]{50.0,51.0};
    	Spatial2DCorrelationStochVector sv = new Spatial2DCorrelationStochVector(CoordinatesType.WGS84
    			, standardDeviation, lengthscale, x, y, null);
    	IVector mean = sv.getExpectations();
    	
    	IVector std = sv.getStandardDeviations();
    	
    	int nSamples=1000;
    	int n=mean.getSize();
    	IVector avg=new Vector(n);
    	Matrix cov=new Matrix(n,n);
    	StochVector.setSeed(123456);
    	for(int i=1;i<=nSamples;i++){
        	IVector sample = sv.createRealization();
        	//System.out.println("   sample("+i+",:)="+sample.toString());
        	avg.axpy(1.0, sample);
        	cov.axpy(1.0, Matrix.outer(sample,sample)); // cov+=sample*sample'
    	}
    	avg.scale(1./nSamples);
    	System.out.println("average="+avg);
    	System.out.println("expected_value="+mean);
    	cov.scale(1./nSamples);
    	System.out.println("Ps="+cov.toString());
    	
    }

    public void testSpatialCorrelation_latlon_nonseparable_stats(){
    	System.out.println("==================================================================");
    	System.out.println("test 2d stats for non-separable case");
    	System.out.println("==================================================================");
    	
    	double standardDeviation=0.5; 
		double lengthscale=1.1123e+05; //approx 1 degree at the equator
		double[] x= new double[]{0.0,0.5,1.0,0.0,0.5,1.0}; // pairs
		double[] y= new double[]{50.0,50.0,50.0,51.0,51.0,51.0};
    	SpatialCorrelationStochVector sv = new SpatialCorrelationStochVector(CoordinatesType.WGS84
    			, standardDeviation, lengthscale, x, y, null);
    	IVector mean = sv.getExpectations();
    	
    	IVector std = sv.getStandardDeviations();
    	
    	int nSamples=1000;
    	int n=mean.getSize();
    	IVector avg=new Vector(n);
    	Matrix cov=new Matrix(n,n);
    	StochVector.setSeed(123456);
    	for(int i=1;i<=nSamples;i++){
        	IVector sample = sv.createRealization();
        	//System.out.println("   sample("+i+",:)="+sample.toString());
        	avg.axpy(1.0, sample);
        	cov.axpy(1.0, Matrix.outer(sample,sample)); // cov+=sample*sample'
    	}
    	avg.scale(1./nSamples);
    	System.out.println("average="+avg);
    	System.out.println("expected_value="+mean);
    	cov.scale(1./nSamples);
    	System.out.println("Pn="+cov.toString());
    	
    }

}
