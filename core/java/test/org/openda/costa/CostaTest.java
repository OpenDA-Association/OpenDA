/* OpenDA v2.4.3 
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

package org.openda.costa;

import junit.framework.TestCase;
import org.openda.interfaces.*;
import org.openda.resultwriters.NetcdfResultWriterNative;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Results;
import org.openda.utils.Time;

import java.io.File;
import java.io.IOException;

/**
 * Test for COSTA Components
 */
public class CostaTest extends TestCase {

    private File testRunDataDir;
    private OpenDaTestSupport testData;

    protected void setUp() throws IOException {
        testData = new OpenDaTestSupport(CostaTest.class, "core");
        testRunDataDir = testData.getTestRunDataDir();
    }


    public void testctaMessage() {

        ITime ctaTime1 = new CtaTime();
//        IVector v1 = new CtaVector(3);

        double t1 = ctaTime1.getMJD();
        System.out.println("t1="+t1);

        double[] vals1 = new double[]{1.0, 2.2, 3.0};
        IVector vec1 = new CtaVector(3);
        vec1.setValues(vals1);
        CtaTreeVector treevec1 = new CtaTreeVector("first part", "first", vec1);

        ctaTime1.free();

        try {
            // 1) this (again asking time) causes an error
            double t2 = ctaTime1.getMJD();
            System.out.println("message t2="+t2);

            // 2) wrong handles:
            double vals2 = vec1.dotProduct(treevec1);
            System.out.println("message dotproduct="+vals2);

        } catch (Exception e) {
            assertTrue(
                    e.getMessage().contains("Could not compute dot product, Costa Error Code=-3") ||
                    e.getMessage().contains("Could not getMJD , Costa Error Code=-2"));
        }
    }

    public void testCtaVector() {

        IVector v1 = new CtaVector(3);

        // void setConstant(double value)

        v1.setConstant(1.0);
        double[] values = v1.getValues();
        assertEquals("setConstant 0", 1.0, values[0]);
        assertEquals("setConstant 1", 1.0, values[1]);
        assertEquals("setConstant 2", 1.0, values[2]);
    //    System.out.print("Debug testctavector\n");

        // void scale(double alpha)

        v1.scale(2.0);
        values = v1.getValues();
        assertEquals("scale 0", 2.0, values[0]);
        assertEquals("scale 1", 2.0, values[1]);
        assertEquals("scale 2", 2.0, values[2]);

        // void setValues(double[] values)

        double[] v1_values = {3.0, 2.0, 1.0};
        v1.setValues(v1_values);
        values = v1.getValues();
        assertEquals("setValues 0", 3.0, values[0]);
        assertEquals("setValues 1", 2.0, values[1]);
        assertEquals("setValues 2", 1.0, values[2]);

        // void setValues(CtaVector source)

        double[] v2_values = new double[]{1.0, 2.0, 3.0};
        IVector v2 = new CtaVector(v2_values.length);
        v2.setValues(v2_values);
        v1.setValues(v2.getValues());
        values = v1.getValues();
        assertEquals("setValues from vector 0", 1.0, values[0]);
        assertEquals("setValues from vector 1", 2.0, values[1]);
        assertEquals("setValues from vector 2", 3.0, values[2]);

        // void setValue(int index, double value)

        v1.setValue(0, 4.0);
        values = v1.getValues();
        assertEquals("setValue 0", 4.0, values[0]);
        assertEquals("setValue 1", 2.0, values[1]);
        assertEquals("setValue 2", 3.0, values[2]);

        // double getValue(int index)

        double value = v1.getValue(0);
        assertEquals("value v1[0]", 4.0, value);

        // int getSize()

        int n = v1.getSize();
        assertEquals("v1.getSize", 3, n);

        // void axpy(double alpha, CtaVector x)

        v1.axpy(1.0, v2); // [4,2,3]+1*[1,2,3]=[5,4,6]
        values = v1.getValues();
        assertEquals("axpy 0", 5.0, values[0]);
        assertEquals("axpy 1", 4.0, values[1]);
        assertEquals("axpy 2", 6.0, values[2]);

        // double dotProduct(CtaVector otherVector)

        double dotVal = v1.dotProduct(v2); // [5,4,6]*[1,2,3]=31
        assertEquals("dot(v1,v2)", 31.0, dotVal);

        // export


        // double norm2()

        v2_values = new double[]{3.0, 4.0};
        v2 = new CtaVector(v2_values.length);
        v2.setValues(v2_values);
        double normVal = v2.norm2(); // |(3,4)|=sqrt(3*3+4*4) = 5
        assertEquals("norm2(v2)", 5.0, normVal);

        // public void pointwiseDivide(CtaVector otherVector)

        v1.pointwiseDivide(v1); // [5,4,6]./[5,4,6]=[1,1,1]
        values = v1.getValues();
        assertEquals("pointwiseDivide 0", 1.0, values[0]);
        assertEquals("pointwiseDivide 1", 1.0, values[1]);
        assertEquals("pointwiseDivide 2", 1.0, values[2]);

        // public void pointwiseMultiply(CtaVector otherVector)

        v1_values = new double[]{1.0, 2.0, 3.0};
        v1.setValues(v1_values);
        try {
            v1.pointwiseMultiply(v1); // [1,2,3].*[1,2,3]=[1,4,9]
        } catch (Exception e) {
            assertTrue("PointwiseMultiply", e.getMessage().contains(
                    "Pointwise multiply is not implemented"));
        }


    }

    public void testCtaTreevector() {

        double[] vals1 = new double[]{1.0, 2.2, 3.0, 3.14, 5.6,7.8};
        double[] vals2 = new double[]{4.0,5.2,6.0,7,7.5};
        IVector vec1 = new CtaVector(6);
        IVector vec2 = new CtaVector(5);
        vec1.setValues(vals1);
        vec2.setValues(vals2);


        // First way to create a cta-TV: using an existing cta-vector.
        CtaTreeVector v2cta_part1 = new CtaTreeVector("first part", "first", vec1);
        int nrows = 3;
        int ncols = 2;
        v2cta_part1.setReggrid(nrows,ncols,1,0.0,0.0,0.0,1.0,1.0,1.0);


        CtaTreeVector v2cta_part2 = new CtaTreeVector("second part", "second", vec2);

        // Second way to create a cta-TV: concatenation using a list of treevectors.
        //int h1 = v2cta_part1.gethandle();
        //int h2 = v2cta_part2.gethandle();
        //int[] handlelist = {h1, h2};
        CtaTreeVector[] subVectors = {v2cta_part1,  v2cta_part2};

        CtaTreeVector v2cta_tot = new CtaTreeVector("both parts", "total", subVectors);


        int size_v2 = v2cta_tot.getSize();
        assertEquals("v2cta_tot.getSize", 11, size_v2);
		int size_v2_part1 = v2cta_part2.getSize();
        assertEquals("v2cta_part2.getSize", 5, size_v2_part1);

		// now we test the export and import by netcdf. Note that the export is implemented
		// using the resultwriter; see the corresponding test for more examples!
	    IResultWriter netcdfWriter = new NetcdfResultWriterNative(testRunDataDir, "ctatvtest.nc");
        Results.addResultWriter(netcdfWriter);
        Results.putValue("concat", v2cta_tot, v2cta_tot.getSize(), "any", IResultWriter.OutputLevel.Essential , IResultWriter.MessageType.Instance);
        netcdfWriter.free();

        Results.reset();

		// now import the treevector again!
		v2cta_tot.setConstant(-1.0);

		String netcdfname = new File(testRunDataDir, "ctatvtestconcat.nc").getAbsolutePath();
		v2cta_tot.TVimport(netcdfname);
		assertEquals("first value",1.0,v2cta_tot.getValue(0));
		assertEquals("9th value",6.0,v2cta_tot.getValue(8));

		// now import the treevector again, as a flat vector!
		 IVector vecflat = new CtaVector(11);
		 CtaTreeVector v3cta_try = new CtaTreeVector("unknown structure", "flat", vecflat);
		 v3cta_try.Vimport(netcdfname);
		assertEquals("first value",1.0,v3cta_try.getValue(0));
		assertEquals("9th value",6.0,v3cta_try.getValue(8));

    }


    public void testCtaTime() {

        // create time stamps

        ITime ctaTime1 = new CtaTime();
        ITime ctaTime2 = new CtaTime(new Time(0.0));
        ITime ctaTime3 = new CtaTime(new Time(1.0,10.0));

        // set get
        double delta=0.0001;
        assertEquals(0.0,ctaTime2.getMJD(),delta);
        assertEquals(true,ctaTime2.isStamp());
        assertEquals(false,ctaTime2.isSpan());

        assertEquals(true,ctaTime3.isSpan());
        assertEquals(1.0,ctaTime3.getBeginTime().getMJD(),delta);
        assertEquals(10.0,ctaTime3.getEndTime().getMJD(),delta);
        assertEquals(9.0,ctaTime3.getStepMJD(),delta);
        // free the time stamps

        ctaTime1.free();
        ctaTime2.free();
        ctaTime3.free();
        
    }

    public void testReltable() {

        // create a relation table, en 2 vectors to apply it to
        IRelationTable relationTable = new CtaRelationTable(
                new int[]{1, 3, 6});
        IVector v1 = new CtaVector(6);
        IVector v2 = new CtaVector(3);
        v1.setValues(new double[]{1.0, 2.0, 3.0, 4.0, 5.0, 6.0});
        v2.setConstant(0.0);

        // apply
        relationTable.apply(v1, v2);
        double[] values = v2.getValues();
        assertEquals("apply 0", 1.0, values[0]);
        assertEquals("apply 1", 3.0, values[1]);
        assertEquals("apply 2", 6.0, values[2]);

        // applyInv
        v2.scale(2);
        relationTable.applyInv(v2, v1);
        values = v1.getValues();
        assertEquals("apply 0", 2.0, values[0]);
        assertEquals("apply 0", 2.0, values[1]);
        assertEquals("apply 1", 6.0, values[2]);
        assertEquals("apply 0", 4.0, values[3]);
        assertEquals("apply 2", 5.0, values[4]);
        assertEquals("apply 2", 12.0, values[5]);
    }

    public void testModel() {

        File modelConfigFile = new File(testRunDataDir, "oscill_sp.xml");
        assertTrue("FILE existence check " + modelConfigFile.getAbsolutePath(),
                modelConfigFile.exists());

        File modelClsConfigFile = new File(testRunDataDir, "oscill_sp_class.xml");
        assertTrue("FILE existence check " + modelClsConfigFile.getAbsolutePath(),
                modelConfigFile.exists());


        CtaOpenDaModel oscill = new CtaOpenDaModel(modelClsConfigFile.getAbsolutePath(), modelConfigFile.getAbsolutePath());

        IVector x = oscill.getState();

        int n = x.getSize();


        assertEquals("Size of state vector ", 2, n);
        assertEquals("t=0 state vector(1)", 1.0, x.getValue(0));
        assertEquals("t=0 state vector(2)", 1.0, x.getValue(1));

        CtaTime tstop = new CtaTime();
        //tstop.setSpanMJD(0.0, 1.0, 0.0);
        tstop.setMJD(1.0);
        System.out.println("... tstop=" + tstop.getMJD());

        oscill.compute(tstop);

        x = oscill.getState();
        n = x.getSize();
        assertEquals("Size of state vector ", 2, n);

        System.out.print(x.toString() + "\n");
        assertEquals("t=1 state vector(1)", 0.3551679237879442, x.getValue(0),
                1.e-10);
        assertEquals("t=1 state vector(2)", -11.078345912513752, x.getValue(1),
                1.e-10);

        oscill.axpyOnState(-0.25, x);
        x = oscill.getState();
        assertEquals("t=1 state after axpy vector(1)", 0.26637594284096, x
                .getValue(0), 1.e-10);
        assertEquals("t=1 state after axpy vector(2)", -8.30875943438531, x
                .getValue(1), 1.e-10);

        IVector p = oscill.getParameters();
        n = p.getSize();
        assertEquals("Size of state vector ", 2, n);
        assertEquals("model parameters(1)", 8.0, p.getValue(0), 1.e-10);
        assertEquals("model parameters(2)", 13.8230076757951, p.getValue(1),
                1.e-10);

        p.scale(0.25);
        oscill.axpyOnParameters(0.5, p);
        p = oscill.getParameters();
        assertEquals("model parameters(1)", 9, p.getValue(0), 1.e-10);
        assertEquals("model parameters(2)", 15.55088363526949, p.getValue(1),
                1.e-10);

        // Load Stochastic observer and get observations for time is 10
        String sqliteFileName = "obs_oscill.sql";
        assertTrue("FILE existence check " + "obs_oscill.sql", new File(testRunDataDir, sqliteFileName).exists());

        IStochObserver stochObserver = new CtaStochObserver();
        stochObserver.initialize(testRunDataDir, new String[]{sqliteFileName});
        CtaTime tsel = new CtaTime();
        tsel.setSpanMJD(0.99, 1, 1.0);
        IStochObserver stochObservert10 = stochObserver.createSelection(tsel);
        IObservationDescriptions Obsdesrc10 = stochObservert10
                .getObservationDescriptions();
        int nobs = stochObservert10.getCount();
        assertEquals("Number of Observations ", 1, nobs);
        IVector pred10 = oscill.getObservedValues(Obsdesrc10);
        assertEquals("Number of observations at t=0.01 ", 1, pred10.getSize());
        assertEquals("predicted value t=0.01 (x(1))", 0.26637594284096, pred10
                .getValue(0), 1.e-10);

    }

    public void testStochObserver() {


        // create a stoch observer
        // Load Stochastic observer and get observations for time is 10
        String sqliteFileName = "obs_oscill.sql";
        assertTrue("FILE existence check " + "obs_oscill.sql", new File(testRunDataDir, sqliteFileName).exists());

        IStochObserver stochObserver = new CtaStochObserver();
        stochObserver.initialize(testRunDataDir, new String[]{sqliteFileName});

        // getCount

        assertEquals("COUNT", 100, stochObserver.getCount());

        // getExpectations

        IVector expectations = stochObserver.getExpectations();
        assertTrue("expectations", expectations.toString().startsWith(
                "[0.3183,-0.7849,-0.791,0.2797,0.9417,0.3046,-0.7304"));
        assertTrue(
                "expectations",
                expectations
                        .toString()
                        .endsWith(
                        "-0.2118,-0.2609,0.04643,0.2825,0.1279,-0.1968,-0.2451,0.04143,0.2641]"));

        // getStandardDeviations
        try {
            IVector standardDeviation = stochObserver.getStandardDeviations();

            assertTrue("standardDeviation", standardDeviation.toString()
                    .startsWith("[0.1,0.1,0.1"));
            //		assertTrue("standardDeviation", standardDeviation.toString()
            //				.endsWith("0.1,0.1,0.1]"));
        } catch (Exception e) {
            assertTrue("standardDeviation exception",
                    (e instanceof UnsupportedOperationException));
        }


        // test the observation descriptions
        IObservationDescriptions sobsDescr = stochObserver.getObservationDescriptions();

        int nObs = sobsDescr.getObservationCount();
        assertEquals("number of observations", 100, nObs);

        int nKeys = sobsDescr.getPropertyCount();
        assertEquals("number of properties", 6, nKeys);

        String keys[] = sobsDescr.getPropertyKeys();

        assertEquals("Key 1", "STATION_ID", keys[0]);
        assertEquals("Key 2", "NAME", keys[1]);
        assertEquals("Key 3", "UNIT", keys[2]);
        assertEquals("Key 4", "STANDARDDEVIATION", keys[3]);
        assertEquals("Key 5", "TIME", keys[4]);
        assertEquals("Key 6", "INDX", keys[5]);



        String names[] = sobsDescr.getStringProperties("NAME");
        assertEquals("NAME", "Station 1", names[0]);
        assertEquals("NAME", "Station 1", names[99]);

        IVector times = sobsDescr.getValueProperties("TIME");
        assertEquals("NAME", 0.1, times.getValue(0), 1e-10);
        assertEquals("NAME", 0.2, times.getValue(1), 1e-10);
        assertEquals("NAME", 10.0, times.getValue(99), 1e-10);

        // Wrap the observation description into a native one
        IObservationDescriptions obsDescrNativeToJava = new CtaObservationDescriptions(stochObserver.getObservationDescriptions());

        int nObs2 = obsDescrNativeToJava.getObservationCount();
        assertEquals("number of observations", 100, nObs2);

        int nKeys2 = obsDescrNativeToJava.getPropertyCount();
        assertEquals("number of properties", 6, nKeys2);

        String keys2[] = obsDescrNativeToJava.getPropertyKeys();

        assertEquals("Key 1", "STATION_ID", keys2[0]);
        assertEquals("Key 2", "NAME", keys2[1]);
        assertEquals("Key 3", "UNIT", keys2[2]);
        assertEquals("Key 4", "STANDARDDEVIATION", keys2[3]);
        assertEquals("Key 5", "TIME", keys2[4]);
        assertEquals("Key 6", "INDX", keys2[5]);

        String names2[] = obsDescrNativeToJava.getStringProperties("NAME");
        assertEquals("NAME", "Station 1", names2[0]);
        assertEquals("NAME", "Station 1", names2[99]);

        IVector times2 = obsDescrNativeToJava.getValueProperties("TIME");
        assertEquals("NAME", 0.1, times2.getValue(0), 1e-10);
        assertEquals("NAME", 0.2, times2.getValue(1), 1e-10);
        assertEquals("NAME", 10.0, times2.getValue(99), 1e-10);

    }
    
}
