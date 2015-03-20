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

package org.costa;

import junit.framework.TestCase;
import org.openda.interfaces.*;

import java.io.File;

/**
 * Test for COSTA Components
 */
public class CostaTest extends TestCase {

	public static void testCtaVector() {

		Vector v1 = new CtaVector(3);

		// void setConstant(double value)

		v1.setConstant(1.0);
		double[] values = v1.getValues();
		assertEquals("setConstant 0", 1.0, values[0]);
		assertEquals("setConstant 1", 1.0, values[1]);
		assertEquals("setConstant 2", 1.0, values[2]);

		// void scale(double alpha)

		v1.scale(2.0);
		values = v1.getValues();
		assertEquals("scale 0", 2.0, values[0]);
		assertEquals("scale 1", 2.0, values[1]);
		assertEquals("scale 2", 2.0, values[2]);

		// void setValues(double[] values)

		double[] v1_values = { 3.0, 2.0, 1.0 };
		v1.setValues(v1_values);
		values = v1.getValues();
		assertEquals("setValues 0", 3.0, values[0]);
		assertEquals("setValues 1", 2.0, values[1]);
		assertEquals("setValues 2", 1.0, values[2]);

		// void setValues(CtaVector source)

		double[] v2_values = new double[] { 1.0, 2.0, 3.0 };
		Vector v2 = new CtaVector(v2_values.length);
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

		// double norm2()

		v2_values = new double[] { 3.0, 4.0 };
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

		v1_values = new double[] { 1.0, 2.0, 3.0 };
		v1.setValues(v1_values);
		try {
			v1.pointwiseMultiply(v1); // [1,2,3].*[1,2,3]=[1,4,9]
		} catch (Exception e) {
			assertTrue("PointwiseMultiply", e.getMessage().contains(
					"Pointwise multiply is not implemented"));
		}
	}

	public static void testCtaTime() {

		// create time stamps

		Time ctaTime1 = new CtaTime();
		Time ctaTime2 = new CtaTime();
		Time ctaTime3 = new CtaTime();

		// free the time stamps

		ctaTime1.free();
		ctaTime2.free();
		ctaTime3.free();
	}

	public static void testReltable() {

		// create a relation table, en 2 vectors to apply it to
		RelationTable relationTable = new CtaRelationTable(
				new int[] { 1, 3, 6 });
		Vector v1 = new CtaVector(6);
		Vector v2 = new CtaVector(3);
		v1.setValues(new double[] { 1.0, 2.0, 3.0, 4.0, 5.0, 6.0 });
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

	public static void testModel() {

		File modelConfigFile = new File(
				"costa/src/openda/org/costa/testData/oscill_sp.xml");
		assertTrue("FILE existence check " + modelConfigFile.getAbsolutePath(),
				modelConfigFile.exists());

        File modelClsConfigFile = new File(
				"costa/src/openda/org/costa/testData/oscill_sp_class.xml");
		assertTrue("FILE existence check " + modelClsConfigFile.getAbsolutePath(),
				modelConfigFile.exists());


        CtaOpenDaModel oscill = new CtaOpenDaModel(modelClsConfigFile.getAbsolutePath(), modelConfigFile.getAbsolutePath());

		Vector x = oscill.getState();
		System.out.print("Debug1 \n");
		int n = x.getSize();
		System.out.print("Debug2 \n");
		assertEquals("Size of state vector ", 2, n);
		assertEquals("t=0 state vector(1)", 1.0, x.getValue(0));
		assertEquals("t=0 state vector(2)", 1.0, x.getValue(1));

		CtaTime tstop = new CtaTime();
		//tstop.setSpanMJD(0.0, 1.0, 0.0);
		tstop.setMJD(1.0);
		System.out.println("tstop="+tstop.getMJD());
		
		oscill.compute(tstop);

		x = oscill.getState();
		n = x.getSize();
		assertEquals("Size of state vector ", 2, n);

		System.out.print(x.toString());
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

		Vector p = oscill.getParameters();
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
        File stochObserverDir = new File("./costa/src/openda/org/costa/testData");
		String sqliteFileName = "obs_oscill.sql";
		assertTrue("FILE existence check " + "obs_oscill.sql", new File(stochObserverDir, sqliteFileName).exists());

		StochObserver stochObserver = new CtaStochObserver();
        stochObserver.initialize(stochObserverDir, new String[]{sqliteFileName});
		CtaTime tsel = new CtaTime();
		tsel.setSpanMJD(0.99, 1, 1.0);
		StochObserver stochObservert10 = stochObserver.createSelection(tsel);
		ObservationDescriptions Obsdesrc10 = stochObservert10
				.getObservationDescriptions();
        int nobs=stochObservert10.getCount();
        assertEquals("Number of Observations ", 1, nobs);
		Vector pred10 = oscill.getObservedValues(Obsdesrc10);
		assertEquals("Number of observations at t=0.01 ", 1, pred10.getSize());
		assertEquals("predicted value t=0.01 (x(1))", 0.26637594284096, pred10
				.getValue(0), 1.e-10);

	}

	public static void testStochObserver() {

		// create a stoch observer
        // Load Stochastic observer and get observations for time is 10
        File stochObserverDir = new File("./costa/src/openda/org/costa/testData");
        String sqliteFileName = "obs_oscill.sql";
        assertTrue("FILE existence check " + "obs_oscill.sql", new File(stochObserverDir, sqliteFileName).exists());

		StochObserver stochObserver = new CtaStochObserver();
        stochObserver.initialize(stochObserverDir, new String[]{sqliteFileName});

		// getCount

		assertEquals("COUNT", 100, stochObserver.getCount());

		// getExpectations

		Vector expectations = stochObserver.getExpectations();
		assertTrue("expectations", expectations.toString().startsWith(
				"[0.3183,-0.7849,-0.791,0.2797,0.9417,0.3046,-0.7304"));
		assertTrue(
				"expectations",
				expectations
						.toString()
						.endsWith(
								"-0.2118,-0.2609,0.04643,0.2825,0.1279,-0.1968,-0.2451,0.04143,0.2641]"));

		// getRealizations

/*		Vector realizations = stochObserver.getRealizations();

        assertTrue(
				"realizations",
				realizations
						.toString()
						.startsWith(
								"[0.24185238585351376,-0.7419986574075934,-0.8899227050172487,0.19965258159861438,0.9915015332872966"));
		assertTrue(
				"realizations",
				realizations
						.toString()
						.endsWith(
								"0.09219829960645745,-0.3971217432490801,-0.39540253327936564,-0.0035842754404847785,0.19041522772026345]"));
*/
		// getStandardDeviations
		try {
			Vector standardDeviation = stochObserver.getStandardDeviations();
			assertTrue("standardDeviation", standardDeviation.toString()
					.startsWith("[0.1,0.1,0.1"));
			assertTrue("standardDeviation", standardDeviation.toString()
					.endsWith("0.1,0.1,0.1]"));
		} catch (Exception e) {
			assertTrue("standardDeviation exception",
					(e instanceof UnsupportedOperationException));
		}
	}
}
