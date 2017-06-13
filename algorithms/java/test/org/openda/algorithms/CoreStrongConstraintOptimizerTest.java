/* OpenDA v2.4 
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
package org.openda.algorithms;
import junit.framework.TestCase;
import org.openda.interfaces.*;
import org.openda.models.oscillator.OscillatorStochModelFactory;
import org.openda.observers.NoosTimeSeriesStochObserver;
import org.openda.utils.OpenDaTestSupport;
import org.openda.utils.Vector;

import java.io.File;
import java.io.IOException;
//import org.openda.algorithms.*;

public class CoreStrongConstraintOptimizerTest extends TestCase {

	private static File testRunDataDir;

	public static void testDummy(){}
	protected void setUp() throws IOException {
		OpenDaTestSupport testData = new OpenDaTestSupport(CalibrationAlgorithmRestartTest.class, "algorithms");
		testRunDataDir = testData.getTestRunDataDir();
	}

	public static void testDudWithStrongConstraint_upperBounds() {
		System.out.println("========================================================");
		System.out.println("Dud minimization of kwadratic least-squares cost ");
		System.out.println("Testing upper bounds");
		System.out.println("========================================================");
		// Dud test on simple quadratic cost
		LeastSquaresCostFunction f1 = new SimpleLeastSquaresCostFunction();
		f1.setBackgroundTerm(true);
		IVector p1 = new Vector("[0.0,0.0]");
		DudCoreOptimizer d1 = new DudCoreOptimizer(f1);
		d1.lower="[-10.0,-10.0]";
		d1.upper="[ 0.3, 0.4]";
		d1.add_constraints=true;
        d1.initialize(p1);
		d1.absTol = 0.001;
		d1.relTol = 0.001;
		
		d1.optimize();
		IVector p1b = d1.getOptimalValue();
		System.out.println("d1.getOptimalValue() = "+p1b);
		System.out.println("Should be d1.getOptimalValue() = [0.2999999999999999,0.4]");
		assertEquals("s2.getOptimalValue()",""+p1b.toString(), "[0.2999999999999999,0.4]");
		double f1b = d1.getOptimalCost();
		System.out.println("d1.getOptimalCost() = "+f1b);
		System.out.println("Should be s2.getOpimalCosts()[1] = 0.7100000000000002");
		assertEquals("s2.getOptimalCost()",""+f1b, "0.7100000000000002");
		IVector[] p1c = d1.getCurrentValues();
		double[] f1c = d1.getCurrentCosts();
		for(int i=0;i<p1c.length;i++){
			System.out.println("par["+i+"]="+p1c[i]);
			System.out.println("cost["+i+"]="+f1c[i]);
		}
		
	}

	public static void testDudWithStrongConstraint_upperBounds_2() {
		System.out.println("========================================================");
		System.out.println("Dud minimization of kwadratic least-squares cost ");
		System.out.println("Testing upper bounds where initial parameter values equal ");
		System.out.println("to the upper bounds");
		System.out.println("========================================================");
		// Dud test on simple quadratic cost
		LeastSquaresCostFunction f1 = new SimpleLeastSquaresCostFunction();
		f1.setBackgroundTerm(true);
		IVector p1 = new Vector("[0.45,0.21]");
		DudCoreOptimizer d1 = new DudCoreOptimizer(f1);
		d1.upper="[ 0.45, 0.21]";
		d1.add_constraints=true;
		d1.initialize(p1);
		d1.absTol = 0.001;
		d1.relTol = 0.001;

		d1.optimize();
		IVector p1b = d1.getOptimalValue();
		System.out.println("d1.getOptimalValue() = "+p1b);
		System.out.println("Should be d1.getOptimalValue() = [0.45,0.21]");
		assertEquals("s2.getOptimalValue()",""+p1b.toString(), "[0.45,0.21]");
		double f1b = d1.getOptimalCost();
		System.out.println("d1.getOptimalCost() = "+f1b);
		System.out.println("Should be s2.getOpimalCosts()[1] = 0.67205");
		assertEquals("s2.getOptimalCost()",""+f1b, "0.67205");
		IVector[] p1c = d1.getCurrentValues();
		double[] f1c = d1.getCurrentCosts();
		for(int i=0;i<p1c.length;i++){
			System.out.println("par["+i+"]="+p1c[i]);
			System.out.println("cost["+i+"]="+f1c[i]);
		}

	}

	public static void testDudWithStrongConstraint_lowerBounds() {
		System.out.println("========================================================");
		System.out.println("Dud minimization of kwadratic least-squares cost ");
		System.out.println("Testing lower bounds");
		System.out.println("========================================================");
		// Dud test on simple quadratic cost
		LeastSquaresCostFunction f1 = new SimpleLeastSquaresCostFunction();
		f1.setBackgroundTerm(true);
		IVector p1 = new Vector("[20.0,200.0]");
		DudCoreOptimizer d1 = new DudCoreOptimizer(f1);
		d1.lower="[  0.7,  0.6]";
		d1.upper="[ 10.0, 10.0]";
		d1.add_constraints=true;
        d1.initialize(p1);
		d1.absTol = 0.001;
		d1.relTol = 0.001;
		
		d1.optimize();
		IVector p1b = d1.getOptimalValue();
		System.out.println("d1.getOptimalValue() = "+p1b);
		System.out.println("Should be d1.getOptimalValue() = [0.7,0.6]");
		assertEquals("param1: ", 0.7, p1b.getValue(0), 0.00000001);
		assertEquals("param2: ",0.6,p1b.getValue(1),0.00000001);
		double f1b = d1.getOptimalCost();
		System.out.println("d1.getOptimalCost() = "+f1b);
		System.out.println("Should be s2.getOpimalCosts()[1] = 0.71");
		assertEquals("optimal cost: ",0.71,f1b,0.00000001);
		IVector[] p1c = d1.getCurrentValues();
		double[] f1c = d1.getCurrentCosts();
		for(int i=0;i<p1c.length;i++){
			System.out.println("par["+i+"]="+p1c[i]);
			System.out.println("cost["+i+"]="+f1c[i]);
		}
		
	}

	public static void testSparseDudWithStrongConstraint_lowerBounds() {
		System.out.println("========================================================");
		System.out.println("SparseDud minimization of kwadratic least-squares cost ");
		System.out.println("Testing lower bounds");
		System.out.println("========================================================");
		// Dud test on simple quadratic cost
		LeastSquaresCostFunction f1 = new SimpleLeastSquaresCostFunction();
		f1.setBackgroundTerm(true);

		// Initial parameters reside within the constraints.
		IVector p1 = new Vector("[2.0,2.0]");
		int[][] sparsenessPattern = new int[][]{{1,0},{0,1}};
		SparseDudCoreOptimizer d1 = new SparseDudCoreOptimizer(f1,sparsenessPattern);
		d1.lower="[  0.7,  .6]";
		d1.upper="[ 10.0, 10.0]";
		d1.add_constraints=true;
		d1.initialize(p1);
		d1.absTol = 0.001;
		d1.relTol = 0.001;

		d1.optimize();
		IVector p1b = d1.getOptimalValue();
		System.out.println("d1.init = "+p1);
		System.out.println("d1.getOptimalValue() = "+p1b);
		System.out.println("Should be d1.getOptimalValue() = [0.7,0.6]");
		assertEquals("param1: ", 0.7, p1b.getValue(0), 0.00000001);
		assertEquals("param2: ",0.6,p1b.getValue(1),0.00000001);
		double f1b = d1.getOptimalCost();
		System.out.println("d1.getOptimalCost() = " + f1b);
		System.out.println("Should be s2.getOpimalCosts()[1] = 0.71");
		assertEquals("optimal cost: ",0.71,f1b,0.00000001);
		IVector[] p1c = d1.getCurrentValues();
		double[] f1c = d1.getCurrentCosts();
		for(int i=0;i<p1c.length;i++){
			System.out.println("par["+i+"]="+p1c[i]);
			System.out.println("cost["+i+"]="+f1c[i]);
		}

		// Initial parameters reside above the upper constraints.
		p1 = new Vector("[20.0,200.0]");
		d1 = new SparseDudCoreOptimizer(f1,sparsenessPattern);
		d1.lower="[  0.7,  .6]";
		d1.upper="[ 10.0, 10.0]";
		d1.add_constraints=true;
		d1.initialize(p1);
		d1.absTol = 0.001;
		d1.relTol = 0.001;

		d1.optimize();
		p1b = d1.getOptimalValue();
		System.out.println("d1.init = "+p1);
		System.out.println("d1.getOptimalValue() = "+p1b);
		System.out.println("Should be d1.getOptimalValue() = [0.7,0.6]");
		assertEquals("param1: ", 0.7, p1b.getValue(0), 0.00000001);
		assertEquals("param2: ",0.6,p1b.getValue(1),0.00000001);
		f1b = d1.getOptimalCost();
		System.out.println("d1.getOptimalCost() = " + f1b);
		System.out.println("Should be s2.getOpimalCosts()[1] = 0.71");
		assertEquals("optimal cost: ",0.71,f1b,0.00000001);
		p1c = d1.getCurrentValues();
		f1c = d1.getCurrentCosts();
		for(int i=0;i<p1c.length;i++){
			System.out.println("par["+i+"]="+p1c[i]);
			System.out.println("cost["+i+"]="+f1c[i]);
		}

		// One initial parameter resides below the lower constraint, the other above the upper constraint.
		p1 = new Vector("[20.2,0.2]");
		d1 = new SparseDudCoreOptimizer(f1,sparsenessPattern);
		d1.lower="[  0.7,  .6]";
		d1.upper="[ 10.0, 10.0]";
		d1.add_constraints=true;
		d1.initialize(p1);
		d1.absTol = 0.001;
		d1.relTol = 0.001;

		d1.optimize();
		p1b = d1.getOptimalValue();
		System.out.println("d1.init = "+p1);
		System.out.println("d1.getOptimalValue() = "+p1b);
		System.out.println("Should be d1.getOptimalValue() = [0.7,0.6]");
		assertEquals("param1: ", 0.7, p1b.getValue(0), 0.00000001);
		assertEquals("param2: ",0.6,p1b.getValue(1),0.00000001);
		f1b = d1.getOptimalCost();
		System.out.println("d1.getOptimalCost() = " + f1b);
		System.out.println("Should be s2.getOpimalCosts()[1] = 0.71");
		assertEquals("optimal cost: ",0.71,f1b,0.00000001);
		p1c = d1.getCurrentValues();
		f1c = d1.getCurrentCosts();
		for(int i=0;i<p1c.length;i++){
			System.out.println("par["+i+"]="+p1c[i]);
			System.out.println("cost["+i+"]="+f1c[i]);
		}

		// Initial parameters reside below the lower constraints.
		p1 = new Vector("[0.2,0.2]");
		System.out.println("d1.init = "+p1);
		d1 = new SparseDudCoreOptimizer(f1,sparsenessPattern);
		d1.lower="[  0.7,  .6]";
		d1.upper="[ 10.0, 10.0]";
		d1.add_constraints=true;
		d1.initialize(p1);
		d1.absTol = 0.001;
		d1.relTol = 0.001;

		d1.optimize();
		p1b = d1.getOptimalValue();
		System.out.println("d1.init new = "+p1);
		System.out.println("d1.getOptimalValue() = "+p1b);
		System.out.println("Should be d1.getOptimalValue() = [0.7,0.6]");
		assertEquals("param1: ", 0.7, p1b.getValue(0), 0.00000001);
		assertEquals("param2: ",0.6,p1b.getValue(1),0.00000001);
		f1b = d1.getOptimalCost();
		System.out.println("d1.getOptimalCost() = " + f1b);
		System.out.println("Should be s2.getOpimalCosts()[1] = 0.71");
		assertEquals("optimal cost: ",0.71,f1b,0.00000001);
		p1c = d1.getCurrentValues();
		f1c = d1.getCurrentCosts();
		for(int i=0;i<p1c.length;i++){
			System.out.println("par["+i+"]="+p1c[i]);
			System.out.println("cost["+i+"]="+f1c[i]);
		}
	}

	public static void testSparseDudWithStrongConstraint_upperBounds() {
		System.out.println("========================================================");
		System.out.println("SparseDud minimization of kwadratic least-squares cost ");
		System.out.println("Testing upper bounds");
		System.out.println("========================================================");
		// Dud test on simple quadratic cost
		LeastSquaresCostFunction f1 = new SimpleLeastSquaresCostFunction();
		f1.setBackgroundTerm(true);
//		IVector p1 = new Vector("[200.0,200.0]");
		IVector p1 = new Vector("[0.13,0.29]");
		int[][] sparsenessPattern = new int[][]{{1,0},{0,1}};
		SparseDudCoreOptimizer d1 = new SparseDudCoreOptimizer(f1,sparsenessPattern);
		d1.lower="[-10.0,-10.0]";
		d1.upper="[ 0.3, 0.4]";
		d1.add_constraints=true;
		d1.initialize(p1);
		d1.absTol = 0.001;
		d1.relTol = 0.001;

		d1.optimize();
		IVector p1b = d1.getOptimalValue();
		System.out.println("d1.getOptimalValue() = "+p1b);
		System.out.println("Should be d1.getOptimalValue() = [0.7,0.6]");
		assertEquals("param1: ", 0.3, p1b.getValue(0), 0.00000001);
		assertEquals("param2: ",0.4,p1b.getValue(1),0.00000001);
		double f1b = d1.getOptimalCost();
		System.out.println("d1.getOptimalCost() = " + f1b);
		System.out.println("Should be s2.getOpimalCosts()[1] = 0.71");
		assertEquals("optimal cost: ",0.71,f1b,0.00000001);
		IVector[] p1c = d1.getCurrentValues();
		double[] f1c = d1.getCurrentCosts();
		for(int i=0;i<p1c.length;i++){
			System.out.println("par["+i+"]="+p1c[i]);
			System.out.println("cost["+i+"]="+f1c[i]);
		}

	}

	public static void testSparseDudWithStrongConstraint_oscill() {
		System.out.println("========================================================");
		System.out.println("sparseDud minimization of oscillator model ");
		System.out.println("Testing upper bounds");
		System.out.println("========================================================");
		// Dud test on simple quadratic cost
		LeastSquaresCostFunction f1 = new SimpleLeastSquaresCostFunction();
		f1.setBackgroundTerm(true);

		IStochModelFactory stochModelFactory = new OscillatorStochModelFactory();
		stochModelFactory.initialize(null, new String[]{
				"<oscillatorConfig>"
						+"	<parameters names=\"t_damp,omega\">[8.5,8.0]</parameters>"
//						+"	<parameters names=\"t_damp,omega\">[9.9,7.5]</parameters>"
						+"</oscillatorConfig>"});
		IStochObserver obsGenerated = new NoosTimeSeriesStochObserver();
		obsGenerated.initialize(testRunDataDir, new String[]{"noosOscillatorWithNoFilter.xml"});
		// Now start calibration through proper algorithm-class
		String dudConfig =
				"<DudConfig>"
						+"	<costFunction weakParameterConstraint=\"off\" class=\"org.openda.algorithms.SimulationKwadraticCostFunction\" />"
						+"	<outerLoop maxIterations=\"10\" absTolerance=\"0.01\" relTolerance=\"0.01\" />"
						+"	<lineSearch maxIterations=\"5\" maxRelStepSize=\"10.0\" >"
						+"		<backtracking shorteningFactor=\"0.5\" startIterationNegativeLook=\"3\" />"
						+"	</lineSearch>"

						+"	<matlabOutputFile>dud.m</matlabOutputFile>"

						+" <constraints parameterConstraint=\"true\">"
						// With the bounds below, SparseDud brings the parameters to the correct ones:
						+" <lowerbounds bounds=\"[8.0, 1.0]\"/>"
						+" <upperbounds bounds=\"[10.0, 10.0]\"/>"
// 						// With the bounds below, SparseDud brings the parameters to the lower bounds:
//						+" <lowerbounds bounds=\"[9.0, 9.0]\"/>"
//						+" <upperbounds bounds=\"[10.0, 10.0]\"/>"
//						// With the bounds below and initial parameter: <parameters names="t_damp,omega">[9.9,7.5]</parameters>:
//						// SparseDud will collapse because gradient becomes zero. It is also interesting that although the initial
//						// parameters are above the upper bounds, they end up equal to the lower bounds. Something to check later.
//						+" <lowerbounds bounds=\"[2.2, .1]\"/>"
//						+" <upperbounds bounds=\"[9.0, 9.0]\"/>"
						+" </constraints>"

						+"	<dependencies>"
						+"		<obs id=\"global.position\"> <depends_on> <par id=\"t_damp\"/> </depends_on> </obs>"
						+"		<obs id=\"global.position\"> <depends_on> <par id=\"omega\"/> </depends_on> </obs>"
						+"	</dependencies>"
						+"</DudConfig>";
		SparseDud algorithm = new SparseDud();
		algorithm.initialize(testRunDataDir,new String[]{dudConfig});
		algorithm.setStochComponents(obsGenerated,stochModelFactory);
		algorithm.prepare();
		algorithm.run();

		IVector p1b = algorithm.optimizer.getOptimalValue();
		System.out.println("d1.getOptimalValue() = "+p1b);
		System.out.println("Should be d1.getOptimalValue() = [8.531918348750187,7.971759330112735]");
		assertEquals("tdamp: ", 8.531918348750187, p1b.getValue(0), 0.01);
		assertEquals("omega: ", 7.971759330112735, p1b.getValue(1), 0.01);
//		assertEquals("s2.getOptimalValue()",""+p1b.toString(), "[8.531918348750187,7.971759330112735]");
		double f1b = algorithm.optimizer.getOptimalCost();
		System.out.println("d1.getOptimalCost() = "+f1b);
		System.out.println("Should be s2.getOpimalCosts()[1] = 0.070");
		assertEquals("cost: ", 0.0696057145865588, f1b, 0.01);
		IVector[] p1c = algorithm.optimizer.getCurrentValues();
		double[] f1c = algorithm.optimizer.getCurrentCosts();
		for(int i=0;i<p1c.length;i++){
			System.out.println("par["+i+"]="+p1c[i]);
			System.out.println("cost["+i+"]="+f1c[i]);
		}

	}

}
