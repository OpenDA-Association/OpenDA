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
package org.openda.algorithms;
import junit.framework.TestCase;
import org.openda.interfaces.*;
import org.openda.utils.Matrix;
import org.openda.utils.Vector;
//import org.openda.algorithms.*;

public class CoreOptimizerTest extends TestCase {

	public static void testSimplexCoreOptimizer_1() {
		System.out.println("========================================================");
		System.out.println("Basic test of a cost function");
		System.out.println("========================================================");
		// getting and setting
		ICostFunction f1 = new SimpleCostFunction();
		IVector p1 = new Vector("[0.0,0.0]");
		//public SimplexCoreOptimizer(SimpleCostFunction f, Vector pInit, double width){
		SimplexCoreOptimizer s1 = new SimplexCoreOptimizer(f1);
        s1.initialize(p1,1.5);
		//public Vector[] getCurrentValues(){
		IVector[] p1b = s1.getCurrentValues();
		System.out.println("s1.getCurrentValues()[1] = "+p1b[1].toString());
		System.out.println("Should be s1.getCurrentValues()[1] = [1.5,0.0]");
		assertEquals("s1.getCurrentValues()",p1b[1].toString(), "[1.5,0.0]");
		//public double[] getCurrentCosts(){
		double[] f1b = s1.getCurrentCosts();
		System.out.println("s1.getCurrentCosts()[1] = "+f1b[1]);
		System.out.println("Should be s1.getCurrentCosts()[1] = 0.5");
		assertEquals("s1.getCurrentCosts()",""+f1b[1], "0.5");
	}

	public static void testSimplexCoreOptimizer_2() {
		System.out.println("========================================================");
		System.out.println("Simplex minimization of kwadratic cost");
		System.out.println("========================================================");
		// Simplex test on simple kwadratic cost
		SimpleCostFunction f2 = new SimpleCostFunction();
		IVector p2 = new Vector("[0.0,0.0]");
		//public SimplexCoreOptimizer(SimpleCostFunction f, Vector pInit, double width){
		SimplexCoreOptimizer s2 = new SimplexCoreOptimizer(f2);
        s2.initialize(p2,1.5);
		s2.absTolSimplex = 0.001;
		s2.relTolSimplex = 0.00001;
		//public void optimize(int maxIter, double tol){
		s2.optimize();
		//public Vector getOptimalValue(){
		IVector p2b = s2.getOptimalValue();
		System.out.println("s2.getOptimalValue() = "+p2b);
		System.out.println("Should be s2.getOptimalValue() = [0.96148681640625,0.9224853515625]");
		assertEquals("s2.getOptimalValue()",""+p2b.toString(), "[0.96148681640625,0.9224853515625]");
		//public double getOptimalCost(){
		double f2b = s2.getOptimalCost();
		System.out.println("s2.getOptimalCost() = "+f2b);
		System.out.println("Should be s2.getOpimalCosts()[1] = 0.0029853954911231995");
		assertEquals("s2.getOptimalCost()",""+f2b, "0.0029853954911231995");
		IVector[] p2c = s2.getCurrentValues();
		double[] f2c = s2.getCurrentCosts();
		for(int i=0;i<p2c.length;i++){
			System.out.println("par["+i+"]="+p2c[i]);
			System.out.println("cost["+i+"]="+f2c[i]);
		}
	}

	public static void testRosenbrock_1() {
		System.out.println("========================================================");
		System.out.println("Basic test of the Rosenbrock costfunction");
		System.out.println("========================================================");
		// test Rosenbrock function itself (no optimization)
		ICostFunction fRb2 = new RosenbrockCostFunction();
		IVector p2a = new Vector("[0.0,0.0]");
		IVector p2b = new Vector("[1.0,1.0]"); //optimal with value 0.0
		double r2Vala = fRb2.evaluate(p2a,"initialization");
		double r2Valb = fRb2.evaluate(p2b,"initialization");
		System.out.println("Rosenbrock_2("+p2a+") = "+r2Vala);
		System.out.println("Should be Rosenbrock_2("+p2a+") = 1.0");
		assertEquals("Rosenbrock_2("+p2a+") ",r2Vala, 1.0);
		System.out.println("Rosenbrock_2("+p2b+") = "+r2Valb);
		System.out.println("Should be Rosenbrock_2("+p2b+") = 0.0");
		assertEquals("Rosenbrock_2("+p2b+") ",r2Valb, 0.0);

		ICostFunction fRb4 = new RosenbrockCostFunction();
		IVector p4a = new Vector("[0.0,0.0,0.0,0.0]");
		IVector p4b = new Vector("[1.0,1.0,1.0,1.0]"); //optimal with value 0.0
		double r4Vala = fRb4.evaluate(p4a,"initialization");
		double r4Valb = fRb4.evaluate(p4b,"initialization");
		System.out.println("Rosenbrock_4("+p4a+") = "+r4Vala);
		System.out.println("Should be Rosenbrock_4("+p4a+") = 2.0");
		assertEquals("Rosenbrock_4("+p4a+") ",r4Vala, 2.0);
		System.out.println("Rosenbrock_4("+p4b+") = "+r4Valb);
		System.out.println("Should be Rosenbrock_2("+p2b+") = 0.0");
		assertEquals("Rosenbrock_4("+p4b+") ",r4Valb, 0.0);
	}

	public static void testSimplexRosenbrock_1() {
		System.out.println("========================================================");
		System.out.println("Simplex minimization of the Rosenbrock function");
		System.out.println("========================================================");
		// test simplex method on rosenbrock
		ICostFunction fRb2 = new RosenbrockCostFunction();
		IVector p2 = new Vector("[0.0,0.0]");
		SimplexCoreOptimizer sRb2 = new SimplexCoreOptimizer(fRb2);
        sRb2.initialize(p2,1.5);
		sRb2.absTolSimplex = 0.001;
		sRb2.relTolSimplex = 0.0001;
		sRb2.optimize();
		//public Vector getOptimalValue(){
		IVector p2b = sRb2.getOptimalValue();
		System.out.println("s2.getOptimalValue() = "+p2b);
		System.out.println("Should be close to s2.getOptimalValue() = [1.0,1.0]");
		assertEquals("sRb2.getOptimalValue()",""+p2b.toString(), "[0.9961825689188117,0.9919014011138643]");
		//public double getOptimalCost(){
		double f2b = sRb2.getOptimalCost();
		System.out.println("s2.getOptimalCost() = "+f2b);
		System.out.println("Should be close to s2.getOpimalCosts() = 0.0");
		assertEquals("sRb2.getOptimalCost()",f2b, 0.0,0.0001);

	}

	public static void testPowelKwadratic_1() {
		System.out.println("========================================================");
		System.out.println("Powell minimization of a simple kwadratic function");
		System.out.println("========================================================");
		SimpleCostFunction f2 = new SimpleCostFunction();
		IVector p2 = new Vector("[0.1,0.1]");
		PowellCoreOptimizer p = new PowellCoreOptimizer(f2);
        p.initialize(p2,0.5);
		p.absTolPowell = 0.001;
		p.relTolPowell = 0.0001;
		p.optimize();
		//public Vector getOptimalValue(){
		IVector pOpt = p.getOptimalValue();
		System.out.println("p.getOptimalValue() = "+pOpt);
		System.out.println("Should be close to p.getOptimalValue() = [1.0,1.0]");
		assertEquals("p.getOptimalValue()",""+pOpt.toString(), "[0.9999999999999999,1.0]");
		//public double getOptimalCost(){
		double fOpt = p.getOptimalCost();
		System.out.println("p.getOptimalCost() = "+fOpt);
		System.out.println("Should be close to p.getOpimalCosts() = 0.0");
		assertEquals("p.getOptimalCost()",fOpt, 0.0,0.0001);

	}

	
	public static void testPowelRosenbrock_1() {
		System.out.println("========================================================");
		System.out.println("Powell minimization of the Rosenbrock function");
		System.out.println("========================================================");
		ICostFunction fRb2 = new RosenbrockCostFunction();
		IVector p2 = new Vector("[0.0,0.0]");
		PowellCoreOptimizer pRb = new PowellCoreOptimizer(fRb2);
        pRb.initialize(p2,0.5);
		pRb.absTolPowell = 0.001;
		pRb.relTolPowell = 0.0001;
		pRb.optimize();
		//public Vector getOptimalValue(){
		IVector pOpt = pRb.getOptimalValue();
		System.out.println("pRb.getOptimalValue() = "+pOpt);
		System.out.println("Should be close to pRb.getOptimalValue() = [1.0,1.0]");
		assertEquals("pRb.getOptimalValue()",""+pOpt.toString(), "[0.9313307487691697,0.8666303581882956]");
		//public double getOptimalCost(){
		double fOpt = pRb.getOptimalCost();
		System.out.println("pRb.getOptimalCost() = "+fOpt);
		System.out.println("Should be close to pRb.getOpimalCosts() = 0.0");
		assertEquals("pRb.getOptimalCost()",fOpt, 0.0,0.01);

	}

	public static void testPowellWithInit() {
		System.out.println("========================================================");
		System.out.println("Powell minimization of kwadratic cost with initialization");
		System.out.println("========================================================");
		// Powell test on simple kwadratic cost with inialization
		SimpleCostFunction f2 = new SimpleCostFunction();
		IVector p2 = new Vector("[0.0,0.0]");
		// minimum is at [1.0,1.0]
		IVector[] searchInit = new Vector[2];
		searchInit[0] = new Vector("[2.0,0.0]");
		searchInit[1] = new Vector("[0.0,1.0]");
		//public PowellCoreOptimizer(SimpleCostFunction f, Vector pInit, double width){
		PowellCoreOptimizer s2 = new PowellCoreOptimizer(f2);
        s2.initialize(p2,searchInit);
		//public void optimize()
		s2.optimize();
		//public Vector getOptimalValue(){
		IVector p2b = s2.getOptimalValue();
		System.out.println("s2.getOptimalValue() = "+p2b);
		System.out.println("Should be s2.getOptimalValue() = [0.9999999999999998,1.0]");
		assertEquals("s2.getOptimalValue()",""+p2b.toString(), "[0.9999999999999998,1.0]");
		//public double getOptimalCost(){
		double f2b = s2.getOptimalCost();
		System.out.println("s2.getOptimalCost() = "+f2b);
		System.out.println("Should be s2.getOpimalCosts()[1] = 4.930380657631324E-32");
		assertEquals("s2.getOptimalCost()",""+f2b, "4.930380657631324E-32");
		IVector[] p2c = s2.getCurrentSearchDirections();
		for(int i=0;i<p2c.length;i++){
			System.out.println("searchDir["+i+"]="+p2c[i]);
		}
	}

	public static void testSimplexWithInit() {
		System.out.println("========================================================");
		System.out.println("Simplex minimization of kwadratic cost with initialization");
		System.out.println("========================================================");
		// Simplex test on simple kwadratic cost with inialization
		SimpleCostFunction f2 = new SimpleCostFunction();
		//Vector p2 = new Vector("[0.0,0.0]");
		// minimum is at [1.0,1.0]
		IVector[] simplexInit = new Vector[3];
		simplexInit[0] = new Vector("[2.0,0.0]");
		simplexInit[1] = new Vector("[0.0,1.0]");
		simplexInit[2] = new Vector("[0.0,0.0]");
		//public SimplexCoreOptimizer(SimpleCostFunction f, Vector[] initialSimplex){
		SimplexCoreOptimizer s2 = new SimplexCoreOptimizer(f2);
        s2.initialize(simplexInit);
		s2.absTolSimplex = 0.001;
		s2.relTolSimplex = 0.00001;
		//public void optimize()
		s2.optimize();
		//public Vector getOptimalValue(){
		IVector p2b = s2.getOptimalValue();
		System.out.println("s2.getOptimalValue() = "+p2b);
		System.out.println("Should be s2.getOptimalValue() = [0.9871273040771484,0.9974803924560547]");
		assertEquals("s2.getOptimalValue()",""+p2b.toString(), "[0.9871273040771484,0.9974803924560547]");
		//public double getOptimalCost(){
		double f2b = s2.getOptimalCost();
		System.out.println("s2.getOptimalCost() = "+f2b);
		System.out.println("Should be s2.getOpimalCosts()[1] = 1.6729340586607577E-4");
		assertEquals("s2.getOptimalCost()",""+f2b, "1.6729340586607577E-4");
		IVector[] p2c = s2.getCurrentValues();
		for(int i=0;i<p2c.length;i++){
			System.out.println("searchDir["+i+"]="+p2c[i]);
		}
	}
	
	public static void testLeastSquaresCost_1() {
		System.out.println("========================================================");
		System.out.println("Simplex minimization of kwadratic LEAST-SQUARES cost");
		System.out.println("========================================================");
		// Simplex test on simple kwadratic cost
		ICostFunction f2 = new SimpleLeastSquaresCostFunction();
		IVector p2 = new Vector("[0.0,0.0]");
		//public SimplexCoreOptimizer(SimpleCostFunction f, Vector pInit, double width){
		SimplexCoreOptimizer s2 = new SimplexCoreOptimizer(f2);
        s2.initialize(p2,1.5);
		s2.absTolSimplex = 0.001;
		s2.relTolSimplex = 0.00001;
		//public void optimize(int maxIter, double tol){
		s2.optimize();
		//public Vector getOptimalValue(){
		IVector p2b = s2.getOptimalValue();
		System.out.println("s2.getOptimalValue() = "+p2b);
		System.out.println("Should be s2.getOptimalValue() = [0.96148681640625,0.9224853515625]");
		assertEquals("s2.getOptimalValue()",""+p2b.toString(), "[0.96148681640625,0.9224853515625]");
		//public double getOptimalCost(){
		double f2b = s2.getOptimalCost();
		System.out.println("s2.getOptimalCost() = "+f2b);
		System.out.println("Should be s2.getOpimalCosts()[1] = 1.9291848002467304E-4");
		assertEquals("s2.getOptimalCost()",f2b,0.0,0.01);
		IVector[] p2c = s2.getCurrentValues();
		double[] f2c = s2.getCurrentCosts();
		for(int i=0;i<p2c.length;i++){
			System.out.println("par["+i+"]="+p2c[i]);
			System.out.println("cost["+i+"]="+f2c[i]);
		}
	}

	public static void testDudWithoutConstraint() {
		System.out.println("========================================================");
		System.out.println("Dud minimization of kwadratic least-squares cost");
		System.out.println("without constraint");
		System.out.println("========================================================");
		// Dud test on simple kwadratic cost
		LeastSquaresCostFunction f1 = new SimpleLeastSquaresCostFunction();
		IVector p1 = new Vector("[0.0,0.0]");
		//public DudCoreOptimizer(SimpleLeastSquaresCostFunction f, Vector pInit){
		DudCoreOptimizer d1 = new DudCoreOptimizer(f1);
        d1.initialize(p1);
		d1.absTol = 0.001;
		d1.relTol = 0.001;
		//public void optimize(int maxIter, double tol){
		d1.optimize();
		//public Vector getOptimalValue(){
		IVector p1b = d1.getOptimalValue();
		System.out.println("d1.getOptimalValue() = "+p1b);
		System.out.println("Should be d1.getOptimalValue() = [1.0,1.0]");
		assertEquals("s2.getOptimalValue()",""+p1b.toString(), "[1.0,1.0]");
		double f1b = d1.getOptimalCost();
		System.out.println("d1.getOptimalCost() = "+f1b);
		System.out.println("Should be s2.getOpimalCosts()[1] = 0.0");
		assertEquals("s2.getOptimalCost()",""+f1b, "0.0");
		IVector[] p1c = d1.getCurrentValues();
		double[] f1c = d1.getCurrentCosts();
		for(int i=0;i<p1c.length;i++){
			System.out.println("par["+i+"]="+p1c[i]);
			System.out.println("cost["+i+"]="+f1c[i]);
		}
	}


	public static void testDudWithConstraint() {
		System.out.println("========================================================");
		System.out.println("Dud minimization of kwadratic least-squares cost ");
		System.out.println("with constraint");
		System.out.println("========================================================");
		// Dud test on simple kwadratic cost
		LeastSquaresCostFunction f1 = new SimpleLeastSquaresCostFunction();
		f1.setBackgroundTerm(true);
		IVector p1 = new Vector("[0.0,0.0]");
		//public DudCoreOptimizer(SimpleLeastSquaresCostFunction f, Vector pInit){
		DudCoreOptimizer d1 = new DudCoreOptimizer(f1);
        d1.initialize(p1);
		d1.absTol = 0.001;
		d1.relTol = 0.001;
		
		//public void optimize(int maxIter, double tol){
		d1.optimize();
		//public Vector getOptimalValue(){
		IVector p1b = d1.getOptimalValue();
		System.out.println("d1.getOptimalValue() = "+p1b);
		System.out.println("Should be d1.getOptimalValue() = [0.5,0.5000000000000002]");
		assertEquals("s2.getOptimalValue()",""+p1b.toString(), "[0.5,0.5000000000000002]");
		double f1b = d1.getOptimalCost();
		System.out.println("d1.getOptimalCost() = "+f1b);
		System.out.println("Should be s2.getOpimalCosts()[1] = 0.625");
		assertEquals("s2.getOptimalCost()",""+f1b, "0.625");
		IVector[] p1c = d1.getCurrentValues();
		double[] f1c = d1.getCurrentCosts();
		for(int i=0;i<p1c.length;i++){
			System.out.println("par["+i+"]="+p1c[i]);
			System.out.println("cost["+i+"]="+f1c[i]);
		}
		
	}

	public static void testSimplexWithConstraint() {
		System.out.println("========================================================");
		System.out.println("Simplex minimization of kwadratic least-squares cost ");
		System.out.println("with constraint");
		System.out.println("========================================================");
		// Simplex test on simple kwadratic cost
		LeastSquaresCostFunction f1 = new SimpleLeastSquaresCostFunction();
		f1.setBackgroundTerm(true);
		IVector p1 = new Vector("[0.0,0.0]");
		//public DudCoreOptimizer(SimpleLeastSquaresCostFunction f, Vector pInit){
		SimplexCoreOptimizer s1 = new SimplexCoreOptimizer(f1);
        s1.initialize(p1,2.0);
		s1.absTolSimplex = 0.0001;
		s1.relTolSimplex = 0.0001;
		
		//public void optimize(int maxIter, double tol){
		s1.optimize();
		//public Vector getOptimalValue(){
		IVector p1b = s1.getOptimalValue();
		System.out.println("d1.getOptimalValue() = "+p1b);
		System.out.println("Should be d1.getOptimalValue() = [0.5033355318009853,0.4968565031886101]");
		assertEquals("s2.getOptimalValue()",""+p1b.toString(), "[0.5033355318009853,0.4968565031886101]");
		double f1b = s1.getOptimalCost();
		System.out.println("d1.getOptimalCost() = "+f1b);
		System.out.println("Should be s2.getOpimalCosts()[1] = 0.6250271923308923");
		assertEquals("s2.getOptimalCost()",""+f1b, "0.6250271923308923");
		IVector[] p1c = s1.getCurrentValues();
		double[] f1c = s1.getCurrentCosts();
		for(int i=0;i<p1c.length;i++){
			System.out.println("par["+i+"]="+p1c[i]);
			System.out.println("cost["+i+"]="+f1c[i]);
		}
	}

	public static void testGriddedFullSearch() {
		System.out.println("========================================================");
		System.out.println("gridded full search minimization of kwadratic least-squares cost ");
		System.out.println("without constraint");
		System.out.println("========================================================");
		// Simplex test on simple kwadratic cost
		LeastSquaresCostFunction f1 = new SimpleLeastSquaresCostFunction();
		f1.setBackgroundTerm(false);
        IVector pStart = new Vector("[0.0,0.0]");
		IVector pStop  = new Vector("[2.0,2.0]");
		IVector pStep  = new Vector("[0.2,0.2]");
		GriddedFullSearchCoreOptimizer s1 = new GriddedFullSearchCoreOptimizer(f1,pStart,pStop,pStep);
		
		s1.optimize();
		//public Vector getOptimalValue(){
		IVector p1b = s1.getOptimalValue();
		System.out.println("s1.getOptimalValue() = " + p1b);
		System.out.println("Should be s1.getOptimalValue() = [1.0,1.0]");
		assertEquals("s2.getOptimalValue()",""+p1b.toString(), "[1.0,1.0]");
		//public double getOptimalCost(){
		double f1b = s1.getOptimalCost();
		System.out.println("s1.getOptimalCost() = " + f1b);
		System.out.println("Should be s2.getOpimalCosts()[1] = 0.0");
		assertEquals("s2.getOptimalCost()", f1b, 0.0, 0.01);
	}

	public static void testLMWithConstraint() {
		System.out.println("========================================================");
		System.out.println("LM minimization of kwadratic least-squares cost ");
		System.out.println("with constraint");
		System.out.println("========================================================");
		// Dud test on simple kwadratic cost
		LeastSquaresCostFunction f1 = new SimpleLeastSquaresCostFunction();
		f1.setBackgroundTerm(true);
		IVector p1 = new Vector("[0.0,0.0]");
		//public DudCoreOptimizer(SimpleLeastSquaresCostFunction f, Vector pInit){
		LMCoreOptimizer d1 = new LMCoreOptimizer(f1);
		d1.initialize(p1);
		d1.absTol = 0.001;
		d1.relTol = 0.001;
		d1.fwdEps = new Vector(new double[]{0.001,0.001});
		//public void optimize(int maxIter, double tol){
		d1.optimize();
		//public Vector getOptimalValue(){
		IVector p1b = d1.getOptimalValue();
		System.out.println("d1.getOptimalValue() = "+p1b);
		System.out.println("Should be d1.getOptimalValue() = [0.5,0.5]");
		assertEquals("s2.getOptimalValue() 1st parameter: ", p1b.getValue(0), 0.5, 1e-4);
		assertEquals("s2.getOptimalValue() 2nd parameter: ",p1b.getValue(1),0.5,1e-4);
//		System.out.println("s2.getOptimalValue()",""+p1b.toString(), "[0.5,0.5000000000000002]");
		double f1b = d1.getOptimalCost();
		System.out.println("d1.getOptimalCost() = "+f1b);
		System.out.println("Should be s2.getOpimalCosts()[1] = 0.625");
		assertEquals("s2.getOptimalCost()",f1b, 0.625,1e-8);
		IVector p1c = d1.getCurrentValues();
		double f1c = d1.getCurrentCosts();
		System.out.println("par="+p1c);
		System.out.println("cost="+f1c);

	}

	public static void testLMWithoutConstraint() {
		System.out.println("========================================================");
		System.out.println("LM minimization of quadratic least-squares cost");
		System.out.println("without constraint");
		System.out.println("========================================================");
		// Dud test on simple kwadratic cost
		LeastSquaresCostFunction f1 = new SimpleLeastSquaresCostFunction();
//		SimpleLeastSquaresCostFunctionWithState f1 = new SimpleLeastSquaresCostFunctionWithState();
		IVector p1 = new Vector("[0.0,0.0]");
		//public DudCoreOptimizer(SimpleLeastSquaresCostFunction f, Vector pInit){
		LMCoreOptimizer d1 = new LMCoreOptimizer(f1);
		d1.initialize(p1);
		d1.absTol = 0.001;
		d1.relTol = 0.001;
		d1.computeErrorEstimate = true;
		d1.fwdEps = new Vector(new double[]{0.001,0.001});
		//public void optimize(int maxIter, double tol){
		d1.optimize();
		//public Vector getOptimalValue(){
		IVector p1b = d1.getOptimalValue();
		System.out.println("d1.getOptimalValue() = " + p1b);
		System.out.println("Should be d1.getOptimalValue() = [1.0,1.0]");
		assertEquals("s2.getOptimalValue() 1st parameter: ",p1b.getValue(0),1.0,1e-3);
		assertEquals("s2.getOptimalValue() 2nd parameter: ",p1b.getValue(1),1.0,1e-3);
		double f1b = d1.getOptimalCost();
		System.out.println("d1.getOptimalCost() = "+f1b);
		System.out.println("Should be s2.getOpimalCosts()[1] = 0.0");
		assertEquals("s2.getOptimalCost()", f1b, 0.0, 1e-5);
		IVector p1c = d1.getCurrentValues();
		double f1c = d1.getCurrentCosts();
		System.out.println("par="+p1c);
		System.out.println("cost[]="+f1c);
	}

	public static void testLMWithoutConstraint_withStateErrorEstimate() {
		System.out.println("========================================================");
		System.out.println("LM minimization of quadratic least-squares cost");
		System.out.println("without constraint");
		System.out.println("========================================================");
		// LM test on simple kwadratic cost
		SimpleLeastSquaresCostFunctionWithState f1 = new SimpleLeastSquaresCostFunctionWithState();
		IVector p1 = new Vector("[0.0,0.0]");
		LMCoreOptimizer d1 = new LMCoreOptimizer(f1);
		d1.initialize(p1);
		d1.absTol = 0.001;
		d1.relTol = 0.001;
		d1.computeErrorEstimate = true;
		d1.computeStateJacobian = true;
		d1.fwdEps = new Vector(new double[]{0.001,0.001});
		d1.optimize();
		IVector p1b = d1.getOptimalValue();
		System.out.println("d1.getOptimalValue() = " + p1b);
		System.out.println("Should be d1.getOptimalValue() = [1.0,0.0]");
		assertEquals("s2.getOptimalValue() 1st parameter: ",p1b.getValue(0),1.0,1e-6);
		assertEquals("s2.getOptimalValue() 2nd parameter: ",p1b.getValue(1),1.0,1e-6);
		double f1b = d1.getOptimalCost();
		System.out.println("d1.getOptimalCost() = "+f1b);
		System.out.println("Should be s2.getOpimalCosts()[1] = 0.0");
		assertEquals("s2.getOptimalCost()", f1b, 0.0, 1e-5);
		IVector p1c = d1.getCurrentValues();
		double f1c = d1.getCurrentCosts();
		System.out.println("par="+p1c);
		System.out.println("cost[]="+f1c);
		IMatrix paramCov = d1.getParamCovariance();
		assertEquals("paramCov[0,0]: ",paramCov.getValue(0,0),0.975609756097775,1e-6);
		assertEquals("paramCov[1,1]: ",paramCov.getValue(1,1),3.609756097561771,1e-6);
		assertEquals("paramCov[0,1]: ",paramCov.getValue(0,1),-0.097560975609778,1e-6);
		IMatrix stateCov = d1.getStateCovariance();
		IVector stateStd = ((Matrix)stateCov).diag();
		assertEquals("stateCov[0,0]: ",stateCov.getValue(0,0),0.9756097560975605,1e-6);
		assertEquals("stateCov[1,1]: ",stateCov.getValue(1,1),3.6097560975609757,1e-6);
		assertEquals("stateCov[2,2]: ",stateCov.getValue(2,2),1.097560975609756,1e-6);
		assertEquals("stateCov[0,1]: ",stateCov.getValue(0,1),-0.09756097560975613,1e-6);
		assertEquals("stateCov[0,2]: ",stateCov.getValue(0,2),0.43902439024390216,1e-6);
		assertEquals("stateCov[1,2]: ",stateCov.getValue(1,2),1.7560975609756098,1e-6);
		System.out.println("d1.getStateCovariance() = "+stateCov.toString());
		System.out.println("stateStd " + stateStd.toString());
	}

	public static void testLMWithConstraint_withStateErrorEstimate() {
		System.out.println("========================================================");
		System.out.println("LM minimization of quadratic least-squares cost");
		System.out.println("with constraint");
		System.out.println("========================================================");
		// LM test on simple kwadratic cost
		SimpleLeastSquaresCostFunctionWithState f1 = new SimpleLeastSquaresCostFunctionWithState();
		f1.setBackgroundTerm(true);
		IVector p1 = new Vector("[0.0,0.0]");
		LMCoreOptimizer d1 = new LMCoreOptimizer(f1);
		d1.initialize(p1);
		d1.absTol = 0.001;
		d1.relTol = 0.001;
		d1.computeErrorEstimate = true;
		d1.computeStateJacobian = true;
		d1.fwdEps = new Vector(new double[]{0.001,0.001});
		d1.optimize();
		IVector p1b = d1.getOptimalValue();
		System.out.println("d1.getOptimalValue() = " + p1b);
		System.out.println("Should be d1.getOptimalValue() = [1.0,0.0]");
		assertEquals("s2.getOptimalValue() 1st parameter: ",p1b.getValue(0),0.7564934328893387,1e-6);
		assertEquals("s2.getOptimalValue() 2nd parameter: ",p1b.getValue(1),0.7759739562556575,1e-6);
		double f1b = d1.getOptimalCost();
		System.out.println("d1.getOptimalCost() = "+f1b);
		System.out.println("Should be s2.getOpimalCosts()[1] = 0.16274350649352032");
		assertEquals("s2.getOptimalCost()", f1b, 0.16274350649352032, 1e-5);
		IVector p1c = d1.getCurrentValues();
		double f1c = d1.getCurrentCosts();
		System.out.println("par="+p1c);
		System.out.println("cost[]="+f1c);
		IMatrix paramCov = d1.getParamCovariance();
		assertEquals("paramCov[0,0]: ",paramCov.getValue(0,0),0.493506493506494,1e-6);
		assertEquals("paramCov[1,1]: ",paramCov.getValue(1,1),1.896103896103896,1e-6);
		assertEquals("paramCov[0,1]: ",paramCov.getValue(0,1),-0.025974025974026,1e-6);
		IMatrix stateCov = d1.getStateCovariance();
		IVector stateStd = ((Matrix)stateCov).diag();
		assertEquals("stateCov[0,0]: ",stateCov.getValue(0,0),0.493506493506494,1e-6);
		assertEquals("stateCov[1,1]: ",stateCov.getValue(1,1),1.896103896103896,1e-6);
		assertEquals("stateCov[2,2]: ",stateCov.getValue(2,2),0.584415584415584,1e-6);
		assertEquals("stateCov[0,1]: ",stateCov.getValue(0,1),-0.025974025974026,1e-6);
		assertEquals("stateCov[0,2]: ",stateCov.getValue(0,2),0.233766233766234,1e-6);
		assertEquals("stateCov[1,2]: ",stateCov.getValue(1,2),0.935064935064935,1e-6);
		System.out.println("d1.getStateCovariance() = "+stateCov.toString());
		System.out.println("stateStd " + stateStd.toString());
	}

	public static void testDelsa() {
		System.out.println("========================================================");
		System.out.println("DELSA with quadratic least-squares cost");
		System.out.println("========================================================");
		SimpleLeastSquaresCostFunctionWithState f1 = new SimpleLeastSquaresCostFunctionWithState();
		f1.setBackgroundTerm(false);
		IVector p1 = new Vector("[0.0,0.0]");
		Vector maxRange = new Vector(new double[]{1.0,1.0});
		Vector minRange = new Vector(new double[]{0.0,0.0});
		int nPointsPerParameter = 2;
		DelsaCoreOptimizer d1  = new DelsaCoreOptimizer(f1,minRange,maxRange,nPointsPerParameter,false,p1);
		while (d1.hasNext()){
			d1.next();
		}
		IVector[] SL1 = d1.getSensitivityIndex();
		IVector[] paramSets = d1.getParametersSets();
		// Assuming regular grid:
		assertEquals("param0, sample 1: ",paramSets[0].getValue(0),0.0);
		assertEquals("SL1 param0, sample 1: ",SL1[0].getValue(0),0.9226373955644765);
		assertEquals("param1, sample 1: ",paramSets[0].getValue(1),0.0);
		assertEquals("SL1 param1, sample 1: ",SL1[0].getValue(1),0.07736260443552348);
		assertEquals("param0, sample 2: ",paramSets[1].getValue(0),1.0);
		assertEquals("SL1 param0, sample 2: ",SL1[1].getValue(0),0.006664460982337924);
		assertEquals("param1, sample 2: ",paramSets[1].getValue(1),0.0);
		assertEquals("SL1 param1, sample 2: ",SL1[1].getValue(1),0.993335539017662);
		assertEquals("param0, sample 1: ",paramSets[2].getValue(0),0.0);
		assertEquals("SL1 param0, sample 3: ",SL1[2].getValue(0),0.9993345606051148);
		assertEquals("param1, sample 1: ",paramSets[2].getValue(1),1.0);
		assertEquals("SL1 param1, sample 3: ",SL1[2].getValue(1),6.654393948851352E-4);
		assertEquals("param0, sample 1: ",paramSets[3].getValue(0),1.0);
		assertEquals("SL1 param0, sample 4: ",SL1[3].getValue(0),0.9319264805990473);
		assertEquals("param1, sample 1: ",paramSets[3].getValue(1),1.0);
		assertEquals("SL1 param1, sample 5: ",SL1[3].getValue(1),0.06807351940095262);
	}

	//TODO restart on stored iteration
	//public void setCurrentValues(Vector[] pars){
	//public void setCurrentValues(Vector pars[], double values[]){

}
