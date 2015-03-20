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
package org.openda.algorithms;
import junit.framework.TestCase;
import org.openda.interfaces.*;
import org.openda.utils.*;

public class BfgsCoreOptimizerTest extends TestCase {

	public static void testLBFGSKwadratic_1() {
		System.out.println("==============================================================");
		System.out.println("L-BFGS minimization of a simple kwadratic function");
		System.out.println("==============================================================");
		SimpleCostFunction f2 = new SimpleCostFunction();
		IVector p2 = new Vector("[2.0,2.0]");
		BFGSCoreOptimizer p = new BFGSCoreOptimizer(f2);
		p.limitedMemory = true;
		p.initialize(p2);
		p.optimize();
		IVector pOpt = p.getOptimalValue();
		System.out.println("p.getOptimalValue() = "+pOpt);
		System.out.println("Should be close to solution = [1.0,1.0]");
		double fOpt = p.getOptimalCost();
		System.out.println("p.getOptimalCost() = " + fOpt);
		System.out.println("Should be close to p.getOpimalCosts() = 0.0");
		System.out.println("");
		
		assertEquals("First variable ",1.0,pOpt.getValue(0),0.01);
		assertEquals("Second variable ",1.0,pOpt.getValue(1),0.01);
		assertEquals("p.getOptimalCost()",fOpt, 0.0,0.0001);

	}

	public static void testLBFGSKwadratic_2() {
		System.out.println("==============================================================");
		System.out.println("BFGS minimization of a simple kwadratic function (builds a matrix)");
		System.out.println("==============================================================");
		SimpleCostFunction f2 = new SimpleCostFunction();
		IVector p2 = new Vector("[2.0,2.0]");
		BFGSCoreOptimizer p = new BFGSCoreOptimizer(f2);
		p.limitedMemory = false;
		p.initialize(p2);
		p.optimize();
		IVector pOpt = p.getOptimalValue();
		System.out.println("p.getOptimalValue() = "+pOpt);
		System.out.println("Should be close to solution = [1.0,1.0]");
		double fOpt = p.getOptimalCost();
		System.out.println("p.getOptimalCost() = " + fOpt);
		System.out.println("Should be close to p.getOpimalCosts() = 0.0");
		System.out.println("");
		
		assertEquals("First variable ",1.0,pOpt.getValue(0),0.01);
		assertEquals("Second variable ",1.0,pOpt.getValue(1),0.01);
		assertEquals("p.getOptimalCost()",fOpt, 0.0,0.0001);

	}
	
	public static void testLBFGSRosenbrock_1() {
		System.out.println("==============================================================");
		System.out.println("L-BFGS minimization of the Rosenbrock function");
		System.out.println("==============================================================");
		ICostFunctionWithGradient fRb1 = new RosenbrockCostFunction();
		IVector p1 = new Vector("[0.0,0.0]");
		BFGSCoreOptimizer pRb1 = new BFGSCoreOptimizer(fRb1);
		pRb1.limitedMemory = true;
		pRb1.nStore = 1;
		pRb1.initialize(p1);
		pRb1.relTolStep = 0.0001;
		pRb1.absTolStep = 0.0001;
		pRb1.relTolBrent = 0.00001;
		pRb1.absTolBrent = 0.00001;
		pRb1.optimize();
		IVector pOpt = pRb1.getOptimalValue();
		System.out.println("pRb1.getOptimalValue() = "+pOpt);
		System.out.println("Should be close to pRb1.getOptimalValue() = [1.0,1.0]");
		
		//public double getOptimalCost(){
		double fOpt = pRb1.getOptimalCost();
		System.out.println("pRb1.getOptimalCost() = "+fOpt);
		System.out.println("Should be close to pRb1.getOpimalCosts() = 0.0");

		assertEquals("First variable ",1.0,pOpt.getValue(0),0.01);
		assertEquals("Second variable ",1.0,pOpt.getValue(1),0.01);
		assertEquals("p.getOptimalCost()",fOpt, 0.0,0.0001);
	}

	public static void testBFGSRosenbrock_2() {
		System.out.println("==============================================================");
		System.out.println("BFGS minimization of the Rosenbrock function (builds a matrix)");
		System.out.println("==============================================================");
		ICostFunctionWithGradient fRb2 = new RosenbrockCostFunction();
		IVector p2 = new Vector("[0.0,0.0]");
		BFGSCoreOptimizer pRb2 = new BFGSCoreOptimizer(fRb2);
		pRb2.limitedMemory = false;
		pRb2.initialize(p2);
		pRb2.relTolStep = 0.0001;
		pRb2.absTolStep = 0.0001;
		pRb2.relTolBrent = 0.00001;
		pRb2.absTolBrent = 0.00001;
		pRb2.optimize();
		IVector pOpt = pRb2.getOptimalValue();
		System.out.println("pRb2.getOptimalValue() = "+pOpt);
		System.out.println("Should be close to pRb2.getOptimalValue() = [1.0,1.0]");
		
		//public double getOptimalCost(){
		double fOpt = pRb2.getOptimalCost();
		System.out.println("pRb2.getOptimalCost() = "+fOpt);
		System.out.println("Should be close to pRb2.getOpimalCosts() = 0.0");

		assertEquals("First variable ",1.0,pOpt.getValue(0),0.01);
		assertEquals("Second variable ",1.0,pOpt.getValue(1),0.01);
		assertEquals("p.getOptimalCost()",fOpt, 0.0,0.0001);
	}
	
	public static void testLBFGSWithInit_1() {
		System.out.println("==============================================================");
		System.out.println("BFGS minimization of kwadratic cost with initial updates");
		System.out.println("==============================================================");
		SimpleCostFunction f2 = new SimpleCostFunction();
		IVector p2 = new Vector("[2.0,2.0]");
		double f = 1.25;
		IVector g = new Vector("[2.0,0.5]");
		IVector[] s = new Vector[1];
		IVector[] y = new Vector[1];
		s[0] = new Vector("[-2.0,-3.0]");
		y[0] = new Vector("[0.6,-2.0]");
		BFGSCoreOptimizer s2 = new BFGSCoreOptimizer(f2);
        s2.initializeLBFGS(p2,f,g,s,y);
        s2.optimize();
		IVector p2b = s2.getOptimalValue();
		System.out.println("s2.getOptimalValue() = "+p2b);
		System.out.println("Should be close to s2.getOptimalValue() = [1.0,1.0]");
		double f2b = s2.getOptimalCost();
		System.out.println("s2.getOptimalCost() = "+f2b);
		System.out.println("Should be close to s2.getOpimalCosts() = 0.0");
		
		assertEquals("First variable ",1.0,p2b.getValue(0),0.01);
		assertEquals("Second variable ",1.0,p2b.getValue(1),0.01);
		assertEquals("p.getOptimalCost()",f2b, 0.0,0.0001);
	}
	
	public static void testBFGSWithInit_2() {
		System.out.println("==============================================================");
		System.out.println("BFGS minimization of kwadratic cost with initial matrix");
		System.out.println("==============================================================");
		SimpleCostFunction f2 = new SimpleCostFunction();
		IVector p2 = new Vector("[2.0,2.0]");
		double f = 1.25;
		IVector g = new Vector("[2.0,0.5]");
		Matrix M = new Matrix("[1.0,2.0;2.0,0.0]");
		BFGSCoreOptimizer s2 = new BFGSCoreOptimizer(f2);
        s2.initializeBFGS(p2,f,g,M);
        s2.optimize();
		IVector p2b = s2.getOptimalValue();
		System.out.println("s2.getOptimalValue() = "+p2b);
		System.out.println("Should be close to s2.getOptimalValue() = [1.0,1.0]");
		double f2b = s2.getOptimalCost();
		System.out.println("s2.getOptimalCost() = "+f2b);
		System.out.println("Should be close to s2.getOpimalCosts() = 0.0");
		
		assertEquals("First variable ",1.0,p2b.getValue(0),0.01);
		assertEquals("Second variable ",1.0,p2b.getValue(1),0.01);
		assertEquals("p.getOptimalCost()",f2b, 0.0,0.0001);
	}
}
