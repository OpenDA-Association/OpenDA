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
import org.openda.utils.Vector;

public class ConjugateGradientCoreOptimizerTest extends TestCase {


	public static void testConGradKwadratic_1() {
		System.out.println("==============================================================");
		System.out.println("Conjugate Gradient minimization of a simple kwadratic function");
		System.out.println("==============================================================");
		SimpleCostFunction f2 = new SimpleCostFunction();
		IVector p2 = new Vector("[2.0,2.0]");
		ConjugateGradientCoreOptimizer p = new ConjugateGradientCoreOptimizer(f2);
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

	public static void testConGradRosenbrock_1() {
		System.out.println("==============================================================");
		System.out.println("Conjugate Gradient minimization of the Rosenbrock function");
		System.out.println("==============================================================");
		ICostFunctionWithGradient fRb2 = new RosenbrockCostFunction();
		IVector p2 = new Vector("[0.0,0.0]");
		ConjugateGradientCoreOptimizer pRb = new ConjugateGradientCoreOptimizer(fRb2);
		pRb.initialize(p2);
		pRb.relTolStep = 0.0001;
		pRb.absTolStep = 0.0001;
		pRb.relTolBrent = 0.00001;
		pRb.absTolBrent = 0.00001;
		pRb.method = 1;
		pRb.optimize();
		IVector pOpt = pRb.getOptimalValue();
		System.out.println("pRb.getOptimalValue() = "+pOpt);
		System.out.println("Should be close to pRb.getOptimalValue() = [1.0,1.0]");
		double fOpt = pRb.getOptimalCost();
		System.out.println("pRb.getOptimalCost() = "+fOpt);
		System.out.println("Should be close to pRb.getOpimalCosts() = 0.0");

		assertEquals("First variable ",1.0,pOpt.getValue(0),0.01);
		assertEquals("Second variable ",1.0,pOpt.getValue(1),0.01);
		assertEquals("p.getOptimalCost()",fOpt, 0.0,0.0001);
	}

	public static void testConGradWithInit() {
		System.out.println("==============================================================");
		System.out.println("Conjugate Gradient minimization of kwadratic cost with initialization");
		System.out.println("==============================================================");
		SimpleCostFunction f2 = new SimpleCostFunction();
		IVector p2 = new Vector("[2.0,2.0]"); double fp2 = 1.25;
		IVector g = new Vector("[1.0,2.0]"); IVector s = new Vector("[1.0,0.5]");
		ConjugateGradientCoreOptimizer s2 = new ConjugateGradientCoreOptimizer(f2);
        s2.initialize(p2,fp2,g,s); 
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
