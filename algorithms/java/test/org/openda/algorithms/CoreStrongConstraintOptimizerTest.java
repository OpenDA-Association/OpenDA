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
//import org.openda.algorithms.*;

public class CoreStrongConstraintOptimizerTest extends TestCase {

        public static void testDummy(){}


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
		IVector p1 = new Vector("[1.0,1.0]");
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
		assertEquals("s2.getOptimalValue()",""+p1b.toString(), "[0.7,0.6]");
		double f1b = d1.getOptimalCost();
		System.out.println("d1.getOptimalCost() = "+f1b);
		System.out.println("Should be s2.getOpimalCosts()[1] = 0.71");
		assertEquals("s2.getOptimalCost()",""+f1b, "0.71");
		IVector[] p1c = d1.getCurrentValues();
		double[] f1c = d1.getCurrentCosts();
		for(int i=0;i<p1c.length;i++){
			System.out.println("par["+i+"]="+p1c[i]);
			System.out.println("cost["+i+"]="+f1c[i]);
		}
		
	}

}
