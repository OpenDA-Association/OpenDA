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

import org.openda.interfaces.IVector;
import org.openda.utils.Matrix;
import org.openda.utils.PrintNumber;
import org.openda.utils.Results;
import org.openda.utils.Vector;

public class RosenbrockCostFunction implements ICostFunctionWithGradient{

	private int factor = 100; // 'narrowness'-factor of the curved valley (default = 100)

	// for saving results
	private int numberEvaluations = 0;
	private int numberGradientEvaluations = 0;
	private IVector pMin = null;     // best sofar
	private double fMin = Double.MAX_VALUE;
	protected java.util.Vector<IVector> allPars = new java.util.Vector<IVector>();
	protected java.util.Vector<Double> allCosts = new java.util.Vector<Double>();
	
	// printing to stdout
	private int maxPrintSize = 100;
	
//	public double evaluate(IVector pVector){
//		++this.numberEvaluations;
//		double pArray[] = pVector.getValues();
//	    double result=0.0;
//	    // NOTE index starts at 0, contrary to fortran version
//
//	    int nop = pArray.length; //number of parameters
//	    for(int iop1=0;iop1<nop;iop1+=2){
//	    	// f = SUM_{i=0}^{n-1} (1-x_{i})^2 + factor*(x_{i+1} - x_{i}^2)^2
//	    	int iop2 = iop1 + 1;
//		    double x1 = pArray[iop1];
//		    double x2 = pArray[iop2];
//		    double scrtch = x1 * x1 - x2;
//		    result += factor * scrtch * scrtch;
//		    scrtch = 1. - x1;
//		    result += scrtch * scrtch;
//	    }
//	    this.allPars.add(pVector.clone());
//	    this.allCosts.add(result);
//	    if(result<this.fMin){
//	    	this.fMin = result;
//	    	this.pMin = pVector.clone();
//	    }
//        Results.putProgression("RosenbrockCostFunction: evaluation " + this.numberEvaluations
//        		+ " : value=" + result + "for p="+pVector.toString());
//		return result;
//	}

	public double evaluate(IVector pVector, String context) {
        ++this.numberEvaluations;
        double pArray[] = pVector.getValues();
        double result=0.0;
        // NOTE index starts at 0, contrary to fortran version

        int nop = pArray.length; //number of parameters
        for(int iop1=0;iop1<nop;iop1+=2){
            // f = SUM_{i=0}^{n-1} (1-x_{i})^2 + factor*(x_{i+1} - x_{i}^2)^2
            int iop2 = iop1 + 1;
            double x1 = pArray[iop1];
            double x2 = pArray[iop2];
            double scrtch = x1 * x1 - x2;
            result += factor * scrtch * scrtch;
            scrtch = 1. - x1;
            result += scrtch * scrtch;
        }
        this.allPars.add(pVector.clone());
        this.allCosts.add(result);
        if(result<this.fMin){
            this.fMin = result;
            this.pMin = pVector.clone();
        }
        Results.putProgression("RosenbrockCostFunction: evaluation " + this.numberEvaluations
                + " : value=" + result + "for p="+pVector.toString());
        return result;
	}

	/**
	 * Get all the costValues evaluated so far
	 * @return costvalue for each evaluation
	 */
	public IVector getCosts(){
		int n = this.allCosts.size();
		Vector result = new Vector(n);
		for(int i=0;i<n;i++){
			result.setValue(i, this.allCosts.get(i));
		}
		return result;
	}
	
	/**
	 * Get the parameters used for each evaluation
	 * @return array of parameters, each as a Vector
	 */
	public IVector[] getParameters(){
		int n = this.allCosts.size();
		IVector[] result = new Vector[n];
		for(int i=0;i<n;i++){
			result[i] = this.allPars.get(i);
		}
		return result;		
	}
	
	/**
	 * Get optimal costs
	 * @return optimal cost as double
	 */
	public double getOptimalCost(){
		double result = this.fMin;
		return result;
	}
	
	/**
	 * Get parameters leading to optimal cost
	 * @return optimal parameters as Vector
	 */
	public IVector getOptimalParameters(){
		if(this.pMin==null){
		    throw new RuntimeException("No costs were computed, thus no optimal parameters exist.");	
		}
		IVector result = this.pMin.clone();
		return result;
	}

    public void writeResults() {
        Results.putMessage("===================================================================");
        Results.putMessage("RosenBrockCostfunction: optimal results");
        Results.putMessage("    number of evaluations: " + this.allCosts.size());
        Results.putMessage("    all cost values:");
        Results.putMessage("        " + this.getCosts().toString());
        Results.putMessage("    all parameter values:");
        Matrix allParsMatrix = new Matrix(this.getParameters());
        Results.putMessage("        " + allParsMatrix.toString());
        Results.putMessage("    best cost:");
        Results.putMessage("        cost = " + PrintNumber.printNumber(this.getOptimalCost()));
        Results.putMessage("    best parameters:");
        Results.putMessage("        " + this.getOptimalParameters().printString("        "));
        Results.putMessage("===================================================================");
        Results.putIterationReport(null, -1, this.getOptimalCost(), this.getOptimalParameters());
    }

	public double getMultiplicationFactor() {
		// TODO Auto-generated method stub
		return 1.0;
	}

	
	public void prepare(IVector p) {
		// no work needed
	}

	public ICostFunction clone(){
		RosenbrockCostFunction result = new RosenbrockCostFunction();
		return result;
		// TODO this is not a proper deep copy
	}
    
	
	public boolean getTryParallel() {
		return false;
	}

	
	public void setTryParallel(boolean tryParallel) {
		// do nothing, never parallel.	
	}

	public IVector evaluateGradient(IVector pVector) {
		++this.numberGradientEvaluations;
		double pArray[] = pVector.getValues();
		IVector result1 = pVector.clone();
		IVector result2 = pVector.clone();
		int n = pArray.length; //number of parameters
		// for factor = 100:
		// df/dx_0 = -4*factor*x_0*(x_1 - x_0^2) - 2*(1 - x_0)
		// df/dx_i = 2*factor*(x_i-x_{i-1}^2) - 4*factor*x_i*(x_{i+1}-x_i^2) - 2*(1-x_i) for 0<i<(n-1)
		// df/dx_{n-1} = 2*factor*(x_{n-1} - x_{n-2}^2)
		result1.setValue(n-1, 0.0);
		result2.setValue(0, 0.0);
		for(int i=0;i<n-1;i++){
			double x0 = pArray[i];
			double x1 = pArray[i+1];
			double dfa = this.factor*-4.0*x0*(x1 - x0*x0) - 2.0*(1.0 - x0);
			double dfb = this.factor*2.0*(x1 - x0*x0);
			result1.setValue(i, dfa);
			result2.setValue(i+1, dfb);
	    }
		result1.axpy(1.0,result2);
		
        Results.putProgression("RosenbrockCostFunction: evaluation Gradient "+
        this.numberGradientEvaluations+" : gradient value="+result1+" for p="+pVector.toString());
        return result1;
	}	

}
