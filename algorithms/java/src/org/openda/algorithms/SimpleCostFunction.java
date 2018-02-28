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
import org.openda.interfaces.IVector;
import org.openda.utils.Matrix;
import org.openda.utils.PrintNumber;
import org.openda.utils.Results;
import org.openda.utils.Vector;

public class SimpleCostFunction implements ICostFunctionWithGradient{
	//fields for this class
	double means[] = {1.0,1.0};
	double width[] = {1.0,2.0};
	
	// for saving results
	int numberEvaluations = 0;
	int numberGradientEvaluations = 0;
	IVector pMin = null;     // best sofar
	double fMin = Double.MAX_VALUE;
	private java.util.Vector<IVector> allPars = new java.util.Vector<IVector>();
	private java.util.Vector<Double> allCosts = new java.util.Vector<Double>();
	// printing to stdout
	int maxPrintSize = 100;
	
	public SimpleCostFunction(){
		numberEvaluations = 0;
		numberGradientEvaluations = 0;
	}
	
//	public double evaluate(IVector p){  // Cost = Sum_i ((p(i)-means(i)/width(i))^2
//	    double result=0.0;
//	    for(int i=0;i<this.means.length;i++){
//	    	result+= Math.pow((p.getValue(i)-this.means[i])/this.width[i], 2.0);
//	    }
//	    this.allPars.add(p.clone());
//	    this.allCosts.add(result);
//	    if(result<this.fMin){
//	    	this.fMin = result;
//	    	this.pMin = p.clone();
//	    }
//        Results.putProgression("SimpleCostFunction: evaluation " + ++this.numberEvaluations
//        		+ " : value=" + result + " for p="+p.toString());
//
//		return result;
//	}

	
	public double evaluate(IVector p, String context) { // Cost = Sum_i ((p(i)-means(i)/width(i))^2
        double result=0.0;
        for(int i=0;i<this.means.length;i++){
            result+= Math.pow((p.getValue(i)-this.means[i])/this.width[i], 2.0);
        }
        this.allPars.add(p.clone());
        this.allCosts.add(result);
        if(result<this.fMin){
            this.fMin = result;
            this.pMin = p.clone();
        }
        Results.putProgression("SimpleCostFunction: evaluation " + ++this.numberEvaluations
                + " : value=" + result + " for p="+p.toString());

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
        Results.putMessage("SimpleCostfunction: optimal results");
        Results.putMessage("    number of evaluations: " + this.allCosts.size());
        Results.putMessage("    all cost values");
        Results.putMessage("        " + this.getCosts().printString(""));
        Results.putMessage("    all parameter values");
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
		// do nothing in this case
	}

	public ICostFunction clone(){
		SimpleCostFunction result = new SimpleCostFunction();
		return result;
		// TODO NOTE this is not a proper deep copy
	}

	
	public boolean getTryParallel() {
		return false;
	}

	
	public void setTryParallel(boolean tryParallel) {
		// do nothing, never parallel.	
	}

	
	public IVector evaluateGradient(IVector p) {
		IVector result = p.clone();
	    for(int i=0;i<this.means.length;i++){
	    	result.setValue(i, 2.0*(p.getValue(i)-this.means[i])/this.width[i]/this.width[i]);
	    }
        Results.putProgression("SimpleCostFunction: evaluation gradient " + ++this.numberGradientEvaluations
		+ " : gradient value=" + result + " for p="+p.toString());
        
		return result;
	}
}
