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
import org.openda.interfaces.IResultWriter;
import org.openda.interfaces.ISqrtCovariance;
import org.openda.interfaces.IStochVector;
import org.openda.interfaces.IVector;
import org.openda.utils.Results;
import org.openda.utils.Matrix;
import org.openda.utils.StochVector;
import org.openda.utils.Vector;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;

/**
 * 
 * @author verlaanm
 *
 * Simple least squares cost function for testing
 * 
 * model y=p
 * y_obs = Gaussian with mean= [1.0,1.0] with uncorrelated elements with sigma=[1.0,2.0]
 * p0 = Gaussian with mean= [0.0,0.0] with uncorrelated elements with sigma=[1.0,2.0]
 * 
 * The solution without constraint is 
 * p_opt = [1.0,1.0] sigma = [1.0,2.0]
 * cost = 0.0
 * Pest = [1 0;0 4]
 * with constraint 
 * p_opt = [0.5,0.5] sigma = [0.5,1.0]
 * cost = 1/4+1/16+1/4+1/16 = 0.625
 * Pest = [0.5 0;0 2.0] //TODO fix this
 */


public class SimpleLeastSquaresCostFunction implements LeastSquaresCostFunction{

	//fields for this class
	private IStochVector obs = new StochVector("[1.0,1.0]","[1.0,2.0]");
	private IStochVector par = new StochVector("[0.0,0.0]","[1.0,2.0]");

	//last predictions computed by J.evaluate(p)
	private IVector pred = null;
	private boolean backgroundOn = false;

	// for saving results
	private int numberEvaluations = 0;
	private IVector pMin = null;     // best sofar
	private IVector predMin = null;
	private double fMin = Double.MAX_VALUE;
	private java.util.Vector<IVector> allPars = new java.util.Vector<IVector>();
	private java.util.Vector<Double> allCosts = new java.util.Vector<Double>();
	private java.util.Vector<IVector> allPredictions = new java.util.Vector<IVector>();
	// printing to stdout
	private int maxPrintSize = 10; // maximum number of elements to print

	public SimpleLeastSquaresCostFunction(){
		numberEvaluations = 0;
	}

//	public double evaluate(IVector p){  // Cost = Sum_i ((p(i)-means(i)/width(i))^2
//	    double result=0.0;
//	    //background
//	    if(this.backgroundOn==true){
//	    	IVector res = p.clone();   // compute obs-pred
//		    res.axpy(-1.0, this.par.getExpectations());
//	    	ISqrtCovariance L = this.par.getSqrtCovariance();
//	    	IVector normRes = new Vector(L.getNumberOfColumns());
//            L.rightSolve(res,normRes);
//    	    result += Math.pow(normRes.norm2(),2.0);
//	    }
//	    //map
//	    this.pred = p.clone(); // y=p , i.e. very simple model!
//	    //compare to obs
//	    IVector res = this.obs.getExpectations();   // compute obs-pred
//	    res.axpy(-1.0, pred);
//	    if(obs.hasCorrelatedElements()==true){
//            ISqrtCovariance L = this.obs.getSqrtCovariance();
//            IVector normRes = new Vector(L.getNumberOfColumns());
//            L.rightSolve(res,normRes);
//            res=normRes;
//	    }else{
//	    	res.pointwiseDivide(this.obs.getStandardDeviations());
//	    }
//	    result += Math.pow(res.norm2(),2.0);
//        //save results
//	    this.allPars.add(p.clone());
//	    this.allPredictions.add(pred.clone());
//	    this.allCosts.add(result);
//	    if(result<this.fMin){
//	    	this.fMin = result;
//	    	this.pMin = p.clone();
//	    	this.predMin = pred.clone();
//	    }
//        Results.putProgression("SimpleLeastSquaresCostFunction: evaluation " + ++this.numberEvaluations
//        		+ " cost="+result);
//		return result;
//	}

	public double evaluate(IVector p, String context) { // Cost = Sum_i ((p(i)-means(i)/width(i))^2
        double result=0.0;
        //background
        if(this.backgroundOn==true){
            IVector res = p.clone();   // compute obs-pred
            res.axpy(-1.0, this.par.getExpectations());
            ISqrtCovariance L = this.par.getSqrtCovariance();
            IVector normRes = new Vector(L.getNumberOfColumns());
            L.rightSolve(res,normRes);
            result += Math.pow(normRes.norm2(),2.0);
        }
        //map
        this.pred = p.clone(); // y=p , i.e. very simple model!
        //compare to obs
        IVector res = this.obs.getExpectations();   // compute obs-pred
        res.axpy(-1.0, pred);
        if(obs.hasCorrelatedElements()==true){
            ISqrtCovariance L = this.obs.getSqrtCovariance();
            IVector normRes = new Vector(L.getNumberOfColumns());
            L.rightSolve(res,normRes);
            res=normRes;
        }else{
            res.pointwiseDivide(this.obs.getStandardDeviations());
        }
        result += Math.pow(res.norm2(),2.0);
        //save results
        this.allPars.add(p.clone());
        this.allPredictions.add(pred.clone());
        this.allCosts.add(result);
        if(result<this.fMin){
            this.fMin = result;
            this.pMin = p.clone();
            this.predMin = pred.clone();
        }
        Results.putProgression("SimpleLeastSquaresCostFunction: evaluation " + ++this.numberEvaluations
                + " cost="+result);
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
        Results.putMessage("SimpleLeastSquaresCostfunction: optimal results");
        Results.putMessage("    number of evaluations: " + this.allCosts.size());
        Results.putMessage("    all cost values:");
        Results.putMessage("        " + this.getCosts().toString());
        Results.putMessage("    all parameter values");
        Matrix allParsMatrix = new Matrix(this.getParameters());
        Results.putMessage("        " + allParsMatrix.toString());
        Results.putMessage("    best cost:");
        Results.putMessage("        cost = " + printNumber(this.getOptimalCost()));
        Results.putMessage("    best parameters");
        Results.putMessage("        " + this.getOptimalParameters().printString("        "));
        if(this.obs.getExpectations().getSize()<=this.maxPrintSize){
            IVector obs=this.obs.getExpectations();
            IVector prd=this.predMin;
            Results.putMessage("best predictions");
            Results.putValue("predOpt", prd, prd.getSize(), "any", IResultWriter.OutputLevel.Essential , IResultWriter.MessageType.OuterIteration);
            Results.putMessage("observations (expectation)");
            Results.putValue("obs", obs, obs.getSize(), "any", IResultWriter.OutputLevel.Essential , IResultWriter.MessageType.OuterIteration);
        }
        Results.putMessage("===================================================================");
        Results.putIterationReport(null, -1, this.getOptimalCost(), this.getOptimalParameters());
    }


	/*
	 *
	 * additional methods for least-squares problems
	 *
	 */


    /**
     * Get mean and covariance for the parameters
     * @return StochVector with description of uncertainty for the parameters
     */
    public IStochVector getParameterUncertainty(){
    	IStochVector result=this.par;
    	return result;
    }

    /**
     * Get predictions corresponding to the last call to evaluate so far
     * @return Vector with predictions
     */
    public IVector getLastPredictions(){
    	IVector result = this.pred;
    	return result;
    }

    /**
     * Get uncertainty for the observations
     * @return StochVector with observation uncertainties
     */
    public IStochVector getObservationUncertainty(){
    	IStochVector result = this.obs;
    	return result;
    }

    /**
     * Use background term for cost function, i.e. add Jb = (p-p0)'/B(p-p0)
     * @param onIsTrue to turn backgroundterm on(true) of off(false)
     */
    public void setBackgroundTerm(boolean onIsTrue){
    	this.backgroundOn = onIsTrue;
    }

    /**
     * Test if background term is in use for this cost function
     * @return
     */
    public boolean doAddBackgroundTerm(){
    	boolean result = this.backgroundOn;
    	return result;
    }

    /**
     * Get predictions corresponding to the lowest cost value evaluated so far
     * @return Vector with predictions
     */
    public IVector getOptimalPredictions(){
		if(this.predMin==null){
		    throw new RuntimeException("No costs were computed, thus no optimal predictions exist.");
		}
		IVector result = this.predMin.clone();
		return result;
    }

    /**
     * Get predictions for each function evaluation until now
     * @return Array of Vectors one for each evaluation predictions
     */
    public IVector[] getAllPredictions(){
		int n = this.allCosts.size();
		IVector[] result = new Vector[n];
		for(int i=0;i<n;i++){
			result[i] = this.allPredictions.get(i);
		}
    	return result;
    }

    // Note: copied from Vector - should be refactored (AM)
    private String printNumber(double value) {
        DecimalFormatSymbols symbols = new DecimalFormatSymbols();
        symbols.setDecimalSeparator('.');
        DecimalFormat formatFloat = new DecimalFormat("0.###", symbols);
        DecimalFormat formatExponent = new DecimalFormat("0.###E0", symbols);

        if (Math.abs(value) > 0.01 && Math.abs(value) < 1000.0 || value == 0.0) {
            return formatFloat.format(value);
        } else {
            return formatExponent.format(value);
        }
    }
    
	
	public double getMultiplicationFactor() {
		// TODO Auto-generated method stub
		return 1.0;
	}

	
	public void prepare(IVector p) {
		// no computations needed
	}

	public LeastSquaresCostFunction clone(){
		SimpleLeastSquaresCostFunction result = new SimpleLeastSquaresCostFunction();
		return result;
		//TODO This is no proper deep copy 
	}

	
	public boolean getTryParallel() {
		return false;
	}

	
	public void setTryParallel(boolean tryParallel) {
		// do nothing, never parallel.	
	}

}
