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
package org.openda.algorithms.kalmanFilter;
import org.openda.algorithms.LeastSquaresCostFunction;
import org.openda.interfaces.*;
import org.openda.utils.Results;
import org.openda.utils.StochVector;
import org.openda.utils.Vector;

/**
 * Cost-function for the analysis step in a sequential data-assimilation scheme.
 * J(x) = (x-x_f)'*inv(P)*(x-x_f)  + (y_obs - H(x))'*inv(R)*(y_obs - H(x))
 * 
 * A low rank preconditioning is used that is based on the ensemble
 * x = x_f + L * xt
 * with x_f = 1/n sum xi_i and xi_i is the i'th ensemble state 
 * and L = sqrt(1/(n-1)) * [xi_1 - x_f, ... ,xi_n - x_f]
 * 
 * This leads to 
 * 
 * J(xt) = factor * xt'*xt + factor * (y_obs - H(x_f + L * xt))'*inv(R)*(y_obs - H(x_f + L * xt))
 * 
 * For initial values we use 
 * 
 * xt_i =  sqrt(n-1) * e_i 
 *      =  [0, ..., 0, sqrt(n-1), 0, ...., 0 ]
 * This results in 
 * x_i  = xi_i  i.e. the original ensemble members
 * 
 * NOTE that although this method is developed for non-linear observation operators, it 
 * still assumes Gaussian additive errors. It also assumes a Gaussian distribution for the
 * forecast errors.
 *
 * Details:
 *  - xt is taken from the modelInstance
 *  - y_obs is provided separately to account for perturbed obs (!! ignore observer.getValues() )
 *  - R is taken from the observer
 *  - h comes from the modelInstance 
 *  - the modelInstance is modified, thus use save/restore to keep previous values
 *  
 * @author verlaanm
 *
 */
public class AnalysisLeastSquaresCost implements LeastSquaresCostFunction {
	// data for constructing the costfunction
	boolean backGroundOn       = true; // use term for background errorcovariance
	IStochModelInstance model   = null;
    IVector meanState           = null; // mean of the ensemble
	IVector deltaStates[]       = null; // ensemble members minus meanState sqrt(n-1)*L
	IVector predictions[]       = null;
    IStochObserver observations = null;
    IVector obsValues           = null;
    IStochVector parameterUncertainty = null; // uses transformed space xt
    IStochVector observationUncertainty = null;
    IModelState savedState      = null;
    double factor               = 0.5; //use default factor of 0.5 for cost values
    // collecting results
	int numberEvaluations = 0;
	IVector pMin = null;     // best parameters sofar
	IVector predMin = null;  // best predictions
	double fMin = Double.MAX_VALUE;
	private java.util.Vector<IVector> allPars = new java.util.Vector<IVector>();
	private java.util.Vector<IVector> allPreds = new java.util.Vector<IVector>();
	private java.util.Vector<Double> allCosts = new java.util.Vector<Double>();
	private IVector lastPredicted=null;
	private IVector bestState=null;

	// printing to stdout
	int maxPrintSize = 100;

	/**
	 * Constructor for Analysis cost-function
	 * @param model StochModelInstance to use for model operations
	 * @param states Ensemble of states used for background term and preconditioning
	 * @param stochObserver Stochastic Observer
	 * @param obsValues observed values (i.e. computed values at obs. locations, including their uncertainty)
     * @param savedState present model state
	 */
	public AnalysisLeastSquaresCost(IStochModelInstance model, IVector states[],
			IStochObserver stochObserver, IVector obsValues, IModelState savedState){
		this.model = model;
		this.observations = stochObserver;
		this.obsValues = obsValues;
		
		//derived data
		int n=states.length;
		IVector mean  = new Vector(n);
		IVector stdev = new Vector(n); stdev.setConstant(1.0); // uncertainty for transformed state
		this.parameterUncertainty = new StochVector(mean,stdev);
		// mean state
		this.meanState = states[0].clone();
		for(int i=1;i<states.length;i++){
			this.meanState.axpy(1.0, states[i]);
		}
		this.meanState.scale(1.0/n);
		// deltaStates 
		this.deltaStates = new IVector[states.length];
		for(int i=0;i<states.length;i++){
			this.deltaStates[i] = states[i].clone();
			this.deltaStates[i].axpy(-1.0, this.meanState);
		} 
		// observation uncertainty
		this.observationUncertainty = new StochVector(obsValues,stochObserver.getStandardDeviations());
		// save present state for the model
		this.savedState=savedState;
	}

	
	public IVector[] getAllPredictions() {
		int n = this.allCosts.size();
		IVector[] result = new IVector[n];
		for(int i=0;i<n;i++){
			result[i] = this.allPreds.get(i);
		}
		return result;
	}

	
	public boolean doAddBackgroundTerm() {
		return this.backGroundOn;
	}

	
	public IVector getLastPredictions() {
        return this.lastPredicted.clone();
	}

	
	public IStochVector getObservationUncertainty() {
        return this.observationUncertainty;
	}

	
	public IVector getOptimalPredictions() {
	   	return this.predMin.clone();
	}

	
	public IStochVector getParameterUncertainty() {
        return this.parameterUncertainty;
	}

	
	public void setBackgroundTerm(boolean onIsTrue) {
		this.backGroundOn = onIsTrue;
	}

//	
//	public double evaluate(IVector p) {
//		++this.numberEvaluations;
//	    double totalCost=0.0;
//    	//Results.putMessage("========================================================================");
//        //Results.putMessage("no"+ this.numberEvaluations);
//	    this.model.restoreInternalState(this.savedState);
//	    // transform to state
//	    int n = this.deltaStates.length;
//	    IVector deltaState = this.deltaStates[0].clone();
//	    deltaState.scale(1.0/Math.sqrt(n-1.0)*p.getValue(0));
//	    for(int i=1;i<n;i++){
//	    	deltaState.axpy(1.0/Math.sqrt(n-1.0)*p.getValue(i), this.deltaStates[i]);
//	    }
//	    // insert new state
//        this.model.axpyOnState(1.0, deltaState); // add changes instead of setting values to min side effects
//
//        /*
//         *  Jb background term
//         *  Jb = Sum_i factor *((p(i)-means(i)/width(i))^2
//         */
//	    if(this.backGroundOn){
//	    	// check for zero standard deviations
//	    	IVector stdPar=this.parameterUncertainty.getStandardDeviations();
//	    	for(int i=0;i<stdPar.getSize();i++){
//	    		double sigma = stdPar.getValue(i);
//	    		if(sigma<1e-8){
//	    			//Results.putProgression("Error: Parameter "+(i+1)+" has zero or negative standard deviation.\n Can not compute penalty term.");
//	    			throw new RuntimeException("Error: Parameter "+(i+1)+" has zero or negative standard deviation.\n Can not compute penalty term.");
//	    		}
//	    	}
//	    	double backgroundCost = Math.pow(p.norm2(),2.0);
//	    	backgroundCost *= this.factor;
//	    	totalCost += backgroundCost;
//	    }
//
//	    /*
//	     *  Jo observation term
//	     *  Jo = sum_k factor ((obs_k - prd_k)/sigma_o)^2
//	     */
//	    // get model values corresponding to observations
//	    IObservationDescriptions descr = this.observations.getObservationDescriptions();
//	    // get results as Vectors
//	    IVector prd      = this.model.getObservedValues(descr);
//	    this.lastPredicted=prd;
//	    IVector obsMean  = this.obsValues;
//	    IVector obsStd   = this.observationUncertainty.getStandardDeviations();
//	    // residuals obs-prd
//	    IVector residuals= obsMean.clone();
//	    residuals.axpy(-1.0, prd); //TODO fails for periodic variables
//	    if(prd.getSize()<this.maxPrintSize){
//	    }
//	    // Jo = sum_k factor ((obs_k - prd_k)/sigma_o)^2
//	    IVector temp     = residuals.clone();
//	    temp.pointwiseDivide(obsStd);
//	    double obsCost = Math.pow(temp.norm2(),2.0);
//	    obsCost *= this.factor;
//	    totalCost += obsCost;
//	    this.allPars.add(p.clone());
//	    this.allPreds.add(this.lastPredicted);
//	    this.allCosts.add(totalCost);
//	    if(totalCost<this.fMin){
//	    	this.fMin      = totalCost;
//	    	this.pMin      = p.clone();
//	    	this.bestState = this.model.getState();
//	    	this.predMin   = this.lastPredicted.clone();
//	    }
//        Results.putProgression("AnalysisLeastSquaresCost: evaluation " + this.numberEvaluations +" : cost = "+totalCost);
//
//	    return totalCost;
//	}

	
	public double evaluate(IVector p, String context) {
        ++this.numberEvaluations;
        double totalCost=0.0;
        //Results.putMessage("========================================================================");
        //Results.putMessage("no"+ this.numberEvaluations);
        this.model.restoreInternalState(this.savedState);
        // transform to state
        int n = this.deltaStates.length;
        IVector deltaState = this.deltaStates[0].clone();
        deltaState.scale(1.0/Math.sqrt(n-1.0)*p.getValue(0));
        for(int i=1;i<n;i++){
            deltaState.axpy(1.0/Math.sqrt(n-1.0)*p.getValue(i), this.deltaStates[i]);
        }
        // insert new state
        this.model.axpyOnState(1.0, deltaState); // add changes instead of setting values to min side effects

        /*
         *  Jb background term
         *  Jb = Sum_i factor *((p(i)-means(i)/width(i))^2
         */
        if(this.backGroundOn){
            // check for zero standard deviations
            IVector stdPar=this.parameterUncertainty.getStandardDeviations();
            for(int i=0;i<stdPar.getSize();i++){
                double sigma = stdPar.getValue(i);
                if(sigma<1e-8){
                    //Results.putProgression("Error: Parameter "+(i+1)+" has zero or negative standard deviation.\n Can not compute penalty term.");
                    throw new RuntimeException("Error: Parameter "+(i+1)+" has zero or negative standard deviation.\n Can not compute penalty term.");
                }
            }
            double backgroundCost = Math.pow(p.norm2(),2.0);
            backgroundCost *= this.factor;
            totalCost += backgroundCost;
        }

        /*
         *  Jo observation term
         *  Jo = sum_k factor ((obs_k - prd_k)/sigma_o)^2
         */
        // get model values corresponding to observations
        IObservationDescriptions descr = this.observations.getObservationDescriptions();
        // get results as Vectors
        IVector prd      = this.model.getObservedValues(descr);
        this.lastPredicted=prd;
        IVector obsMean  = this.obsValues;
        IVector obsStd   = this.observationUncertainty.getStandardDeviations();
        // residuals obs-prd
        IVector residuals= obsMean.clone();
        residuals.axpy(-1.0, prd); //TODO fails for periodic variables
        if(prd.getSize()<this.maxPrintSize){
        }
        // Jo = sum_k factor ((obs_k - prd_k)/sigma_o)^2
        IVector temp     = residuals.clone();
        temp.pointwiseDivide(obsStd);
        double obsCost = Math.pow(temp.norm2(),2.0);
        obsCost *= this.factor;
        totalCost += obsCost;
        this.allPars.add(p.clone());
        this.allPreds.add(this.lastPredicted);
        this.allCosts.add(totalCost);
        if(totalCost<this.fMin){
            this.fMin      = totalCost;
            this.pMin      = p.clone();
            this.bestState = this.model.getState();
            this.predMin   = this.lastPredicted.clone();
        }
        Results.putProgression("AnalysisLeastSquaresCost: evaluation " + this.numberEvaluations +" : cost = "+totalCost);

        return totalCost;
	}

	
	public IVector getCosts() {
		int n = this.allCosts.size();
		Vector result = new Vector(n);
		for(int i=0;i<n;i++){
			result.setValue(i, this.allCosts.get(i));
		}
		return result;
	}

	
	public double getMultiplicationFactor() {
		// TODO Auto-generated method stub
		return this.factor;
	}

	
	public double getOptimalCost() {
        return this.fMin;
	}

	
	public IVector getOptimalParameters() {
		if(this.pMin==null){
		    throw new RuntimeException("No costs were computed, thus no optimal parameters exist.");
		}
        return this.pMin.clone();
	}

	
	public IVector[] getParameters() {
		int n = this.allCosts.size();
		IVector[] result = new IVector[n];
		for(int i=0;i<n;i++){
			result[i] = this.allPars.get(i);
		}
		return result;
	}

	
	public void writeResults() {
		//Results.putMessage("===================================================================");
		//Results.putMessage("AnalysisLeastSquaresCost:");
		////Results.putMessage("number of evaluations");
		////Results.putMessage("n = "+this.allCosts.size());
		////Results.putMessage("all cost values");
		////Results.putMessage("costs = "+this.getCosts().toString());
		////Results.putMessage("all parameter values");
		// Matrix allParsMatrix = new Matrix(this.getParameters());
		////Results.putMessage("parameters="+allParsMatrix.toString());
		////Results.putMessage("number of observations");
		////Results.putMessage("nobs="+this.lastPredicted.getSize());
		////Results.putMessage("best cost");
		////Results.putValue("costOpt",this.getOptimalCost());
		////Results.putMessage("best parameters");
		////Results.putMessage("===================================================================");
        ////Results.putValue("pOpt",this.getOptimalParameters());
	}
	
	/**
	 * Get the state vector which resulted in the best cost sofar.
	 * Contrary to getOptimalParameters() this function returns the non-transformed state.
	 * @return optimal state
	 */
	public IVector getOptimalState() {
        return this.bestState;
	}

	
	public void prepare(IVector p) {
		// TODO Auto-generated method stub
		
	}

	public LeastSquaresCostFunction clone(){
		LeastSquaresCostFunction result = null;
		throw new RuntimeException("LeastSquaresCostFunction.clone() not implemented");
		//return result;
		// TODO create content for this method
	}

	
	public boolean getTryParallel() {
		return false;
	}

	
	public void setTryParallel(boolean tryParallel) {
		// do nothing, never parallel.	
	}
}
