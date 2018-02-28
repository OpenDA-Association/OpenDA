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

import org.openda.algorithms.ICostFunctionWithGradient;
import org.openda.algorithms.LeastSquaresCostFunction;
import org.openda.interfaces.*;
import org.openda.utils.Results;
import org.openda.utils.StochVector;
import org.openda.utils.Vector;

/**
 * Cost-function for the analysis step in a sequential data-assimilation scheme.
 * J(x) = (x-x_f)'*inv(P)*(x-x_f)  + (y_obs - H(x))'*inv(R)*(y_obs - H(x))
 * 
 * Preconditioning is applied by defining x = x_f + L * xi, where L = B^{1/2} 
 * 
 * This leads to 
 * 
 * J(xi) = factor * xi'*xi + factor * (y_obs - H(x_f + L * xi))'*inv(R)*(y_obs - H(x_f + L * xi))
 * 
 * NOTE that this method needs a linear (or linearized) observation operator 
 * We assume Gaussian forecast errors.
 *
 * Details:
 *  - xi is taken from the modelInstance
 *  - y_obs is provided separately to account for perturbed obs (!! ignore observer.getValues() )
 *  - R is taken from the observer
 *  - H comes from the modelInstance 
 *  - the modelInstance is modified, thus use save/restore to keep previous values
 *
 */

public class AnalysisLeastSquaresCostWithGradient implements LeastSquaresCostFunction,ICostFunctionWithGradient{

	// data for constructing the cost function
	boolean backGroundOn = true;
	IStochModelInstance model = null;
	IStochObserver observations = null;
	IVector obsValues = null;
	ISqrtCovariance L = null;
	IStochVector stateUncertainty = null;
	IStochVector observationUncertainty = null;

	IModelState savedState = null;
	IVector initialState=null;
	double factor = 0.5; //default 0.5

	// results
	int numberOfEvaluations = 0;
	int numberOfGradEval = 0;
	IVector pMin = null;     // best parameters so far
	IVector predMin = null;  // best predictions

	double fMin = Double.MAX_VALUE;
	private java.util.Vector<IVector> allPars = new java.util.Vector<IVector>();
	private java.util.Vector<IVector> allPreds = new java.util.Vector<IVector>();
	private java.util.Vector<Double> allCosts = new java.util.Vector<Double>();
	private java.util.Vector<IVector> allGrads = new java.util.Vector<IVector>();
	private IVector lastPredicted=null;
	private IVector bestState=null;

	public AnalysisLeastSquaresCostWithGradient(IStochModelInstance model, IStochObserver stochObserver,
			IVector obsValues, IModelState savedState) {
		this.model = model;
		this.initialState = model.getState();
		this.observations = stochObserver;
		this.obsValues = obsValues;
		this.savedState = savedState;

		// uncertainties
		this.stateUncertainty = this.model.getStateUncertainty();
		this.L = this.stateUncertainty.getSqrtCovariance();
		this.observationUncertainty = new StochVector(this.obsValues,this.observations.getStandardDeviations());
	}

	
	public void prepare(IVector p) {
		// Auto-generated method stub
	}

//	public double evaluate(IVector p) {
//		//
//		// J = factor * p'*p + factor * (y_obs - H(x_f + L * p))' * inv(R) * (y_obs - H(x_f + L * p))
//		//
//		++this.numberOfEvaluations;
//		double cost = 0.0;
//
//		if(this.backGroundOn){
//			// J = factor * p'*p
//			cost = Math.pow(p.norm2(),2.0);
//			cost *= this.factor;
//		}
//
//		// derive x_f + L*p
//		IVector deltaState = this.initialState.clone();
//		this.L.rightMultiply(1.0, p, 0.0, deltaState);
//
//		// redefine state
//		this.model.axpyOnState(1.0, deltaState);
//
//		// get model values corresponding to the observations: H * [x + L*p]
//		IObservationDescriptions descr = this.observations.getObservationDescriptions();
//
//		// J = J + factor (y - pred)^2/sigma^2
//		IVector prd = this.model.getObservedValues(descr);
//		this.lastPredicted = prd;
//		IVector residuals = this.obsValues.clone();
//		residuals.axpy(-1.0, prd);
//
//		IVector scaledResiduals = null;
//		if (this.observationUncertainty.hasCorrelatedElements()) {
//			throw new RuntimeException("Error: Observations are correlated. AnalysesLeastSquareCostWithGradient supports uncorrelated observations only.");
//		} else {
//			//scale residuals
//			IVector stdSquare = this.observationUncertainty.getStandardDeviations();
//			scaledResiduals = residuals.clone();
//			scaledResiduals.pointwiseDivide(stdSquare);
//		}
//		double obsCost = Math.pow(scaledResiduals.norm2(),2.0);
//		obsCost *= this.factor;
//
//		cost += obsCost;
//
//		Results.putProgression("AnalysisLeastSquaresCostWitGradient: evaluation " + this.numberOfEvaluations +" : cost = "+cost);
//
//		this.allPars.add(p.clone());
//		this.allPreds.add(this.lastPredicted);
//		this.allCosts.add(cost);
//		if(cost<this.fMin){
//			this.fMin      = cost;
//			this.pMin      = p.clone();
//			this.bestState = this.model.getState();
//			this.predMin   = this.lastPredicted.clone();
//		}
//
//		this.model.restoreInternalState(this.savedState);
//
//		return cost;
//	}

	
	public double evaluate(IVector p, String context) {
		//
		// J = factor * p'*p + factor * (y_obs - H(x_f + L * p))' * inv(R) * (y_obs - H(x_f + L * p))
		//
		++this.numberOfEvaluations;
		double cost = 0.0;

		if(this.backGroundOn){
			// J = factor * p'*p
			cost = Math.pow(p.norm2(),2.0);
			cost *= this.factor;
		}

		// derive x_f + L*p
		IVector deltaState = this.initialState.clone();
		this.L.rightMultiply(1.0, p, 0.0, deltaState);

		// redefine state
		this.model.axpyOnState(1.0, deltaState);

		// get model values corresponding to the observations: H * [x + L*p]
		IObservationDescriptions descr = this.observations.getObservationDescriptions();

		// J = J + factor (y - pred)^2/sigma^2
		IVector prd = this.model.getObservedValues(descr);
		this.lastPredicted = prd;
		IVector residuals = this.obsValues.clone();
		residuals.axpy(-1.0, prd);

		IVector scaledResiduals = null;
		if (this.observationUncertainty.hasCorrelatedElements()) {
			throw new RuntimeException("Error: Observations are correlated. AnalysesLeastSquareCostWithGradient supports uncorrelated observations only.");
		} else {
			//scale residuals
			IVector stdSquare = this.observationUncertainty.getStandardDeviations();
			scaledResiduals = residuals.clone();
			scaledResiduals.pointwiseDivide(stdSquare);
		}
		double obsCost = Math.pow(scaledResiduals.norm2(),2.0);
		obsCost *= this.factor;

		cost += obsCost;

		Results.putProgression("AnalysisLeastSquaresCostWitGradient: evaluation " + this.numberOfEvaluations +" : cost = "+cost);

		this.allPars.add(p.clone());
		this.allPreds.add(this.lastPredicted);
		this.allCosts.add(cost);
		if(cost<this.fMin){
			this.fMin      = cost;
			this.pMin      = p.clone();
			this.bestState = this.model.getState();
			this.predMin   = this.lastPredicted.clone();
		}

		this.model.restoreInternalState(this.savedState);

		return cost;
	}

	public IVector evaluateGradient(IVector p) {
		//
		// grad J = 2 * factor * p + 2 * factor * L' * \mathbf{H}' * inv(R) * (y_obs - \mathcal{H}(x_f + L * p))
		//
		++this.numberOfGradEval;

		IVector grad=null;
		if(this.backGroundOn){
			// grad J = 2 * factor * p
			 grad = p.clone();
			grad.scale(2.0*factor);		
		}else{
			grad=p.clone();
			grad.setConstant(0.0);
		}

		// derive x = x_f + L*p
		IVector deltaState = this.initialState.clone();
		this.L.rightMultiply(1.0, p, 0.0, deltaState);
		// redefine state
		this.model.axpyOnState(1.0, deltaState);

		// get model values corresponding to observations: H * [x + L*p]
		IObservationDescriptions descr = this.observations.getObservationDescriptions();

		// grad J = grad J + 2 * factor * L' * H' * (y - pred)/(sigma^2)
		IVector prd = this.model.getObservedValues(descr);
		IVector residuals = this.obsValues.clone();
		residuals.axpy(-1.0, prd);
		
		IVector scaledResiduals = null;
		if (this.observationUncertainty.hasCorrelatedElements()) {
			throw new RuntimeException("Error: Observations are correlated. AnalysesLeastSquareCostWithGradient supports uncorrelated observations only.");
		} else {
			//scale residuals
			IVector stdSquare = this.observationUncertainty.getStandardDeviations();
			stdSquare.pointwiseMultiply(stdSquare);
			scaledResiduals = residuals.clone();
			scaledResiduals.pointwiseDivide(stdSquare);
		}
		
		// \mathbf{H}' * inv(R) * (y-pred)
		IVector HScalRes = ((IModelAdjoint) this.model).applyObservationAdjoint(scaledResiduals,descr);
		
		// L' * [ \mathbf{H}' * inv(R) * (y-pred) ]
		IVector gradobs = grad.clone();
		this.L.leftMultiply(-1.0,HScalRes,0.0,gradobs);

		// final gradient
		grad.axpy(2.0*factor,gradobs);

		this.allGrads.add(grad);

		this.model.restoreInternalState(this.savedState);

		return grad;
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
		throw new UnsupportedOperationException("TODO: should return identity of proper size");
	}

	
	public void setBackgroundTerm(boolean onIsTrue) {
		this.backGroundOn = onIsTrue;
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
		// Auto-generated method stub
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
		Results.putMessage("===================================================================");
		Results.putMessage("AnalysisLeastSquaresCostWithGradient");
		Results.putMessage("===================================================================");
		Results.putMessage("number of evaluations: "+this.allCosts.size());
		Results.putMessage("minimum cost: "+this.getOptimalCost());
		Results.putMessage("minimizing parameters: "+this.getOptimalParameters());
		Results.putMessage("===================================================================");
	}

	/**
	 * Get the state vector which resulted in the best cost so far.
	 * Contrary to getOptimalParameters() this function returns the non-transformed state.
	 * @return optimal state
	 */
	public IVector getOptimalState() {
		return this.bestState;
	}

	public AnalysisLeastSquaresCostWithGradient clone(){
		throw new RuntimeException("LeastSquaresCostFunctionWithGradient.clone() not implemented");
		// TODO implement .clone()
		// AnalysisLeastSquaresCostWithGradient result = new AnalysisLeastSquaresCostWithGradient(null,null,null,null);
		// return result;
	}

	
	public boolean getTryParallel() {
		return false;
	}

	
	public void setTryParallel(boolean tryParallel) {
		// do nothing, never parallel.	
	}

}
