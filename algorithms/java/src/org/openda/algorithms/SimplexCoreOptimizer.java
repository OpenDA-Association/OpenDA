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


import org.openda.interfaces.IMatrix;
import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.IResultWriter;
import org.openda.interfaces.IVector;
import org.openda.utils.Matrix;
import org.openda.utils.Results;
import org.openda.utils.Vector;

import java.util.ArrayList;
import java.util.List;

public class SimplexCoreOptimizer{

	// fields of this class
	IVector pCurrent[] = null; //values under consideration
	double fCurrent[] = null;
	IVector predCurrent[] = null; //predictions for each estimate (in case of LeastSquaresCostFunction)
	int nparam=0;
	ICostFunction f = null;
	int maxitSimplex = 100;
	double absTolSimplex=0.01;
	double relTolSimplex=0.01;

    // required for the additional stopping criteria:
    public List<IStopCriterion> stopCriteria = new ArrayList<IStopCriterion>();
    public List<Double> stopCriteriaThreshold = new ArrayList<Double>();
    public double stopCritThresDefault = 0.01;
    public IObservationDescriptions obsDescr=null;
	
    // constants
	double rconst = -1;       // constant for reflection
	double econst =  2.0;     // constant for exspansion
	double cconst = .5;       // constant for contractions
	double sconst = .5;       // constant for scaling
	
	//stopping
	boolean moreToDo = true;         // is this optimization finished
	int imain=0;                     // main iterations done


    /**
     * Constructor for Simplex minimization
     * @param f : cost function to be minimized
     */
    public SimplexCoreOptimizer(ICostFunction f){
        this.f = f;
    }

	/**
	 * Constructor for Simplex minimization
	 * @param pInit : initial parameters
	 * @param width : initial size of perturbations
	 */
	public void initialize(IVector pInit, double width){
		// generate initial perturbations
		int n = pInit.getSize();
		this.pCurrent = new IVector[n+1];
		this.fCurrent = new double[n+1];
		this.predCurrent = new IVector[n+1];
		this.pCurrent[0] = pInit.clone();
		this.fCurrent[0] = this.f.evaluate(pInit,"initialization");
		if(this.f instanceof LeastSquaresCostFunction){
			this.predCurrent[0]=((LeastSquaresCostFunction)this.f).getLastPredictions();
		}
		for(int i=0;i<n;i++){
			IVector p = pInit.clone();
			p.setValue(i, p.getValue(i)+width);
			double fVal = this.f.evaluate(p,"initialization");
			this.pCurrent[i+1] = p;
			this.fCurrent[i+1] = fVal;
			if(this.f instanceof LeastSquaresCostFunction){
				this.predCurrent[i+1]=((LeastSquaresCostFunction)this.f).getLastPredictions();
			}

		}
		sortSimplexByCost();
	}

	/**
	 * Initializer for setting the current simplex, e.g. from a restart file
	 * @param pCurrent : Parameters to be restored, for each point of the simplex
	 */
	public void initialize(IVector pCurrent[]){
        this.pCurrent = new IVector[pCurrent.length];
        this.fCurrent = new double[pCurrent.length];
		this.predCurrent = new IVector[pCurrent.length];
        for(int i=0;i<pCurrent.length;i++){
        	this.pCurrent[i] = pCurrent[i].clone();
        	this.fCurrent[i] = this.f.evaluate(this.pCurrent[i],"initialization");
			if(this.f instanceof LeastSquaresCostFunction){
				this.predCurrent[i]=((LeastSquaresCostFunction)this.f).getLastPredictions();
			}

        }
		sortSimplexByCost();
	}

    /**
     * Initializer for setting costs corresponding to each node of the current simplex.
     * These costValues are likely to be read from an earlier experiment
     * @param pars : parameters as Vector at each node of simplex
     * @param costValues : correspondig cost costValues
     */
    public void initialize(IVector pars[], double costValues[]){
        this.pCurrent = new IVector[pars.length];
        this.fCurrent = new double[pars.length];
        this.predCurrent = new IVector[pCurrent.length];
        for(int i=0;i<pars.length;i++){
            this.pCurrent[i] = pars[i].clone();
            this.fCurrent[i] = costValues[i];
            this.predCurrent[i] = null; //impossible to reconstruct predictions here
        }
        sortSimplexByCost();
    }

	private void sortSimplexByCost(){
        // Sort simplex by cost
		this.nparam = this.pCurrent[0].getSize(); //number of parameters to optimize
		double[] costs=this.fCurrent;
		int[] iSort=this.sortedIndex(costs);
        costs = this.applyIndexToDoubles(costs, iSort);
		double diff=costs[nparam]-costs[0];
	    this.moreToDo = (imain<this.maxitSimplex) & (diff>this.absTolSimplex) & ((diff)>this.relTolSimplex*Math.abs(costs[0]));
	}

	/**
	 * Get the optimal parameters, i.e. the result of the minimization
	 * @return parameters as Vector
	 */
	public IVector getOptimalValue(){
        return this.f.getOptimalParameters();
	}

	/**
	 * Get the cost value at the optimum
	 * @return cost
	 */
	public double getOptimalCost(){
        return this.f.getOptimalCost();
	}

	/**
	 * Get parameters for each point of the simplex. The current simplex equals the initial simplex
	 * if requested before optimization and final simplex if f.optimize() has been called.
	 * @return Vector for parameters at each nod of the simplex (m+1) vectors, where m=length(p)
	 */
	public IVector[] getCurrentValues(){
		IVector result[] = new IVector[this.pCurrent.length];
		for(int i=0;i<this.pCurrent.length;i++){
			result[i] = this.pCurrent[i].clone();
		}
		return result;
	}

	/**
	 * Get the current cost values corresponding to the nodes of the current simplex.
	 * @return Cost at each node as array of doubles
	 */
	public double[] getCurrentCosts(){
        return this.fCurrent.clone();
	}

	/**
	 * Main optimization routine
	 */
	public void optimize(){
		while(this.hasNext()){
			this.next();
		}
 	}
	
	/**
	 * Are there any more steps for this algorithm
	 * @return has next step
	 */
	boolean hasNext(){
        return this.moreToDo;
	}
	
	/**
	 * Run next step of the algorithm
	 */
	void next(){
		//Declare
		IVector sreflect;     // parameters for scaling
		IVector scontract;    // parameters for contraction
		IVector sexpand;      // parameters for expansion
		IVector scentroid;    // parameter for "middle" of simplex
		IVector sshrink;      // parameter for shrinking
		double cost_e;       // cost for expansion
		double cost_c;       // cost for contraction
		double cost_r;       // cost for reflection
		double cost_s;       // cost for scaling
		double costmax;
		IVector predReflect=null;     // predictions for scaling (in case of a LeastSquaresCostFunction)
		IVector predContract=null;    // predictions for contraction (in case of a LeastSquaresCostFunction)
		IVector predExpand=null;      // predictions for expansion (in case of a LeastSquaresCostFunction)
		IVector predShrink=null;      // predictions for shrinking (in case of a LeastSquaresCostFunction)

		int[] iSort;        // index after sorting
  		double[] cost_sort; //costs after sorting
  		IVector[] sparam_sort; //parameters after sorting
  		IVector[] pred_sort; //predictions after sorting

		// abbreviations for often used variables
		IVector[] sparam = this.pCurrent;     //nparam+1 parametervectors in simplex
		double[] costs = this.fCurrent;       //cost for each vector in simplex
		IVector[] preds = this.predCurrent;    // predictions (in case of a LeastSquaresCostFunction)

        // Main loopIter
//		int imain=0;
        int flagstep;
        Results.putValue("costs", new Vector(costs), costs.length, "any", IResultWriter.OutputLevel.Essential , IResultWriter.MessageType.OuterIteration);
		if(this.moreToDo){
		      flagstep=0;           //flag if an action step (reflect,expand,contract,or shrink) has been performed
		                            // = 0 : no step has been performed
		                            // = 1 : a step has been performed
		      imain=imain+1;
		      Results.putMessage("Iteration step no."+ imain);
		      // find centriod of better vertices:
		      scentroid = vectorSum(sparam);
		      scentroid.scale(1.0/nparam);

		      //---   REFLECT:
		      //MVL sreflect   = (1-rconst)*scentroid+rconst*sparam(:,nparam);
		      //sreflect   = (1-rconst)*scentroid-((1-rconst)/nparam-rconst)*sparam[nparam];
		      sreflect   = scentroid.clone();
		      sreflect.scale(1-rconst);
		      sreflect.axpy(-((1-rconst)/nparam-rconst), sparam[nparam]);
		      cost_r     = this.f.evaluate(sreflect,"reflect");
				if(this.f instanceof LeastSquaresCostFunction){
					predReflect=((LeastSquaresCostFunction)this.f).getLastPredictions();
				}


		      //% check if cost_r is between the best and 2nd-worst vertices:
		      if ((cost_r>=costs[0]) & (cost_r<costs[nparam-1])){
		        flagstep           = 1;
		        sparam[nparam]     = sreflect;
		        costs[nparam]      = cost_r;
		        preds[nparam]      = predReflect;
		        Results.putMessage("REFLECT!");
		      }

		      //---   EXPAND:
		      if ((flagstep==0) & (cost_r<costs[0])){
		         //MVL sexpand          = (1-rconst*econst)*scentroid-rconst*econst*sparam(:,nparam+1);
 		         //sexpand   = (1-rconst*econst)*scentroid-((1-rconst*econst)/nparam-rconst*econst)*sparam[nparam+1];
			     sexpand   = scentroid.clone();
			     sexpand.scale(1-rconst*econst);
			     sexpand.axpy(-((1-rconst*econst)/nparam-rconst*econst), sparam[nparam]);
		         cost_e    = this.f.evaluate(sexpand,"expand");
		         if(this.f instanceof LeastSquaresCostFunction){
		        	 predExpand=((LeastSquaresCostFunction)this.f).getLastPredictions();
		         }

		         if (cost_e<cost_r){
		            flagstep           = 1;
		            sparam[nparam]     = sexpand;
		            costs[nparam]      = cost_e;
		            preds[nparam]      = predExpand;
                    Results.putMessage("EXPAND!");
		         }
		         if (cost_e>=cost_r){
		            flagstep           = 1;
		            sparam[nparam]     = sreflect;
		            costs[nparam]      = cost_r;
		            preds[nparam]      = predReflect;
                    Results.putMessage("REFLECT ALSO!");
		         }
		      }

		      //---   CONTRACT:
		      if ((flagstep==0) & (cost_r>=costs[nparam-1]) & (cost_r<costs[nparam])){
		        // outside contraction
		        //MVL scontract        = (1+rconst*cconst)*scentroid-rconst*cconst*sparam(:,nparam+1);
		        //scontract   = (1-rconst*cconst)*scentroid-((1-rconst*cconst)/nparam-rconst*cconst)*sparam[nparam+1];
			    scontract   = scentroid.clone();
			    scontract.scale(1-rconst*cconst);
			    scontract.axpy(-((1-rconst*cconst)/nparam-rconst*cconst), sparam[nparam]);
		        cost_c      = this.f.evaluate(scontract,"contract outside");
				if(this.f instanceof LeastSquaresCostFunction){
					predContract=((LeastSquaresCostFunction)this.f).getLastPredictions();
				}

		        if (cost_c<=cost_r){
		           flagstep           = 1;
		           sparam[nparam]     = scontract;
		           costs[nparam]      = cost_c;
		           preds[nparam]      = predContract;
                   Results.putMessage("CONTRACT OUTSIDE!");
		        }
		      }
		      if ((flagstep==0) & (cost_r>=costs[nparam])){
		        // inside contraction
		        //MVL scontract        = (1-cconst)*scentroid+cconst*sparam(:,nparam+1);
		        //scontract   = (1-cconst)*scentroid-((1-cconst)/nparam-cconst)*sparam[nparam+1];
			    scontract   = scentroid.clone();
			    scontract.scale(1-cconst);
			    scontract.axpy(-((1-cconst)/nparam-cconst), sparam[nparam]);
		        cost_c           = this.f.evaluate(scontract,"contract inside");
				if(this.f instanceof LeastSquaresCostFunction){
					predContract=((LeastSquaresCostFunction)this.f).getLastPredictions();
				}
		        if (cost_c<costs[nparam]){
		           flagstep         = 1;
		           sparam[nparam]   = scontract;
		           costs[nparam]    = cost_c;
		           preds[nparam]    = predContract;
                   Results.putMessage("CONTRACT INSIDE!");
		        }
		      }

		      //--- Check acceptance:
		      if (flagstep==1){
		          // Sort simplex by cost
		  		  iSort=this.sortedIndex(costs);
		  		  cost_sort = this.applyIndexToDoubles(costs, iSort);
		  		  sparam_sort=this.applyIndexToVectors(sparam, iSort);
		  		  pred_sort=this.applyIndexToVectors(preds, iSort);
		  		  sparam = sparam_sort;
		  		  costs  = cost_sort;
		  		  preds  = pred_sort;
		      }

		      //---   SHRINK:
		      if (flagstep==0){
		        costmax=costs[nparam];
		        for(int ivertex=1;ivertex<nparam+1;ivertex++){
		          //sshrink            = (1-sconst)*sparam[0]+sconst*sparam[ivertex];
			      sshrink   = sparam[0].clone();
			      sshrink.scale(1-sconst);
			      sshrink.axpy(sconst, sparam[ivertex]);
		          cost_s             = this.f.evaluate(sshrink,"shrink");
		          if(this.f instanceof LeastSquaresCostFunction){
		        	  predShrink=((LeastSquaresCostFunction)this.f).getLastPredictions();
		          }
		          sparam[ivertex]    = sshrink;
		          costs[ivertex]     = cost_s;
		          preds[ivertex]     = predShrink;
		        }
		        // Sort simplex by cost
				iSort=this.sortedIndex(costs);
				cost_sort = this.applyIndexToDoubles(costs, iSort);
				sparam_sort=this.applyIndexToVectors(sparam, iSort);
				pred_sort=this.applyIndexToVectors(preds, iSort);
				sparam = sparam_sort;
				costs  = cost_sort;
				preds  = pred_sort;
                Results.putMessage("SHRINK");
		        // Check if "SHRINK" produces better sets of parameter
		        if (costmax<costs[nparam]){
                   Results.putValue("maximumCost", costs[nparam], 1, "any", IResultWriter.OutputLevel.Essential , IResultWriter.MessageType.OuterIteration);
		           Results.putMessage("WARNING: SHRINK produces worse parameter set");
                }
		      }
		      // Compute new best-worst difference:
		      double diff=(costs[nparam]-costs[0]);
              double relDiff = diff/Math.abs(costs[0]);
              Results.putMessage("costs"+ new Vector(costs)+ "costs in simplex");
              this.moreToDo = (imain<this.maxitSimplex) & (diff>this.absTolSimplex) & ((diff)>this.relTolSimplex*Math.abs(costs[0]));
            Results.putMessage("stop criterion 1, imain > maxit:\t "+imain+" < "+Math.round(this.maxitSimplex));
            Results.putMessage("stop criterion 2, diff < abstol:\t "+diff+" > "+this.absTolSimplex);
            Results.putMessage("stop criterion 3, relDiff < reltol:\t "+relDiff+" > "+this.relTolSimplex);

		} // iteration loop

        // additional stop criteria:
        if (stopCriteria.size()>0) {
            // To do: generalize for non leastsquarecostfunction too!
            IVector residual = null;
            if(this.f instanceof LeastSquaresCostFunction){
                residual = ((LeastSquaresCostFunction)f).getObservationUncertainty().getExpectations();
            } else {
                throw new RuntimeException("Additional stop criterion is only implemented for LeastSquaresCostFunction..");
            }
            residual.axpy(-1.0,preds[0]);
            boolean isStop = false;
            for (int i=0; i<stopCriteria.size(); i++){
                IStopCriterion object = stopCriteria.get(i);
                double threshold = stopCriteriaThreshold.get(i);
                if (obsDescr!=null) {
                    isStop = object.checkForStop(sparam[0],residual,obsDescr,costs[0],threshold);
                } else {
                    isStop = object.checkForStop(sparam[0],residual,costs[0],threshold);
                }
                Results.putMessage(object.toString());
                this.moreToDo = this.moreToDo & !isStop;
            }
        }

		if(!this.moreToDo){
			//Display final results:
			Results.putMessage("costs"+ new Vector(costs)+ "costs in simplex");
			if((costs[nparam]-costs[0])<this.absTolSimplex){
				Results.putMessage("Convergence on absolute error max(cost)-min(cost)="+(costs[nparam]-costs[0])+"<"+this.absTolSimplex);
			}
			if(((costs[nparam]-costs[0])/costs[0])<this.relTolSimplex){
				Results.putMessage("Convergence on relative error (max(cost)-min(cost))/abs(min(cost))="+((costs[nparam]-costs[0])/Math.abs(costs[0]))+"<"+this.absTolSimplex);
			}
			f.writeResults();
			//estimate uncertainty for parameters //TODO
			if(this.f instanceof LeastSquaresCostFunction){
				for(int i=0;i<=this.nparam;i++){
					if(preds[i]==null){
						Results.putMessage("SimplexCoreOptimizer: uncertainty extimates not possible due to missing predictions");
					}
				}
				// compute error estimate
				// compute analysis error covariance from approximate Hessian
				// Pest = Xi*inv(A' * A)*Xi'
				if(1==1){ //TODO replace with check for computation fo error estimate
					// build least squares problem
					// solve a linear least-squares problem for next estimate
					IVector sigmaObs = ((LeastSquaresCostFunction)f).getObservationUncertainty().getStandardDeviations();
					IVector[] predsNorm = new IVector[nparam];
					IVector[] parsRel  = new IVector[nparam];
					for(int i=0;i<nparam;i++){ //compute normalized predictions (relative to best so far and scaled with obs errors)
						// Y = [y_1-y_0, ... ,y_n-y_0]
						predsNorm[i] = preds[i+1].clone();
						predsNorm[i].axpy(-1.0,preds[0]);predsNorm[i].pointwiseDivide(sigmaObs);
						parsRel[i] = sparam[i+1].clone();
						parsRel[i].axpy(-1.0,sparam[0]);  // ([pars[i+1] -pars[0]); sorted w.r.t. cost!
					}
					Matrix A_obs = new Matrix(predsNorm);
					// additional term for background errors
					Matrix A;
                    IMatrix LPar = ((LeastSquaresCostFunction)this.f).getParameterUncertainty().getSqrtCovariance().asMatrix();
					if(((LeastSquaresCostFunction)this.f).doAddBackgroundTerm()){
						IVector[] parsNorm  = new IVector[nparam];
						for(int i=0;i<nparam;i++){ //compute normalized predictions (relative to best so far and scaled with obs errors)
							parsNorm[i] = parsRel[i].clone();
							LPar.rightSolve(parsRel[i],parsNorm[i]);
						}
						Matrix A_b = new Matrix(parsNorm);
						A = Matrix.concatVertical(A_obs,A_b);
					}else{
						A = A_obs;
					}
					int np            = A.getNumberOfColumns();
					Matrix Pinv = new Matrix(np,np);
					// Pinv = A' * A
					Pinv.multiply(1.0, A, A, 0.0, true, false);
					// P = inv(Pinv)
					Matrix P = Pinv.inverse();
					// Tranform to parameter space Xi*P*Xi'
					Matrix Xi = new Matrix(parsRel);
					Matrix Pest = new Matrix(np,np);
					Pest.multiply(1.0, P, Xi, 0.0, false, true);
					Pest = Matrix.mult(Xi, Pest);
					// standard deviations
					Vector std = (Vector) Pest.diag();
					std.sqrt();
					// now make this pretty for treeVectors
					IVector stdWithStructure = this.pCurrent[0].clone();
					stdWithStructure.setValues(std.getValues());
					Results.putMessage("Error estimate for this outer iteration");
                    Results.putValue(	"parameterErrorEstimateStd", stdWithStructure, stdWithStructure.getSize(),
                                        "outer iteration "+imain, IResultWriter.OutputLevel.Verbose,
                                        IResultWriter.MessageType.OuterIteration);
					/*
					 *  compute correlations
					 */
					Vector stdInverse = std.clone();
					stdInverse.invert();
					IMatrix stdOnDiag = Matrix.diag(stdInverse);
					IMatrix correlations = Matrix.mult(stdOnDiag, Matrix.mult(Pest,stdOnDiag));
                    int correlationsSize = correlations.getNumberOfColumns() * correlations.getNumberOfRows();
                    Results.putValue("parameterErrorCorrelations", correlations, correlationsSize,
                            "outer iteration "+imain, IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.OuterIteration);
				}

			}else{
				Results.putMessage("SimplexCoreOptimizer: uncertainty extimates are only possible for a LeastSquaresCostFunction");
			}
		}
		this.fCurrent = costs;
		this.pCurrent = sparam;
		this.predCurrent = preds;
	}

	
	//============================================================================================
	// only supporting routines below this line

	/**
	 * Sort an array and return the result as an array of indices
	 * @param values : to be sorted
	 * @return indices : first value points to smallest value and last one to largest
	 */
	private int[] sortedIndex(double[] values){

		class indexValuePair
		{
		  int index;
		  double value;
		  public indexValuePair(int index, double value)
		  {
		    this.index = index;
		    this.value = value;
		  }
		}
		class ValueComparator implements java.util.Comparator
		{
		  public int compare(Object o1, Object o2)
		  {
		    return new Double(((indexValuePair)o1).value).compareTo(((indexValuePair) o2).value);
		  }
		}

		int[] result = new int[values.length];
		java.util.ArrayList<indexValuePair> sortedValues = new java.util.ArrayList<indexValuePair>();
		for(int i=0;i<values.length;i++){
		   sortedValues.add(new indexValuePair(i,values[i]));
		}
		ValueComparator vc = new ValueComparator();
		java.util.Collections.sort(sortedValues, vc);
		int j=0;
		for(java.util.Iterator it=sortedValues.iterator();it.hasNext();){
			indexValuePair ivp = (indexValuePair) it.next();
			result[j]=ivp.index;
			j++;
		}
		return result;
	}

	/**
	 * Use indices from sorting to sort an additions array of Vectors
	 * Vectors are not copied, only the references are switched
	 * @param vs vectors to be sorted
	 * @param index indices from sorting
	 * @return sorted vectors
	 */
	private IVector[] applyIndexToVectors(IVector[] vs,int[] index){
		IVector[] result = new IVector[vs.length];
		for(int i=0;i<vs.length;i++){
			result[i] = vs[index[i]];
		}
		return result;
	}

	/**
	 * Use indices from sorting to sort an additions array of doubles
	 * Sorts on a copy.
	 * @param cs doublesto be sorted
	 * @param index indices from sorting
	 * @return sorted doubles
	 */
	private double[] applyIndexToDoubles(double[] cs,int[] index){
		double[] result = new double[cs.length];
		for(int i=0;i<cs.length;i++){
			result[i] = cs[index[i]];
		}
		return result;
	}

	/**
	 * Sum the elements of an array of Vectors
	 * @param vs input vectors
	 * @return sum
	 */
	private IVector vectorSum(IVector[] vs){
		IVector result = vs[0].clone();
		for(int i=1;i<vs.length;i++){
			result.axpy(1.0, vs[i]);
		}
		return result;
	}

}
