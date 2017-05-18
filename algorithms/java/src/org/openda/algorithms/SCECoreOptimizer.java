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


import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.IResultWriter;
import org.openda.interfaces.IVector;
import org.openda.utils.Results;
import org.openda.utils.Vector;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

public class SCECoreOptimizer {

	// fields of this class
	IVector pCurrent[] = null; //values under consideration
	double fCurrent[] = null;
	IVector predCurrent[] = null; //predictions for each estimate (in case of LeastSquaresCostFunction)
	int nparam=0;
	ICostFunction f = null;
	int maxitSCE = 100; // Maximum number of outer loop iterations.
    int numInnerLoop = 30; // beta: Number of evolution steps taken by each complex. Suggested optimum: beta=2n+1
	double absTolSCE =0.01;
	double relTolSCE =0.01;
    IVector LB = null; // vector of parameter range lower bounds
    IVector UB = null; // vector of parameter range upper bounds
    int nComplexes = 5; // p: Number of complexes. Suggested optima: n=2,4:p=1; n=6,8:p=2; n=10,12:p=4
    public int nPointsComplex; // m: Number of points in a complex. Suggested optimum: 2n+1
    public int nPointsSimplex; // q: Number of points in a subcomplex. Suggested optimum: q=n+1
    public IVector pInit;
    public int maxCostEvaluation = 1000; // Maximum number of cost function evaluations.
    private Random randomGenerator = new Random();

    //stopping
    boolean moreToDo = true;         // is this optimization finished
    int imain=0;                     // main iterations done
    private int nCostEvaluation = 0; // number of cost evaluation so far

    // required for the additional stopping criteria:
    public List<IStopCriterion> stopCriteria = new ArrayList<IStopCriterion>();
    public List<Double> stopCriteriaThreshold = new ArrayList<Double>();
    public double stopCritThresDefault = 0.01;

    public IObservationDescriptions obsDescr=null;


    /**
     * Constructor for SCE minimization
     * @param f : cost function to be minimized
     */
    public SCECoreOptimizer(ICostFunction f){
        this.f = f;
    }

	/**
	 * Constructor for SCE minimization
	 * @param pInit : initial parameters
	 * @param width : initial size of perturbations
	 */
	public void initialize(IVector pInit, double width){
		// generate initial perturbations
		int n = pInit.getSize();
		this.pCurrent = new IVector[n+1];
		this.fCurrent = new double[n+1];
		this.predCurrent = new IVector[n+1];
        this.pInit = pInit.clone();
		this.pCurrent[0] = pInit.clone();
//		this.fCurrent[0] = this.f.evaluate(pInit);
//        nCostEvaluation++;
        this.fCurrent[0] = calculatecost(this.f, pInit,this.LB,this.UB);
		if(this.f instanceof LeastSquaresCostFunction){
			this.predCurrent[0]=((LeastSquaresCostFunction)this.f).getLastPredictions();
		}
		for(int i=0;i<n;i++){
			IVector p = pInit.clone();
			p.setValue(i, p.getValue(i)+width);
//			double fVal = this.f.evaluate(p);
//            nCostEvaluation++;
            double fVal = calculatecost(this.f, p,this.LB,this.UB);
			this.pCurrent[i+1] = p;
			this.fCurrent[i+1] = fVal;
			if(this.f instanceof LeastSquaresCostFunction){
				this.predCurrent[i+1]=((LeastSquaresCostFunction)this.f).getLastPredictions();
			}

		}
		sortPopulationByCost();
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
            int thisNCostEvaluation = nCostEvaluation;
            this.fCurrent[i] = calculatecost(this.f,this.pCurrent[i],this.LB,this.UB);
			if(this.f instanceof LeastSquaresCostFunction){
                if (thisNCostEvaluation>nCostEvaluation){
                    // last prediction is available only if the costFunction is evaluated. When parameters are
                    // outside the specified range, the costFunction is not evaluated; see calculatecost().
				    this.predCurrent[i]=((LeastSquaresCostFunction)this.f).getLastPredictions();
                }
			}

        }
		sortPopulationByCost();
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
        sortPopulationByCost();
    }

	private void sortPopulationByCost(){
        int[] iSort = this.sortedIndex(this.fCurrent);
        this.fCurrent = this.applyIndexToDoubles(this.fCurrent,iSort);
        this.pCurrent = this.applyIndexToVectors(this.pCurrent,iSort);
        double diff=this.fCurrent[nparam]-this.fCurrent[0]; // Cost of worst member - Cost of best member.
	    this.moreToDo = (imain<this.maxitSCE) & (diff>this.absTolSCE) & ((diff)>this.relTolSCE *Math.abs(this.fCurrent[0]))
                        & (nCostEvaluation<=maxCostEvaluation);
	}

    private void sortComplexByCost(IVector[] pars, double[] costValues){
        int[] iSort = this.sortedIndex(costValues);
        costValues = this.applyIndexToDoubles(costValues,iSort);
        pars = this.applyIndexToVectors(pars,iSort);
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
//		//Declare
//		IVector sreflect;     // parameters for scaling
//		IVector scontract;    // parameters for contraction
//		IVector sexpand;      // parameters for expansion
//		IVector scentroid;    // parameter for "middle" of simplex
//		IVector sshrink;      // parameter for shrinking
//		double cost_e;       // cost for expansion
//		double cost_c;       // cost for contraction
//		double cost_r;       // cost for reflection
//		double cost_s;       // cost for scaling
//		double costmax;
//		IVector predReflect=null;     // predictions for scaling (in case of a LeastSquaresCostFunction)
//		IVector predContract=null;    // predictions for contraction (in case of a LeastSquaresCostFunction)
//		IVector predExpand=null;      // predictions for expansion (in case of a LeastSquaresCostFunction)
//		IVector predShrink=null;      // predictions for shrinking (in case of a LeastSquaresCostFunction)
//
//		int[] iSort;        // index after sorting
//  		double[] cost_sort; //costs after sorting
//  		IVector[] sparam_sort; //parameters after sorting
//  		IVector[] pred_sort; //predictions after sorting

		// abbreviations for often used variables
		IVector[] sparam = this.pCurrent;     //nparam+1 parametervectors in simplex
		double[] costs = this.fCurrent;       //cost for each vector in simplex
		IVector[] preds = this.predCurrent;    // predictions (in case of a LeastSquaresCostFunction)

        // Main loopIter
        Results.putValue("costs", new Vector(costs), costs.length, "any", IResultWriter.OutputLevel.Essential , IResultWriter.MessageType.OuterIteration);
		if(this.moreToDo){

   	        imain=imain+1;
		    Results.putMessage("Outer iteration no."+ imain);

            for (int j=0; j<this.nComplexes; j++){
                // Construct j-th complex from the population
                IVector[] pComplex = new IVector[this.nPointsComplex];
                double[] fComplex = new double[this.nPointsComplex];
                int[] k1 = new int[this.nPointsComplex];
                int[] k2 = new int[this.nPointsComplex];
                for (int i=0; i<this.nPointsComplex; i++){
                    k1[i] = i;
                    k2[i] = i * this.nComplexes + j;
                    pComplex[k1[i]] = this.pCurrent[k2[i]].clone();
                    fComplex[k1[i]] = this.fCurrent[k2[i]];
                }

                for (int iCCE = 0; iCCE < this.numInnerLoop; iCCE++){
                    // Select simplex by sampling the complex
                    int[] location = new int[nPointsSimplex];
                    location[0] = 0;
                    for (int i=1; i<nPointsSimplex; i++){
                        location[i] = 9999999;
                    }
                    for (int i=1; i<nPointsSimplex; i++){
                        boolean alreadyParent = false;
                        int dummy = 9999;
                        while (!alreadyParent){
                            double r = randomGenerator.nextDouble();
                            dummy = (int) (Math.floor(nPointsComplex+0.5-Math.sqrt(Math.pow(nPointsComplex+0.5,2)-nPointsComplex*(nPointsComplex+1)*r)));
                            if (Arrays.binarySearch(location,dummy)<0){
                                alreadyParent = true;
                            }
                        }
                        location[i] = dummy;
                    }
                    Arrays.sort(location);

                    // construct the simplex
                    IVector[] pSimplex = new IVector[this.nPointsSimplex];
                    double[] fSimplex = new double[this.nPointsSimplex];
                    for (int i=0; i<nPointsSimplex; i++){
                        pSimplex[i] = pComplex[location[i]].clone();
                        fSimplex[i] = fComplex[location[i]];
                    }

                    //// generate new point for simplex (propagate simplex)
                    double fac = -1;
                    IVector paramTry = amotry(pSimplex,fac);
                    double costTry = calculatecost(this.f, paramTry,LB,UB);
                    double eps = 1E-10;
                    // check the cost
                    if (costTry <= fSimplex[0] + eps) {
                        // if the cost is smaller than the smallest vertex, try expansion:
                        fac = -2;
                        IVector paramTry2 = amotry(pSimplex,fac);
                        double costTry2 = calculatecost(this.f, paramTry2,LB,UB);
                        // check the cost
                        if (costTry2 < costTry) {
                            // if expansion is better, use it to replace the worst vertex
                            pSimplex[pSimplex.length-1] = paramTry2.clone();
                            fSimplex[pSimplex.length-1] = costTry2;
                            Results.putMessage("REFLECTION AND EXPANSION");
                        } else {
                            // otherwise, use the relection to replace the worst vertex
                            pSimplex[pSimplex.length-1] = paramTry.clone();
                            fSimplex[pSimplex.length-1] = costTry;
                            Results.putMessage("REFLECTION");
                        }
                    } else if (costTry > fSimplex[fSimplex.length-2]) {
                        // if the cost is larger than the second worst vertex, try one-dimensional contraction
                        fac = -0.5;
                        IVector paramTry3 = amotry(pSimplex,fac);
                        double costTry3 = calculatecost(this.f, paramTry3,LB,UB);
                        if (costTry3 < fSimplex[fSimplex.length-1]){
                            // if 1D contraction is better, use it to replace the worst vertex
                            pSimplex[pSimplex.length-1] = paramTry3.clone();
                            fSimplex[pSimplex.length-1] = costTry3;
                            Results.putMessage("ONE DIMENSIONAL CONTRACTION");
                        } else {
							// can't seem to get rid of the worst vertex, try contraction around the best vertices
							for (int i = 1; i < pSimplex.length - 1; i++) {
								pSimplex[i].axpy(1.0, pSimplex[0]);
								pSimplex[i].scale(0.5);
								fSimplex[i] = calculatecost(this.f, pSimplex[i], UB, LB);
							}
							Results.putMessage("MULTIPLE CONTRACTION");
                        }
                    } else {
                        // if the cost is smaller than the 2nd worst vertex, use this to replace the worst vertex
                        pSimplex[pSimplex.length-1] = paramTry.clone();
                        fSimplex[fSimplex.length-1] = costTry;
                        Results.putMessage("REFLECTION");
                    }

                    // replace the simplex into the complex
                    for (int i=0; i<nPointsSimplex; i++){
                        pComplex[location[i]].setValues(pSimplex[i].getValues());
                        fComplex[location[i]] = fSimplex[i];
                    }

                    // sort the complex
                    sortComplexByCost(pComplex, fComplex);

                } // reselection and propagation of simplex

                // Replace the j-th complex back into the population
                for (int i=0; i<this.nPointsComplex; i++){
                    this.pCurrent[k2[i]].setValues(pComplex[k1[i]].getValues());
                    this.fCurrent[k2[i]] = fComplex[k1[i]];
                }

            } // inner loop / complex selection and propagation (CCE)

            // Sort new population and check stopping criteria
            sortPopulationByCost();

            // Compute new best-worst difference and put message on stopping criteria:
            double diff=(costs[nparam]-costs[0]);
            double relDiff = diff/costs[0];
            Results.putMessage("costs"+ new Vector(costs)+ "costs in simplex");
            Results.putMessage("stop criterion 1, imain > maxit:\t "+imain+" < "+Math.round(this.maxitSCE));
            Results.putMessage("stop criterion 2, diff < abstol:\t "+diff+" > "+this.absTolSCE);
            Results.putMessage("stop criterion 3, relDiff < reltol:\t "+relDiff+" > "+this.relTolSCE);
            Results.putMessage("stop criterion 4, nCostEvaluation > maxCostEvaluation:\t "+nCostEvaluation+" < "+maxCostEvaluation);

		} // outer loop / this.moreToDo

		if(!this.moreToDo){
			//Display final results:
			Results.putMessage("costs"+ new Vector(costs)+ "costs in simplex");
			if((costs[nparam]-costs[0])<this.absTolSCE){
				Results.putMessage("Convergence on absolute error max(cost)-min(cost)="+(costs[nparam]-costs[0])+"<"+this.absTolSCE);
			}
			else if(((costs[nparam]-costs[0])/costs[0])<this.relTolSCE){
				Results.putMessage("Convergence on relative error (max(cost)-min(cost))/abs(min(cost))="+((costs[nparam]-costs[0])/Math.abs(costs[0]))+"<"+this.absTolSCE);
			}
			else {
				Results.putMessage("No convergence occurred for the SCE algorithm.");
			}
			f.writeResults();
		}
	}


    //============================================================================================
	// only supporting routines below this line


    /**
     * Calculate cost function while taking the parameter range into cosideration.
     * When a parameter lies outside its range, a very large oost is given as output.
     * @param costFunction : costFunction to be evaluated
     * @param paramTry : parameter vector to be evaluated
     * @param LB : lower bound of parameter range
     * @param UB : upper bound of parameter range
     * @return cost : cost value
     */
    private double calculatecost(ICostFunction costFunction, IVector paramTry, IVector LB, IVector UB) {
        double costTry;
		nCostEvaluation++;
        for (int i=0; i<paramTry.getSize(); i++){
            // check lower bounds
			if (paramTry.getValue(i) < LB.getValue(i)){
				costTry = 1E12 + (LB.getValue(i)-paramTry.getValue(i)) * 1E6;
				return costTry;
			}
			// check upper bounds
			if (paramTry.getValue(i) > UB.getValue(i)){
				costTry = 1E12 + (paramTry.getValue(i)-UB.getValue(i)) * 1E6;
				return costTry;
			}
        }
        costTry = costFunction.evaluate(paramTry,"any");
        return costTry;
    }

    /**
     * Calculate the next point of simplex.
     * @param fac : extrapolation factor
     * @param pSimplex : vertices of the simplex used to find the next vertex
     * @return newVertex : new vertex of the simplex
     */
    private IVector amotry(IVector[] pSimplex, double fac) {
        int nVertices = pSimplex.length;
        IVector[] pBest = new IVector[nVertices-1];
        for (int i=0; i<nVertices-1; i++){
            pBest[i] = pSimplex[i].clone();
        }
        IVector pSum = vectorSum(pBest);
        pSum.scale(1.0 / ((double) pBest.length));
        pSum.scale(1.0 - fac);
        IVector pTry = pSimplex[nVertices-1].clone();
        pTry.scale(fac);
        pTry.axpy(1.0,pSum);
        return pTry;
    }

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
		ArrayList<indexValuePair> sortedValues = new ArrayList<indexValuePair>();
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
