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


import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.IVector;
import org.openda.utils.Results;

import java.util.ArrayList;
import java.util.List;

public class GriddedFullSearchCoreOptimizer{

	// fields of this class
	IVector pCurrent = null; //values under consideration
	double fCurrent = 0.0;
	int nparam=0;
	ICostFunction f = null;

    // required for the additional stopping criteria:
    public List<IStopCriterion> stopCriteria = new ArrayList<IStopCriterion>();
    public List<Double> stopCriteriaThreshold = new ArrayList<Double>();
    public double stopCritThresDefault = 0.01;
    public IObservationDescriptions obsDescr=null;

	// grid settings
	double minRangePar[]   = null;
	double maxRangePar[]   = null;
	double stepRangePar[]  = null;
	int numberOfStepsPar[] = null;

	//current state
	boolean moreToDo = true;         // is this optimization finished
    int currentParIndex[] =null;
    int imain=0; //number of evaluation
    private IVector pInit = null;


    /**
     * Constructor for GriddedFullSearch
     * @param f : costfunction to be minimized
     * @param minRangePar : minimum of range to scan for each parameter
     * @param maxRangePar : maximum of range
     * @param stepRangePar: step size for each parameter
     */
    public GriddedFullSearchCoreOptimizer(ICostFunction f, IVector minRangePar, IVector maxRangePar, IVector stepRangePar){
        this.f = f;
        this.setGrid(minRangePar, maxRangePar, stepRangePar);
    }

    /**
     * Constructor for GriddedFullSearch
     * @param f : costfunction to be minimized
     * @param minRangePar : minimum of range to scan for each parameter
     * @param maxRangePar : maximum of range
     * @param stepRangePar: step size for each parameter
     */
    public GriddedFullSearchCoreOptimizer(ICostFunction f, IVector minRangePar, IVector maxRangePar, IVector stepRangePar, IVector pInit){
        this.f = f;
        this.setGrid(minRangePar, maxRangePar, stepRangePar);
        this.pInit = pInit;
    }

	/**
	 * Set range and step size for the grid
	 * @param minRange minimum of range for each parameter
	 * @param maxRange maximum of range for each parameter
	 * @param stepRange step size for grid for each parameter
	 */
	public void setGrid(IVector minRange, IVector maxRange, IVector stepRange){
		int n = minRange.getSize();
		//some consistency checks
		if(minRange.getSize()!=maxRange.getSize()){
			throw new RuntimeException("Arrays with minimum of range and maximum of range have unequal length");
		}
		if(minRange.getSize()!=stepRange.getSize()){
			throw new RuntimeException("Arrays with minimum of range and step of range have unequal length");
		}
		// add grid settings
		this.nparam=n;
		this.minRangePar = minRange.getValues();
		this.maxRangePar = maxRange.getValues();
		this.stepRangePar = stepRange.getValues();
		this.numberOfStepsPar = new int[n];
		this.currentParIndex = new int[n];
		for(int i=0;i<n;i++){
			this.numberOfStepsPar[i] = (int) Math.floor((this.maxRangePar[i]-this.minRangePar[i])/this.stepRangePar[i]+1);
			this.currentParIndex[i]  = 0; 
		}
		this.currentParIndex[0]=-1;
		// generate initial settings
		this.pCurrent = minRange;
		this.fCurrent = Double.POSITIVE_INFINITY;
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
	 * Get last evaluated parameters
	 * @return Vector with parameters for last evaluation
	 */
	public IVector getCurrentValues(){
        return this.pCurrent.clone();
	}

	/**
	 * Get the cost for last evaluation.
	 * @return Cost value for last evaluation
	 */
	public double getCurrentCosts(){
		return this.fCurrent;
	}

	/**
	 * Set the current parameters, e.g. from a restart file
	 * @param pars Vector with last evaluated parameters
	 */
	public void setCurrentValues(IVector pars){
		this.pCurrent = pars.clone();
		this.fCurrent = 0.0;
		for(int i=0;i<pars.getSize();i++){
			this.currentParIndex[i] = (int) Math.floor((pars.getValue(i)-this.maxRangePar[i])/this.stepRangePar[i]);
		}
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
		imain++;
		// next parameters
        if (this.pInit == null){
            // do nothing or throw RuntimeException:
//            throw new RuntimeException("GriddedFullSearchCoreOptimizer: pInit is not set. GriddedFullSearchCoreOptimizer is implemented so far only for SimulationKwadraticCostFunction.");
        } else {
            this.pCurrent = pInit.clone();
        }
		this.currentParIndex = nextParIndex(this.currentParIndex,this.numberOfStepsPar);
		int n=this.nparam;
		for(int i=0;i<n;i++){
		    	this.pCurrent.setValue(i, this.minRangePar[i]+this.stepRangePar[i]*this.currentParIndex[i]);
		}
		//evaluate
		this.fCurrent = this.f.evaluate(this.pCurrent,"any");

        // check if all points have been tested:
        this.moreToDo = false;
        for(int i=0;i<this.nparam;i++){
            if(this.currentParIndex[i]<(this.numberOfStepsPar[i]-1)){
                this.moreToDo = true;
            }
        }

        // additional stop criteria:
        if (stopCriteria.size()>0) {
            IVector residual;
            if(this.f instanceof LeastSquaresCostFunction){
                residual = ((LeastSquaresCostFunction)f).getObservationUncertainty().getExpectations();
                residual.axpy(-1.0,((LeastSquaresCostFunction)f).getLastPredictions());
            } else {
                throw new RuntimeException("Additional stop criterion is only implemented for LeastSquaresCostFunction..");
            }
            boolean isStop = false;
            for (int i=0; i<stopCriteria.size(); i++){
                IStopCriterion object = stopCriteria.get(i);
                double threshold = stopCriteriaThreshold.get(i);
                if (obsDescr!=null) {
                    isStop = object.checkForStop(pCurrent,residual,obsDescr,fCurrent,threshold);
                } else {
                    isStop = object.checkForStop(pCurrent,residual,fCurrent,threshold);
                }
                Results.putMessage(object.toString());
                this.moreToDo = this.moreToDo & !isStop;
            }
        }

		if(!this.moreToDo){
			//Display final results:
			f.writeResults();
		}
	}


	//============================================================================================
	// only supporting routines below this line

    int[] nextParIndex(int[] currentParIndex,int[] maxParIndex){
    	int n=currentParIndex.length;
    	int[] result = new int[n];
    	System.arraycopy(currentParIndex, 0, result, 0, n);
    	result[0]++; //increment by one
        for(int i=0;i<(n-1);i++){ // fix overflow
        	if(result[i]>=maxParIndex[i]){
        		result[i]=0;
        		result[i+1]++;
        	}
        }
    	return result;
    }
	
}
