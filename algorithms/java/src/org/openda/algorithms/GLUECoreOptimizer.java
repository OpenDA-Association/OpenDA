/* OpenDA v2.4.1 
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
import org.openda.interfaces.IStochVector;
import org.openda.interfaces.IVector;

import java.util.ArrayList;
import java.util.List;

public class GLUECoreOptimizer {

	// fields of this class
	IVector pCurrent = null; //values under consideration
    IVector pInit = null;
	double fCurrent = 0.0;
	int nparam=0;
	RMSECostFunction f = null;
    int[] typeParameterUncertainty;

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
    IStochVector parameterUncertainty;
//    UncertaintyStochVector parameterUncertainty;
    private int nRuns;

    /**
     * Constructor for GLUE
     * @param f : costfunction to be minimized
     * @param nRuns : number of simulation runs
     */
    public GLUECoreOptimizer(RMSECostFunction f, int nRuns){
        this.f = f;
        this.pInit = this.parameterUncertainty.getExpectations();
        this.nRuns = nRuns;
        this.nparam = this.parameterUncertainty.getExpectations().getSize();
    }

	/**
	 * Constructor for GriddedFullSearch
	 * @param f : costfunction to be minimized
	 * @param minRangePar : minimum of range to scan for each parameter
	 * @param maxRangePar : maximum of range
	 * @param stepRangePar: step size for each parameter
	 */
//	public GLUECoreOptimizer(RMSECostFunction f, IVector minRangePar, IVector maxRangePar, IVector stepRangePar){
//		this.f = f;
//        this.setGrid(minRangePar, maxRangePar, stepRangePar);
//	}

//	/**
//	 * Set range and step size for the grid
//	 * @param minRange minimum of range for each parameter
//	 * @param maxRange maximum of range for each parameter
//	 * @param stepRange step size for grid for each parameter
//	 */
//	public void setGrid(IVector minRange, IVector maxRange, IVector stepRange){
//		int n = minRange.getSize();
//		//some consistency checks
//		if(minRange.getSize()!=maxRange.getSize()){
//			throw new RuntimeException("Arrays with minimum of range and maximum of range have unequal length");
//		}
//		if(minRange.getSize()!=stepRange.getSize()){
//			throw new RuntimeException("Arrays with minimum of range and step of range have unequal length");
//		}
//		// add grid settings
//		this.nparam=n;
//		this.minRangePar = minRange.getValues();
//		this.maxRangePar = maxRange.getValues();
//		this.stepRangePar = stepRange.getValues();
//		this.numberOfStepsPar = new int[n];
//		this.currentParIndex = new int[n];
//		for(int i=0;i<n;i++){
//			this.numberOfStepsPar[i] = (int) Math.floor((this.maxRangePar[i]-this.minRangePar[i])/this.stepRangePar[i]);
//			this.currentParIndex[i]  = 0;
//		}
//		this.currentParIndex[0]=-1;
//		// generate initial settings
//		this.pCurrent = minRange;
//		this.fCurrent = Double.POSITIVE_INFINITY;
//	}
	

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
		int n=this.nparam;
        this.pCurrent = this.pInit.clone();
		IVector realization = this.parameterUncertainty.createRealization();
		for(int i=0;i<n;i++){
			this.pCurrent.setValue(i, realization.getValue(i));
		}

		//evaluate
		this.fCurrent = this.f.evaluate(this.pCurrent,"any");

        // check if all points have been tested:
        this.moreToDo = false;
        if (imain < nRuns){
            this.moreToDo = true;
        }

		if(!this.moreToDo){
			//Display final results:
			f.writeResults();
		}
	}

}
