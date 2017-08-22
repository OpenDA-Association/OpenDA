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

/*
  DELSA algorithm, based on Rakovec et al (2014): Distributed evaluation of local sensitivity analysis (DELSA),
 * with application to hydrologic models, WRR, Vol.50,409-426.
 */

package org.openda.algorithms;


import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.IResultWriter;
import org.openda.interfaces.ISqrtCovariance;
import org.openda.interfaces.IVector;
import org.openda.utils.Matrix;
import org.openda.utils.PrintNumber;
import org.openda.utils.Results;
import org.openda.utils.Vector;
import org.apache.commons.math3.random.SobolSequenceGenerator;

import java.util.ArrayList;
import java.util.List;

public class DelsaCoreOptimizer{

	private SobolSequenceGenerator sobolSequence;
	// fields of this class
	IVector pCurrent = null; //values under consideration
	double fCurrent = 0.0;
	int nparam=0;
	ICostFunction f = null;
	IVector[] SL1 = null;

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
	IVector paramVariance;

	//current state
	boolean moreToDo = true;         // is this optimization finished
    int currentParIndex[] =null;
    int imain=0; //number of evaluation
    private IVector pInit = null;
	private boolean isSobol = false;


	/**
     * Constructor for Delsa
     * @param f : costfunction to be minimized
     * @param minRangePar : minimum of range to scan for each parameter
     * @param maxRangePar : maximum of range
     * @param stepRangePar: step size for each parameter
     */
    public DelsaCoreOptimizer(ICostFunction f, IVector minRangePar, IVector maxRangePar, IVector stepRangePar){
        this.f = f;
        this.setGrid(minRangePar, maxRangePar, stepRangePar);
    }

	/**
	 * Constructor for Delsa
	 * @param f : costfunction to be minimized
	 * @param minRangePar : minimum of range to scan for each parameter
	 * @param maxRangePar : maximum of range
	 * @param stepRangePar: step size for each parameter
	 */
	public DelsaCoreOptimizer(ICostFunction f, IVector minRangePar, IVector maxRangePar, IVector stepRangePar, IVector pInit){
		this.f = f;
		this.setGrid(minRangePar, maxRangePar, stepRangePar);
		this.pInit = pInit;
	}

	/**
	 * Constructor for Delsa
	 * @param f : costfunction to be minimized
	 * @param minRangePar : minimum of range to scan for each parameter
	 * @param maxRangePar : maximum of range
	 * @param nPoints: number of grid points for each parameter or number of sample
	 */
	public DelsaCoreOptimizer(ICostFunction f, IVector minRangePar, IVector maxRangePar, int nPoints, boolean isSobol, IVector pInit){
		this.f = f;
		if (isSobol){
			// add grid settings
			this.nparam=minRangePar.getSize();
			this.minRangePar = minRangePar.getValues();
			this.maxRangePar = maxRangePar.getValues();
			this.pCurrent = minRangePar;
			this.fCurrent = Double.POSITIVE_INFINITY;
			this.SL1 = new IVector[nPoints];
			this.isSobol = isSobol;
			sobolSequence = new SobolSequenceGenerator(this.nparam);

			// compute variances of the parameters (see Appendix B of Rakovec et al, 2014)
			this.paramVariance = new Vector(nparam);
			for (int iParam=0; iParam<this.nparam; iParam++){
				double thisVariance = (this.maxRangePar[iParam]-this.minRangePar[iParam])*(this.maxRangePar[iParam]-this.minRangePar[iParam])/12.0d;
				this.paramVariance.setValue(iParam,thisVariance);
			}

		} else {
			int nPointsPerParameter = nPoints;
			this.setGrid(minRangePar, maxRangePar, nPointsPerParameter);
		}
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

		this.paramVariance = new Vector(nparam);
		for (int iParam=0; iParam<this.nparam; iParam++){
			double thisVariance = (maxRangePar[iParam]-minRangePar[iParam])*(maxRangePar[iParam]-minRangePar[iParam])/12.0d;
			this.paramVariance.setValue(iParam,thisVariance);
		}
	}

	/**
	 * Set range and step size for the grid
	 * @param minRange minimum of range for each parameter
	 * @param maxRange maximum of range for each parameter
	 * @param nPointsPerParameter number of grid points for each parameter
	 */
	public void setGrid(IVector minRange, IVector maxRange, int nPointsPerParameter){
		int n = minRange.getSize();
		//some consistency checks
		if(minRange.getSize()!=maxRange.getSize()){
			throw new RuntimeException("Arrays with minimum of range and maximum of range have unequal length");
		}
		if (nPointsPerParameter==1){
			throw new RuntimeException("Parameter nPointsPerParameter should be larger than one. ");
		}
		// add grid settings
		this.nparam=n;
		this.minRangePar = minRange.getValues();
		this.maxRangePar = maxRange.getValues();
		this.stepRangePar = new double[maxRangePar.length];

		for (int iParam=0; iParam<n; iParam++){
			stepRangePar[iParam] = (maxRangePar[iParam]-minRangePar[iParam])/((double)nPointsPerParameter-1);
		}
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
		int totalSampleSize = (int) Math.pow(nPointsPerParameter,nparam);
		this.SL1 = new IVector[totalSampleSize];

		// compute variances of the parameters (see Appendix B of Rakovec et al, 2014)
		this.paramVariance = new Vector(nparam);
		for (int iParam=0; iParam<this.nparam; iParam++){
			double thisVariance = (maxRangePar[iParam]-minRangePar[iParam])*(maxRangePar[iParam]-minRangePar[iParam])/12.0d;
			this.paramVariance.setValue(iParam,thisVariance);
		}
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
		throw new UnsupportedOperationException("org.openda.algorithms.DelsaCoreOptimizer.getCurrentValues(): Not implemented yet.");
//        return this.pCurrent.clone();
	}

	/**
	 * Get the cost for last evaluation.
	 * @return Cost value for last evaluation
	 */
	public double getCurrentCosts(){
		throw new UnsupportedOperationException("org.openda.algorithms.DelsaCoreOptimizer.getCurrentCosts(): Not implemented yet.");
//		return this.fCurrent;
	}

	/**
	 * Get the sensitivity index at the final step.
	 * @return Vector of sensitivity indices for each parameter set
	 */
	public IVector[] getSensitivityIndex(){
		return this.SL1;
	}

	/**
	 * Get all the sets of parameters used in the sensitivity evaluation.
	 * @return Vector of sets of parameters.
	 */
	public IVector[] getParametersSets(){
		return this.f.getParameters();
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
		// next parameters
		if (this.isSobol){
			double[] deltaPar = sobolSequence.nextVector();
			this.pCurrent = pInit.clone();
			for (int i=0; i<this.nparam; i++){
				double thisRange = this.maxRangePar[i]-this.minRangePar[i];
				this.pCurrent.setValue(i,this.minRangePar[i]+thisRange*deltaPar[i]);
			}
		} else {
			if (this.pInit != null){
				this.pCurrent = pInit.clone();
			}
			this.currentParIndex = nextParIndex(this.currentParIndex,this.numberOfStepsPar);
			int n=this.nparam;
			for(int i=0;i<n;i++){
					this.pCurrent.setValue(i, this.minRangePar[i]+this.stepRangePar[i]*this.currentParIndex[i]);
			}
		}
		//evaluate
		this.fCurrent = this.f.evaluate(this.pCurrent,"any");

		// compute total variance for this parameter sample (equation 12):
		  // a. compute cost gradient w.r.t. parameters (term dPhi/dTheta)
		 IVector costGrad = computeCostGradient(pCurrent);

		  // b. compute total variance of model metric (cost)
		 IVector costGradSquared = costGrad.clone();
		 costGradSquared.pointwiseMultiply(costGrad);
		 double V_L = costGradSquared.dotProduct(paramVariance);

		// compute 1st order sensitivity index for each parameter in this set of parameters (equation 13):
		double[] SL1 = new double[paramVariance.getSize()];
		for (int i=0; i<SL1.length; i++){
			SL1[i] = costGradSquared.getValue(i) * paramVariance.getValue(i) / V_L;
		}
		this.SL1[imain] = new Vector(SL1);

        // check if all points have been tested:
        this.moreToDo = false;
		if (isSobol){
			if (imain<this.SL1.length-1){
				this.moreToDo = true;
			}
		} else {
			for(int i=0;i<this.nparam;i++){
				if(this.currentParIndex[i]<(this.numberOfStepsPar[i]-1)){
					this.moreToDo = true;
				}
			}
		}

		if(!this.moreToDo){
			//Display final results:
			Results.putMessage("===================================================================");
			Results.putMessage("DELSA: ");
			Results.putMessage("    sample size: " + this.SL1.length);
			Results.putMessage("    parameter values:");
			Matrix allParsMatrix = new Matrix(this.f.getParameters());
			Results.putMessage("        " + allParsMatrix.printString());
			Results.putMessage("    sensitivity indices, S_L1: ");
			Matrix allSL1Matrix = new Matrix(this.SL1);
			Results.putMessage("        " + allSL1Matrix.printString());
			Results.putMessage("===================================================================");
			for (int i=0; i<nparam; i++){
				Matrix SL1thisParameter = (Matrix) allSL1Matrix.getMatrixSelection(i,i,0,allSL1Matrix.getNumberOfColumns()-1);
				Results.putValue("SL1_param"+i, SL1thisParameter.asVector(), SL1thisParameter.getNumberOfColumns()*SL1thisParameter.getNumberOfRows(), "final", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Instance);
			}
			for (int i=0; i<nparam; i++){
				Matrix thisParameter = (Matrix) allParsMatrix.getMatrixSelection(i,i,0,allParsMatrix.getNumberOfColumns()-1);
				Results.putValue("Param"+i, thisParameter.asVector(), thisParameter.getNumberOfColumns()*thisParameter.getNumberOfRows(), "final", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Instance);
			}
		}
		imain++;
	}

	private IVector computeCostGradient(IVector currentPar) {
		//Calculate gradient of the cost function w.r.t. model parameters here using forward difference.
		LeastSquaresCostFunction fGradient = (LeastSquaresCostFunction) f.clone();
		int nParam = currentPar.getSize();
		double[] GradDbl = new double[nParam];
		double fwdEps = 0.01; //i.e. 1%
		for (int i=0; i<nParam; i++){
			IVector param = currentPar.clone();
			double delta = fwdEps*param.getValue(i);
			if (param.getValue(i)==0.0){
				System.out.println("WARNING: the parameter is equal to 0. Perturbation used for forward difference is set to 0.01.");
				delta = fwdEps;
			}
			param.setValue(i,param.getValue(i)+delta);
			double fPerturbed = fGradient.evaluate(param, "Forward difference calculation of cost gradient");
			GradDbl[i]=(fPerturbed-fCurrent)/delta;
			param.free();
		}
		return new Vector(GradDbl);
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
