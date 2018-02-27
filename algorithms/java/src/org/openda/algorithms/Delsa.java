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

import org.openda.interfaces.*;
import org.openda.utils.ConfigTree;
import org.openda.utils.Results;
import org.openda.utils.Instance;
import org.openda.utils.Vector;

import java.io.File;


/**
 * OpenDA DELSA algorithm, based on Rakovec et al (2014): Distributed evaluation of local sensitivity analysis (DELSA),
 * with application to hydrologic models, WRR, Vol.50,409-426.
 * This class wraps the generic optimization routines in DelsaCoreOptimizer for use as a tool
 * for the parameters sensitivity analysis.
 */
public class Delsa extends Instance implements IAlgorithm {

    private IStochModelFactory stochModelFactory;
    protected ConfigTree delsaConf;

    private IStochModelInstance bestEstimate = null;
    private IStochObserver stochObserver = null;

    private SimulationKwadraticCostFunction J=null;
    private DelsaCoreOptimizer delsaOptimizer;

    
    public void initialize(File workingDir, String[] arguments) {
        String configString = arguments[0];
        delsaConf = new ConfigTree(workingDir, configString);
        Results.putMessage("configstring = "+ configString);
    }

    public void setStochComponents(IStochObserver stochObserver, IStochModelFactory stochModelFactory) {
        this.stochModelFactory = stochModelFactory;
        this.stochObserver = stochObserver;

        // Create costFunction
        Results.putMessage("costFunction@class=" + delsaConf.getAsString("costFunction@class", "SimulationKwadraticCostFunction"));
        if(delsaConf.getAsString("costFunction@class","SimulationKwadraticCostFunction").contains("SimulationKwadraticCostFunction")){
            this.J = new SimulationKwadraticCostFunction(stochModelFactory, stochObserver);
        }else{
        	throw new RuntimeException("Only implemented for one costfunction yet: org.openda.algorithms.SimulationKwadraticCostFunction");
        }
        // options for costFunctions
        J.addBackgroundTerm = delsaConf.getAsBoolean("costFunction@weakParameterConstraint",true);
        Results.putMessage("costFunction@weakParameterConstraint="+J.addBackgroundTerm);
        J.factor = delsaConf.getAsDouble("costFunction@factor",J.factor);
        Results.putMessage("costFunction@factor="+J.factor);
    }

    public void prepare() {
        //create grid
        IStochModelInstance stochModelInstance = stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
        IVector minRange;
        IVector maxRange;
		int nPoints = Integer.MAX_VALUE;

        // parse and use grid settings
		boolean isSobolUsed = false;
        String minRangeString = delsaConf.getAsString("regularGrid@min", "not specified");
		if (!minRangeString.contains("not specified")){
			minRange = new Vector(minRangeString);
			String maxRangeString = delsaConf.getAsString("regularGrid@max", "not specified");
			if (maxRangeString.contains("not specified")){
				throw new RuntimeException("regularGrid: parameter max should be specified.");
			}
			maxRange = new Vector(maxRangeString);
			nPoints = delsaConf.getAsInt("regularGrid@nPointsPerParameter", Integer.MAX_VALUE);
			if (nPoints==Integer.MAX_VALUE){
				throw new RuntimeException("regularGrid: nPointsPerParameter should be specified.");
			}
		} else {
			minRangeString = delsaConf.getAsString("sobolSequence@min", "not specified");
			if (!minRangeString.contains("not specified")){
				isSobolUsed = true;
				minRange = new Vector(minRangeString);
				String maxRangeString = delsaConf.getAsString("sobolSequence@max", "not specified");
				if (maxRangeString.contains("not specified")){
					throw new RuntimeException("sobolSequence:parameter max should be specified.");
				}
				maxRange = new Vector(maxRangeString);
				nPoints = delsaConf.getAsInt("sobolSequence@totalSample", Integer.MAX_VALUE);
				if (nPoints==Integer.MAX_VALUE){
					throw new RuntimeException("sobolSequence: totalSample should be specified.");
				}
			} else {
				throw new RuntimeException("Grid parameters should be specified: regularGrid or sobolSequence. Grid " +
						"parameters are min, max, and nPointsPerParameter (regularGrid) or totalSample (sobolSequence)");
			}
		}

		// create optimizer
		IVector pInit = stochModelInstance.getParameters();
		this.delsaOptimizer = new DelsaCoreOptimizer(J,minRange,maxRange,nPoints,isSobolUsed,pInit);
		stochModelInstance.finish();
	}

    /**
     * Main routine for Delsa
     * Here the calibration problem is converted to an optimization problem and
     * the delsaCoreOptimizer is then started.
     */
    public void run() {
    	while(this.hasNext()){
    		this.next();
    	}
    }

	/**
	 * Are there any more steps for this algorithm
	 * @return has next step
	 */
	public boolean hasNext(){
		return this.delsaOptimizer.hasNext();
	}

	/**
	 * Run next step of the algorithm
	 */
	public void next(){
		this.delsaOptimizer.next();
        this.bestEstimate = this.J.getBestModel();
	}

    public IModelState saveInternalState() {
        throw new UnsupportedOperationException("org.openda.algorithms.Delsa.restoreInternalState(): Not implemented yet.");
    }

    public void restoreInternalState(IModelState savedInternalState) {
        throw new UnsupportedOperationException("org.openda.algorithms.Delsa.restoreInternalState(): Not implemented yet.");
    }

	public void releaseInternalState(IModelState savedInternalState) {
		throw new UnsupportedOperationException("org.openda.algorithms.Delsa.releaseInternalState(): Not implemented yet.");
	}

	public IModelState loadPersistentState(File algorithmStateFile) {
		throw new UnsupportedOperationException("org.openda.algorithms.Delsa.loadPersistentState(): Not implemented yet.");
	}

	public IStochModelInstance getBestEstimate() {
        return bestEstimate;
    }

	public void finish() {
		// each created model instance has been finished individually (in prepare() and
		// in the cost function)
	}


	public IVector getState(int iDomain) {
		return this.getState();
	}

	public IVector getState() {
		throw new UnsupportedOperationException("method getState not implemented."+this.getClass().getName());
	}

	
	public ITime getTimeHorizon() {
		throw new UnsupportedOperationException("method getTimeHorizon not implemented."+this.getClass().getName());
	}

	
	public ITime getCurrentTime() {
		throw new UnsupportedOperationException("method getCurrentTime not implemented."+this.getClass().getName());
//		return null;
	}

	
	public void compute(ITime targetTime) {
		throw new UnsupportedOperationException("method compute not implemented."+this.getClass().getName());
	}

}
