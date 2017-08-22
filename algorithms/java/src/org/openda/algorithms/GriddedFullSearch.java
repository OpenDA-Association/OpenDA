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

import org.openda.interfaces.*;
import org.openda.utils.ConfigTree;
import org.openda.utils.Results;
import org.openda.utils.Instance;
import org.openda.utils.Vector;

import java.io.File;


/**
 * OpenDA GriddedFullSearch algorithm
 * This class wraps the generic optimization routines in GriddedFullSearchCoreOptimizer for use as a tool
 * for the calibration of parameters
 */
public class GriddedFullSearch extends Instance implements IAlgorithm {

    IStochModelFactory stochModelFactory;
    ConfigTree gfsConf;

    private IStochModelInstance bestEstimate = null;
    private IStochObserver stochObserver = null;

    private SimulationKwadraticCostFunction J=null;
    private GriddedFullSearchCoreOptimizer gfsOptimizer;

    
    public void initialize(File workingDir, String[] arguments) {
        String configString = arguments[0];
        gfsConf = new ConfigTree(workingDir, configString);
        Results.putMessage("configstring = "+ configString);
    }

    public void setStochComponents(IStochObserver stochObserver, IStochModelFactory stochModelFactory) {
        this.stochModelFactory = stochModelFactory;
        this.stochObserver = stochObserver;

        // Create costFunction
        Results.putMessage("costFunction@class="+gfsConf.getAsString("costFunction@class","SimulationKwadraticCostFunction"));
        if(gfsConf.getAsString("costFunction@class","SimulationKwadraticCostFunction").contains("SimulationKwadraticCostFunction")){
            this.J = new SimulationKwadraticCostFunction(stochModelFactory, stochObserver);
        }else{
        	throw new RuntimeException("Only implemented for one costfunction yet: org.openda.algorithms.SimulationKwadraticCostFunction");
        }
        // options for costFunctions
        J.addBackgroundTerm = gfsConf.getAsBoolean("costFunction@weakParameterConstraint",false);
        Results.putMessage("costFunction@weakParameterConstraint="+J.addBackgroundTerm);
        J.factor = gfsConf.getAsDouble("costFunction@factor",J.factor);
        Results.putMessage("costFunction@factor="+J.factor);
    }

    public void prepare() {
        //create grid
        IStochModelInstance stochModelInstance = stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
        // initial uncertainty for parameters is used to start the optimization
        IStochVector parameterUncertainty = stochModelInstance.getParameterUncertainty();
        IVector std = parameterUncertainty.getStandardDeviations();
        IVector mean = parameterUncertainty.getExpectations();
        // use mean-2*sigma:0.2*sigma:mean+2*sigma as default grid
        IVector minRange = mean.clone();
        minRange.axpy(-2.0, std);
        IVector maxRange = mean.clone();
        maxRange.axpy(2.0, std);
        IVector stepRange = std.clone();
        stepRange.scale(0.2);
        // create optimizer
//        this.gfsOptimizer = new GriddedFullSearchCoreOptimizer(J,minRange,maxRange,stepRange);
        IVector pInit = stochModelInstance.getParameters();
        this.gfsOptimizer = new GriddedFullSearchCoreOptimizer(J,minRange,maxRange,stepRange,pInit);

        // parse and use grid settings
        //     <gridRange min="[0.04,2.4]" max="[0.08,3.3]" step="[0.005,0.1]" />
        String minRangeString = gfsConf.getAsString("gridRange@min", minRange.toString());
        minRange = new Vector(minRangeString);
        String maxRangeString = gfsConf.getAsString("gridRange@max", maxRange.toString());
        maxRange = new Vector(maxRangeString);
        String stepRangeString = gfsConf.getAsString("gridRange@step", stepRange.toString());
        stepRange = new Vector(stepRangeString);
        this.gfsOptimizer.setGrid(minRange,maxRange,stepRange);
        // additional stopCriteria
        ConfigTree parts[] = gfsConf.getSubTrees("stopCriteria/stopCriterion");
        if (parts != null) {
            String className = null;
            double threshold;
            Class javaClass;
            Object object = null;
            for (ConfigTree part : parts) {
                className = part.getAsString("@class", null);
                threshold = part.getAsDouble("@threshold", this.gfsOptimizer.stopCritThresDefault);
                Results.putMessage(this, "stopCriteria/stopCriterion@class=" + className + ", @threshold=" + threshold);
                try {
                    javaClass = Class.forName(className);
                    try {
                        object = javaClass.newInstance();
                    } catch (InstantiationException e) {
                        e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                    } catch (IllegalAccessException e) {
                        e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
                    }
                    this.gfsOptimizer.stopCriteria.add((IStopCriterion) object);
                    this.gfsOptimizer.stopCriteriaThreshold.add(threshold);
                } catch (ClassNotFoundException e) {
                    throw new RuntimeException("Could not create instance for " + className + ": " + e.getMessage());
                }
            }
            ITime selectionTimes = stochModelInstance.getTimeHorizon();
            this.gfsOptimizer.obsDescr = this.stochObserver.createSelection(selectionTimes).getObservationDescriptions();
        }
		stochModelInstance.finish();
	}

    /**
     * Main routine for GriddedFullSearch calibration
     * Here the calibration problem is converted to an optimization problem and
     * the gfsCoreOptimizer is then started.
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
		return this.gfsOptimizer.hasNext();
	}
	
	/**
	 * Run next step of the algorithm
	 */
	public void next(){
		this.gfsOptimizer.next();
        this.bestEstimate = this.J.getBestModel();
	}

    public IModelState saveInternalState() {
        throw new UnsupportedOperationException("org.openda.algorithms.GriddedFullSearch.restoreInternalState(): Not implemented yet.");
    }

    public void restoreInternalState(IModelState savedInternalState) {
        throw new UnsupportedOperationException("org.openda.algorithms.GriddedFullSearch.restoreInternalState(): Not implemented yet.");
    }

	public void releaseInternalState(IModelState savedInternalState) {
		throw new UnsupportedOperationException("org.openda.algorithms.GriddedFullSearch.releaseInternalState(): Not implemented yet.");
	}

	public IModelState loadPersistentState(File algorithmStateFile) {
		throw new UnsupportedOperationException("org.openda.algorithms.GriddedFullSearch.loadPersistentState(): Not implemented yet.");
	}

	public IStochModelInstance getBestEstimate() {
        return bestEstimate;
    }

	public void finish() {
		// each created model instance has been finished individually (in prepare() and
		// in the cost function)
	}

	
	public IVector getState() {
		// TODO Auto-generated method stub
		return null;
	}

	
	public ITime getTimeHorizon() {
		throw new UnsupportedOperationException("method getTimeHorizon not implemented."+this.getClass().getName());
	}

	
	public ITime getCurrentTime() {
		return null;
	}

	
	public void compute(ITime targetTime) {
		throw new UnsupportedOperationException("method compute not implemented."+this.getClass().getName());
	}

}
