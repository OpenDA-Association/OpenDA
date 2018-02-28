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


package org.openda.algorithms;

import org.openda.interfaces.*;
import org.openda.utils.Results;
import org.openda.utils.Instance;
import org.openda.utils.io.CalRestartSettings;
import org.openda.utils.ConfigTree;

import java.io.File;


/**
 * OpenDA Powell algorithm
 * This class wraps the generic optimization routines in PowellCoreOptimizer for use as a tool
 * for the calibration of parameters
 */
public class Powell extends Instance implements IAlgorithm {

	private IStochObserver stochObserver;
    IStochModelFactory stochModelFactory;
    ConfigTree powellConf;

    private IStochModelInstance bestEstimate = null;

    private SimulationKwadraticCostFunction J=null;
    private PowellCoreOptimizer powellOptimizer;

    private File workingDir;
    private String configString;

    public void initialize(File workingDir, String[] arguments) {
        this.workingDir = workingDir;
        configString = arguments[0];
    }

    public void setStochComponents(IStochObserver stochObserver, IStochModelFactory stochModelFactory) {
        this.stochModelFactory = stochModelFactory;
		this.stochObserver = stochObserver;

        Results.putMessage("configstring = "+ configString);
        powellConf = new ConfigTree(workingDir, configString);

        // Create costFunction
        Results.putMessage("costFunction@class="+powellConf.getAsString("costFunction@class","SimulationKwadraticCostFunction"));
        if(powellConf.getAsString("costFunction@class","SimulationKwadraticCostFunction").contains("SimulationKwadraticCostFunction")){
            this.J = new SimulationKwadraticCostFunction(stochModelFactory, stochObserver);
        }else{
        	throw new RuntimeException("Only implemented for one costfunction yet: org.openda.algorithms.SimulationKwadraticCostFunction");
        }
        // options for costFunctions
        J.addBackgroundTerm = powellConf.getAsBoolean("costFunction@weakParameterConstraint",false);
        Results.putMessage("costFunction@weakParameterConstraint="+J.addBackgroundTerm);
        J.factor = powellConf.getAsDouble("costFunction@factor",J.factor);
        Results.putMessage("costFunction@factor="+J.factor);

        // PowellCoreOptimizer powellOptimizer = new PowellCoreOptimizer(J,initialParameters,0.1);
        this.powellOptimizer = new PowellCoreOptimizer(J);

        // configure PowellCoreOptimizer
        // <outerLoop maxIterations="10" absTolerance="0.01" relTolerance="0.01" />
        // <lineSearch type="brent" maxIterations="20" relTolerance="0.01" maxRelStepSize="100.0" >
        //    <brent startBracketValue="1.0" />
        // </lineSearch>
        //outerloop
        this.powellOptimizer.maxitPowell = powellConf.getAsInt("outerLoop@maxIterations",powellOptimizer.maxitPowell);
        Results.putMessage("outerLoop@maxIterations="+this.powellOptimizer.maxitPowell);
        this.powellOptimizer.absTolPowell=powellConf.getAsDouble("outerLoop@absTolerance",this.powellOptimizer.absTolPowell);
        Results.putMessage("outerLoop@absTolerance="+this.powellOptimizer.absTolPowell);
        this.powellOptimizer.relTolPowell=powellConf.getAsDouble("outerLoop@relTolerance",this.powellOptimizer.relTolPowell);
        Results.putMessage("outerLoop@relTolerance="+this.powellOptimizer.relTolPowell);
        //linesearch
        // <lineSearch type="brent" maxIterations="20" relTolerance="0.01" maxRelStepSize="100.0" >
        //    <brent startBracketValue="1.0" />
        // </lineSearch>
        String lineSearchMethod = powellConf.getAsString("lineSearch@type","brent");
        if(!lineSearchMethod.equalsIgnoreCase("brent")){
        	throw new RuntimeException("Only method 'brent' supported for linesearch at this time.");
        }
        this.powellOptimizer.maxitBrent = powellConf.getAsInt("lineSearch@maxIterations",powellOptimizer.maxitBrent);
        Results.putMessage("lineSearch@maxIterations="+this.powellOptimizer.maxitBrent);
        this.powellOptimizer.limit = powellConf.getAsDouble("lineSearch@maxRelStepSize",powellOptimizer.limit);
        Results.putMessage("lineSearch@maxRelStepSize="+this.powellOptimizer.limit);
        this.powellOptimizer.bracketFirstTry = powellConf.getAsDouble("lineSearch/brent@startBracketValue",powellOptimizer.bracketFirstTry);
        Results.putMessage("lineSearch/brent@startBracketValue="+this.powellOptimizer.bracketFirstTry);
        this.powellOptimizer.relTolBrent = powellConf.getAsDouble("lineSearch@relTolerance",powellOptimizer.relTolBrent);
        Results.putMessage("lineSearch@relTolerance="+this.powellOptimizer.relTolBrent);
        this.powellOptimizer.absTolBrent = powellConf.getAsDouble("lineSearch@absTolerance",powellOptimizer.absTolBrent);
        Results.putMessage("lineSearch@absTolerance="+this.powellOptimizer.absTolBrent);
        // additional stopCriteria
        ConfigTree parts[] = powellConf.getSubTrees("stopCriteria/stopCriterion");
        if (parts != null) {
            String className;
            double threshold;
            Class javaClass;
            for (ConfigTree part : parts) {
				className = part.getAsString("@class", null);
				threshold = part.getAsDouble("@threshold", this.powellOptimizer.stopCritThresDefault);
				Results.putMessage(this, "stopCriteria/stopCriterion@class=" + className + ", @threshold=" + threshold);
				try {
					javaClass = Class.forName(className);
					Object object;
					try {
                        object = javaClass.newInstance();
                    } catch (Exception e) {
                        throw new RuntimeException(this.getClass().getName() + ": error creating instance of " +
								className + ": " + e.getMessage());
                    }
                    this.powellOptimizer.stopCriteria.add((IStopCriterion) object);
                    this.powellOptimizer.stopCriteriaThreshold.add(threshold);
                } catch (ClassNotFoundException e) {
                    throw new RuntimeException("Could not create instance for " + className + ": " + e.getMessage());
                }
            }
            IStochModelInstance stochModelInstance = stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
            ITime selectionTimes = stochModelInstance.getTimeHorizon();
            this.powellOptimizer.obsDescr = stochObserver.createSelection(selectionTimes).getObservationDescriptions();
            stochModelInstance.finish();
        }
    }

    public void prepare() {
        //create initial value and directions
        IStochModelInstance stochModelInstance = stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
        IVector initialParameters = stochModelInstance.getParameters();
        // initial uncertainty for parameters is used to start the optimization
        IStochVector parameterUncertainty = stochModelInstance.getParameterUncertainty();
        ISqrtCovariance L_par = parameterUncertainty.getSqrtCovariance();
        IVector[] searchDirections = L_par.asVectorArray();
        this.powellOptimizer.initialize(initialParameters,searchDirections);
		stochModelInstance.finish();
	}

    /**
     * Main routine for Powel calibration
     * Here the calibration problem is converted to an optimization problem and
     * the PowellCoreOptimizer is then started.
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
		return this.powellOptimizer.hasNext();
	}
	
	/**
	 * Run next step of the algorithm
	 */
	public void next(){
		this.powellOptimizer.next();
        this.bestEstimate = this.J.getBestModel();
	}

    public IModelState saveInternalState() {
        CalRestartSettings restartSettings = new CalRestartSettings("powell");
        restartSettings.setComment("saved state for powell algorithm");
        restartSettings.setParameters(this.powellOptimizer.getCurrentValue(),
                this.powellOptimizer.getCurrentSearchDirections());
        restartSettings.setCostValue(this.powellOptimizer.getCurrentCost());
        return restartSettings;
    }

    public void restoreInternalState(IModelState savedInternalState) {
        if (!(savedInternalState instanceof CalRestartSettings)) {
            throw new IllegalArgumentException("Unexpected saved State type: " + savedInternalState.getClass().getName());
        }
        CalRestartSettings restartSettings = (CalRestartSettings) savedInternalState;
        this.powellOptimizer.initialize(restartSettings.getBaseParameters(),
                restartSettings.getCost(), restartSettings.getSearchDirParameters());
    }

	public void releaseInternalState(IModelState savedInternalState) {
		if (!(savedInternalState instanceof CalRestartSettings)) {
			throw new IllegalArgumentException("Unexpected saved State type: " + savedInternalState.getClass().getName());
		}
		// no action needed
	}

	public IModelState loadPersistentState(File algorithmStateFile) {
		return new CalRestartSettings("powell", algorithmStateFile);
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
