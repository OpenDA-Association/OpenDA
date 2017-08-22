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
import org.openda.utils.Results;
import org.openda.utils.Instance;
import org.openda.utils.Vector;
import org.openda.utils.io.CalRestartSettings;
import org.openda.utils.ConfigTree;

import java.io.File;


/**
 * OpenDA BFGS algorithm
 * This class wraps the generic optimization routines in BFGSCoreOptimizer
 * for use as a tool for the calibration of parameters
 */
public class BFGS extends Instance implements IAlgorithm {

    public boolean withPreconditioning = true;
	
	private IStochObserver stochObserver;
	IStochModelFactory stochModelFactory;
    ConfigTree bfgsConf;

    private IStochModelInstance bestEstimate = null;

    private SimulationKwadraticCostFunctionWithGradient J=null;
    private PreconditionedCostFunctionWithGradient Jc=null;
    
    private BFGSCoreOptimizer bfgsOptimizer;

    private File workingDir;
    private String configString;

    private IVector init;
    private IVector scale;

    public void initialize(File workingDir, String[] arguments) {
        this.workingDir = workingDir;
        configString = arguments[0];
    }

    public void setStochComponents(IStochObserver stochObserver,IStochModelFactory stochModelFactory){
        this.stochModelFactory = stochModelFactory;
		this.stochObserver = stochObserver;

        Results.putMessage("configstring = "+ configString);
        bfgsConf = new ConfigTree(workingDir, configString);
        
        /*
         * Input syntax
         *  
         * <?xml version="1.0" encoding="UTF-8"?>
		 * <BFGSConfig>
		 * 		<costFunction weakParameterConstraint="false" withPreconditioning="true" factor="1.0" /> 
		 * 		<outerLoop limitedMemory="true" numOfVectors="3" maxIterations="20" absTolGrad="0.01" relTolGrad="0.01" absTolStep="0.01" relTolStep="0.01" /> 
		 * 			<lineSearch type="brent" maxIterations="200" absTolBrent="0.01" relTolBrent="0.01">
		 * 				<brent startBracketValue="1.0" maxItBracket="20.0" maxExtension="100.0" /> 
		 * 			</lineSearch>
		 * </BFGSConfig>
		 */
		 
        // create costFunction
        Results.putMessage("costFunction@class="+bfgsConf.getAsString("costFunction@class","SimulationKwadraticCostFunction"));
        if(bfgsConf.getAsString("costFunction@class","SimulationKwadraticCostFunction").contains("SimulationKwadraticCostFunction")){
            this.J=new SimulationKwadraticCostFunctionWithGradient(stochModelFactory, stochObserver);
        }else{
        	throw new RuntimeException("Only implemented for one costfunction yet: org.openda.algorithms.SimulationKwadraticCostFunction");
        }
        // options for costFunctions
        J.addBackgroundTerm = bfgsConf.getAsBoolean("costFunction@weakParameterConstraint",false);
        Results.putMessage("costFunction@weakParameterConstraint="+J.addBackgroundTerm);
        J.factor = bfgsConf.getAsDouble("costFunction@factor",J.factor);
        Results.putMessage("costFunction@factor="+J.factor);
        this.withPreconditioning = bfgsConf.getAsBoolean("costFunction@withPreconditioning",true);
        Results.putMessage("costFunction@withPreconditioning="+this.withPreconditioning);        

        if (withPreconditioning) {
            this.scale = this.J.getParameterUncertainty().getStandardDeviations().clone();
            this.scale.sqrt();
            this.init = this.J.getParameterUncertainty().getExpectations().clone();
            this.Jc = new PreconditionedCostFunctionWithGradient(this.J, stochModelFactory,
            			  stochObserver, this.init, this.scale);
            this.bfgsOptimizer = new BFGSCoreOptimizer(this.Jc);           
        } else {
            this.bfgsOptimizer = new BFGSCoreOptimizer(this.J);  
        }

        // configure BFGSCoreOptimizer
        this.bfgsOptimizer.limitedMemory = bfgsConf.getAsBoolean("outerLoop@limitedMemory",bfgsOptimizer.limitedMemory);
        Results.putMessage("outerLoop@limitedMemory="+this.bfgsOptimizer.limitedMemory);
    	this.bfgsOptimizer.nStore = bfgsConf.getAsInt("outerLoop@numOfVectors",bfgsOptimizer.nStore);
        if (this.bfgsOptimizer.limitedMemory = true) {
            Results.putMessage("outerLoop@numOfVectors="+this.bfgsOptimizer.nStore);
        }
        this.bfgsOptimizer.maxitBfgs=bfgsConf.getAsInt("outerLoop@maxIterations",bfgsOptimizer.maxitBfgs);
        Results.putMessage("outerLoop@maxIterations="+this.bfgsOptimizer.maxitBfgs);
        this.bfgsOptimizer.absTolGrad=bfgsConf.getAsDouble("outerLoop@absTolGrad",bfgsOptimizer.absTolGrad);
        Results.putMessage("outerLoop@absTolGrad="+this.bfgsOptimizer.absTolGrad);
        this.bfgsOptimizer.relTolGrad=bfgsConf.getAsDouble("outerLoop@relTolGrad",bfgsOptimizer.relTolGrad);
        Results.putMessage("outerLoop@relTolGrad="+this.bfgsOptimizer.relTolGrad);
        this.bfgsOptimizer.absTolStep=bfgsConf.getAsDouble("outerLoop@absTolStep",bfgsOptimizer.absTolStep);
        Results.putMessage("outerLoop@absTolStep="+this.bfgsOptimizer.absTolStep);
        this.bfgsOptimizer.relTolStep=bfgsConf.getAsDouble("outerLoop@relTolStep",bfgsOptimizer.relTolStep);
        Results.putMessage("outerLoop@relTolStep="+this.bfgsOptimizer.relTolStep);

        String lineSearchMethod = bfgsConf.getAsString("lineSearch@type","brent");
        if(!lineSearchMethod.equalsIgnoreCase("brent")){
        	throw new RuntimeException("Only method 'brent' supported for linesearch at this time.");
        }
        this.bfgsOptimizer.maxitBrent = bfgsConf.getAsInt("lineSearch@maxIterations",bfgsOptimizer.maxitBrent);
        Results.putMessage("lineSearch@maxIterations="+this.bfgsOptimizer.maxitBrent);
        this.bfgsOptimizer.absTolBrent = bfgsConf.getAsDouble("lineSearch@absTolBrent",bfgsOptimizer.absTolBrent);
        Results.putMessage("lineSearch@absTolBrent="+this.bfgsOptimizer.absTolBrent);
        this.bfgsOptimizer.relTolBrent = bfgsConf.getAsDouble("lineSearch@relTolBrent",bfgsOptimizer.relTolBrent);
        Results.putMessage("lineSearch@relTolBrent="+this.bfgsOptimizer.relTolBrent);
        
        this.bfgsOptimizer.bracketFirstTry = bfgsConf.getAsDouble("lineSearch/brent@startBracketValue",bfgsOptimizer.bracketFirstTry);
        Results.putMessage("lineSearch/brent@startBracketValue="+this.bfgsOptimizer.bracketFirstTry);
        this.bfgsOptimizer.maxitBracket = bfgsConf.getAsInt("lineSearch/brent@maxItBracket",bfgsOptimizer.maxitBracket);
        Results.putMessage("lineSearch/brent@maxItBracket="+this.bfgsOptimizer.maxitBracket);
        this.bfgsOptimizer.limit = bfgsConf.getAsDouble("lineSearch/brent@maxExtension",bfgsOptimizer.limit);
        Results.putMessage("lineSearch/brent@maxExtension="+this.bfgsOptimizer.limit);
        
        // additional stop criteria
        ConfigTree parts[] = bfgsConf.getSubTrees("stopCriteria/stopCriterion");
        if (parts != null) {
            String className;
            double threshold;
            Class javaClass;
            for (ConfigTree part : parts) {
				className = part.getAsString("@class", null);
				threshold = part.getAsDouble("@threshold",this.bfgsOptimizer.stopCritThresDefault);
				Results.putMessage(this,"stopCriteria/stopCriterion@class="+className+", @threshold="+threshold);
				try {
					javaClass = Class.forName(className);
					Object object;
					try {
                        object = javaClass.newInstance();
                    } catch (Exception e) {
                        throw new RuntimeException(this.getClass().getName()+
                        		  ": error creating instance of "+className + ": "+e.getMessage());
                    }
                    this.bfgsOptimizer.stopCriteria.add((IStopCriterion) object);
                    this.bfgsOptimizer.stopCriteriaThreshold.add(threshold);
                } catch (ClassNotFoundException e) {
                    throw new RuntimeException("Could not create instance for "+className+": "+e.getMessage());
                }
            }
            IStochModelInstance stochModelInstance = stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
            ITime selectionTimes = stochModelInstance.getTimeHorizon();
            this.bfgsOptimizer.obsDescr = stochObserver.createSelection(selectionTimes).getObservationDescriptions();
            stochModelInstance.finish();
        }
    }

    public void prepare() {
    	//create initial value
        IStochModelInstance stochModelInstance = stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);

        IVector initialParameters = stochModelInstance.getParameters();
        if (withPreconditioning) {
        	initialParameters.setConstant(0.0);
        }
        this.bfgsOptimizer.initialize(initialParameters);
		stochModelInstance.finish();
	}
    
    /**
     * Main routine for BFGS calibration
     * Here the calibration problem is converted to an optimization problem and
     * the BFGSCoreOptimizer is then started.
     */
    public void run() {        
    	while(this.hasNext()){
    		this.next();
    	}
    }

	/**
	 * Are there any more steps for this algorithm?
	 * @return has next step
	 */
	public boolean hasNext(){
		return this.bfgsOptimizer.hasNext();
	}
	
	/**
	 * Run next step of the algorithm
	 */
	public void next(){
		this.bfgsOptimizer.next();
        this.bestEstimate = this.J.getBestModel();
	}

    public IModelState saveInternalState() {
        CalRestartSettings restartSettings = new CalRestartSettings("bfgs");
        restartSettings.setComment("saved state for BFGS algorithm");
        if (this.bfgsOptimizer.limitedMemory = true) {
//TODO restartSettings.setParameters(this.bfgsOptimizer.getOptimalValue(),this.bfgsOptimizer.getOptimalCost(),this.bfgsOptimizer.getGradient(),this.bfgsOptimizer.getSs(),this.bfgsOptimizer.getSy());
        } else {
//TODO restartSettings.setParameters(this.bfgsOptimizer.getOptimalValue(),this.bfgsOptimizer.getOptimalCost(),this.bfgsOptimizer.getGradient(),this.bfgsOptimizer.getMatrix());
        }
        restartSettings.setCostValue(this.bfgsOptimizer.getOptimalCost());
        return restartSettings;
    }

    public void restoreInternalState(IModelState savedInternalState) {
        if (!(savedInternalState instanceof CalRestartSettings)) {
            throw new IllegalArgumentException("Unexpected saved State type: " + savedInternalState.getClass().getName());
        }
        CalRestartSettings restartSettings = (CalRestartSettings) savedInternalState;
        if (this.bfgsOptimizer.limitedMemory = true) {
//TODO this.bfgsOptimizer.initializeLBFGS(restartSettings.getBaseParameter(),restartSettings.getCost(), restartSettings.getGradient(), restartSettings.getSs(), restartSettings.getSy());
        } else {
//TODO this.bfgsOptimizer.initializeBFGS(restartSettings.getBaseParameter(),restartSettings.getCost(), restartSettings.getGradient(), restartSettings.getMatrix());
        }
    }

	public void releaseInternalState(IModelState savedInternalState) {
		if (!(savedInternalState instanceof CalRestartSettings)) {
			throw new IllegalArgumentException("Unexpected saved State type: " + savedInternalState.getClass().getName());
		}
		// no action needed
	}

	public IModelState loadPersistentState(File algorithmStateFile) {
		return new CalRestartSettings("bfgs", algorithmStateFile);
	}

	public IStochModelInstance getBestEstimate() {
        return bestEstimate;
    }

	public void finish() {
		// each created model instance has been finished individually
		// (in prepare() and in the cost function)
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
