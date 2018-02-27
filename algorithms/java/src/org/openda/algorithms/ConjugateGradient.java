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
import org.openda.utils.Results;
import org.openda.utils.Instance;
import org.openda.utils.Vector;
import org.openda.utils.io.CalRestartSettings;
import org.openda.utils.ConfigTree;

import java.io.File;


/**
 * OpenDA Conjugate Gradient algorithm
 * This class wraps the generic optimization routines in ConjugateGradientCoreOptimizer
 * for use as a tool for the calibration of parameters
 */
public class ConjugateGradient extends Instance implements IAlgorithm {

    public boolean withPreconditioning = true;

	private IStochObserver stochObserver;
    private IStochModelFactory stochModelFactory;
	protected ConfigTree conjugateGradientConf;

    private IStochModelInstance bestEstimate = null;
    
    private SimulationKwadraticCostFunctionWithGradient J=null;
    private PreconditionedCostFunctionWithGradient Jc=null;
    
    private ConjugateGradientCoreOptimizer conjugateGradientOptimizer;

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
        conjugateGradientConf = new ConfigTree(workingDir, configString);

        // Create costFunction
        Results.putMessage("costFunction@class="+conjugateGradientConf.getAsString("costFunction@class","SimulationKwadraticCostFunctionWithGradient"));
        if (conjugateGradientConf.getAsString("costFunction@class","SimulationKwadraticCostFunctionWithGradient").contains("SimulationKwadraticCostFunctionWithGradient")){
            this.J=new SimulationKwadraticCostFunctionWithGradient(stochModelFactory, stochObserver);
        } else {
        	throw new RuntimeException("Only implemented for one costfunction yet: org.openda.algorithms.SimulationKwadraticCostFunctionWithGradient");
        }
        // options for costFunctions
        J.addBackgroundTerm = conjugateGradientConf.getAsBoolean("costFunction@weakParameterConstraint",false);
        Results.putMessage("costFunction@weakParameterConstraint="+J.addBackgroundTerm);
        J.factor = conjugateGradientConf.getAsDouble("costFunction@factor",J.factor);
        Results.putMessage("costFunction@factor="+J.factor);
        this.withPreconditioning = conjugateGradientConf.getAsBoolean("costFunction@withPreconditioning",true);
        Results.putMessage("costFunction@withPreconditioning="+this.withPreconditioning);        
        
       if (withPreconditioning) {
    	   this.scale = this.J.getParameterUncertainty().getStandardDeviations().clone();
           this.scale.sqrt();
           this.init = this.J.getParameterUncertainty().getExpectations().clone();
           this.Jc = new PreconditionedCostFunctionWithGradient(this.J, stochModelFactory,
           			  stochObserver, this.init, this.scale);
           this.conjugateGradientOptimizer = new ConjugateGradientCoreOptimizer(this.Jc);           
        } else {
           this.conjugateGradientOptimizer = new ConjugateGradientCoreOptimizer(this.J);  
        }
        
        String method = conjugateGradientConf.getAsString("outerLoop@method","fletcher-reeves");
        if ((method.equalsIgnoreCase("fletcher reeves")|method.equalsIgnoreCase("fletcher-reeves"))
        	|method.equals("1")){
        	this.conjugateGradientOptimizer.method = 1;
        	Results.putMessage("outerLoop@method=Fletcher-Reeves=1");
        } else {
        	if ((method.equalsIgnoreCase("polak ribiere")|method.equalsIgnoreCase("polak-ribiere"))
        		|method.equals("2")){
            	this.conjugateGradientOptimizer.method = 2;
            	Results.putMessage("outerLoop@method=Polak-Ribiere=2");
        	} else {
        		this.conjugateGradientOptimizer.method = 3;
            	Results.putMessage("outerLoop@method=Steepest Descent=3");
        	}
        }
        this.conjugateGradientOptimizer.method=conjugateGradientConf.getAsInt("outerLoop@method",this.conjugateGradientOptimizer.method);
        Results.putMessage("outerLoop@method="+this.conjugateGradientOptimizer.method);
        this.conjugateGradientOptimizer.maxitConGrad=conjugateGradientConf.getAsInt("outerLoop@maxIterations",this.conjugateGradientOptimizer.maxitConGrad);
        Results.putMessage("outerLoop@maxIterations="+this.conjugateGradientOptimizer.maxitConGrad);
        this.conjugateGradientOptimizer.absTolGrad=conjugateGradientConf.getAsDouble("outerLoop@absTolGrad",this.conjugateGradientOptimizer.absTolGrad);
        Results.putMessage("outerLoop@absTolGrad="+this.conjugateGradientOptimizer.absTolGrad);
        this.conjugateGradientOptimizer.relTolGrad=conjugateGradientConf.getAsDouble("outerLoop@relTolGrad",this.conjugateGradientOptimizer.relTolGrad);
        Results.putMessage("outerLoop@relTolGrad="+this.conjugateGradientOptimizer.relTolGrad);
        this.conjugateGradientOptimizer.absTolStep=conjugateGradientConf.getAsDouble("outerLoop@absTolStep",this.conjugateGradientOptimizer.absTolStep);
        Results.putMessage("outerLoop@absTolStep="+this.conjugateGradientOptimizer.absTolStep);
        this.conjugateGradientOptimizer.relTolStep=conjugateGradientConf.getAsDouble("outerLoop@relTolStep",this.conjugateGradientOptimizer.relTolStep);
        Results.putMessage("outerLoop@relTolStep="+this.conjugateGradientOptimizer.relTolStep);        

        String lineSearchMethod = conjugateGradientConf.getAsString("lineSearch@type","brent");
        if(!lineSearchMethod.equalsIgnoreCase("brent")){
        	throw new RuntimeException("Only method 'brent' supported for linesearch at this time.");
        }
        this.conjugateGradientOptimizer.maxitBrent = conjugateGradientConf.getAsInt("lineSearch@maxIterations",conjugateGradientOptimizer.maxitBrent);
        Results.putMessage("lineSearch@maxIterations="+this.conjugateGradientOptimizer.maxitBrent);
        this.conjugateGradientOptimizer.relTolBrent = conjugateGradientConf.getAsDouble("lineSearch@relTolBrent",conjugateGradientOptimizer.relTolBrent);
        Results.putMessage("lineSearch@relTolBrent="+this.conjugateGradientOptimizer.relTolBrent);
        this.conjugateGradientOptimizer.absTolBrent = conjugateGradientConf.getAsDouble("lineSearch@absTolBrent",conjugateGradientOptimizer.absTolBrent);
        Results.putMessage("lineSearch@absTolBrent="+this.conjugateGradientOptimizer.absTolBrent);
        
        this.conjugateGradientOptimizer.bracketFirstTry = conjugateGradientConf.getAsDouble("lineSearch/brent@startBracketValue",conjugateGradientOptimizer.bracketFirstTry);
        Results.putMessage("lineSearch/brent@startBracketValue="+this.conjugateGradientOptimizer.bracketFirstTry);
        this.conjugateGradientOptimizer.maxitBracket = conjugateGradientConf.getAsInt("lineSearch/brent@maxItBracket",conjugateGradientOptimizer.maxitBracket);
        Results.putMessage("lineSearch/brent@maxItBracket="+this.conjugateGradientOptimizer.maxitBracket);
        this.conjugateGradientOptimizer.limit = conjugateGradientConf.getAsDouble("lineSearch/brent@maxExtension",conjugateGradientOptimizer.limit);
        Results.putMessage("lineSearch/brent@maxExtension="+this.conjugateGradientOptimizer.limit);
        
        // additional stop criteria
        ConfigTree parts[] = conjugateGradientConf.getSubTrees("stopCriteria/stopCriterion");
        if (parts != null) {
            String className;
            double threshold;
            Class javaClass;
            for (ConfigTree part : parts) {
				className = part.getAsString("@class", null);
				threshold = part.getAsDouble("@threshold",this.conjugateGradientOptimizer.stopCritThresDefault);
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
                    this.conjugateGradientOptimizer.stopCriteria.add((IStopCriterion) object);
                    this.conjugateGradientOptimizer.stopCriteriaThreshold.add(threshold);
                } catch (ClassNotFoundException e) {
                    throw new RuntimeException("Could not create instance for "+className+": "+e.getMessage());
                }
				
            }
            IStochModelInstance stochModelInstance = stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
            ITime selectionTimes = stochModelInstance.getTimeHorizon();
            this.conjugateGradientOptimizer.obsDescr = stochObserver.createSelection(selectionTimes).getObservationDescriptions();
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
        this.conjugateGradientOptimizer.initialize(initialParameters);
        stochModelInstance.finish();
	}

    
    /**
     * Main routine for Conjugate Gradient calibration
     * Here the calibration problem is converted to an optimization problem and
     * the ConjugateGradientCoreOptimizer is then started.
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
		return this.conjugateGradientOptimizer.hasNext();
	}
	
	/**
	 * Run next step of the algorithm
	 */
	public void next(){
		this.conjugateGradientOptimizer.next();
        this.bestEstimate = this.J.getBestModel();
	}

    public IModelState saveInternalState() {
        CalRestartSettings restartSettings = new CalRestartSettings("conjugate gradient");
        restartSettings.setComment("saved state for conjugate gradient algorithm");
//TODO restartSettings.setParameters(this.conjugateGradientOptimizer.getOptimalValue(),this.conjugateGradientOptimizer.getOptimalCost(),this.conjugateGradientOptimizer.getGradient(),this.conjugateGradientOptimizer.getSearchDirection());
        return restartSettings;
    }

    public void restoreInternalState(IModelState savedInternalState) {
        if (!(savedInternalState instanceof CalRestartSettings)) {
            throw new IllegalArgumentException("Unexpected saved State type: " + savedInternalState.getClass().getName());
        }
        CalRestartSettings restartSettings = (CalRestartSettings) savedInternalState;
//TODO this.conjugateGradientOptimizer.initialize(restartSettings.getBaseParameter(),restartSettings.getCost(),restartSettings.getGradient(),restartSettings.getSearchDirection());
    }

	public void releaseInternalState(IModelState savedInternalState) {
		if (!(savedInternalState instanceof CalRestartSettings)) {
			throw new IllegalArgumentException("Unexpected saved State type: " + savedInternalState.getClass().getName());
		}
		// no action needed
	}

	public IModelState loadPersistentState(File algorithmStateFile) {
		return new CalRestartSettings("conjugate gradient", algorithmStateFile);
	}

	public IStochModelInstance getBestEstimate() {
        return bestEstimate;
    }

	public void finish() {
		// each created model instance has been finished individually
		// (in prepare() and in the cost function)
	}

	public IVector getState(int iDomain) {
		return this.getState();
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
