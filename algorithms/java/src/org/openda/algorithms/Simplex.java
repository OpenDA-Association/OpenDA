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
import org.openda.utils.Instance;
import org.openda.utils.Results;
import org.openda.utils.io.CalRestartSettings;

import java.io.File;


/**
 * OpenDA simplex algorithm
 */
public class Simplex extends Instance implements IAlgorithm {

    IStochModelFactory stochModelFactory;
    ConfigTree simplexConf;

    private IStochModelInstance bestEstimate = null;
    //config
    public double initStep=1.0; //scaling factor for initial perturbations of parameters
    //workspace
    private SimulationKwadraticCostFunction J=null;
    private SimplexCoreOptimizer simplexOptimizer = null;

    private File workingDir=null;
    private String[] arguments=null;
    private IStochObserver stochObserver;

    public void initialize(File workingDir, String[] arguments) {
        this.workingDir = workingDir;
        this.arguments = arguments;
    }

    public void setStochComponents(IStochObserver stochObserver, IStochModelFactory stochModelFactory) {
        this.stochModelFactory = stochModelFactory;
        this.stochObserver = stochObserver;
        //parse config
        // <SimplexConfig>
        //     <costFunction weakParameterConstraint="off" class="org.openda.algorithms.SimulationKwadraticCostFunction" />
        //     <outerLoop maxIterations=10 absTolerance=0.01 relTolerance=0.01 />
        // </SimplexConfig>
        String configString = arguments[0];
        Results.putMessage("configstring = "+ configString);
        simplexConf = new ConfigTree(workingDir, configString);
        
        // Create costFunction
        Results.putMessage("costFunction@class="+simplexConf.getAsString("costFunction@class","SimulationKwadraticCostFunction"));
        if(simplexConf.getAsString("costFunction@class","SimulationKwadraticCostFunction").contains("SimulationKwadraticCostFunction")){
            this.J = new SimulationKwadraticCostFunction(stochModelFactory, stochObserver);
        }else{
        	throw new RuntimeException("Only implemented for one costfunction yet: org.openda.algorithms.SimulationKwadraticCostFunction");
        }
        // options for costFunctions
        J.addBackgroundTerm = simplexConf.getAsBoolean("costFunction@weakParameterConstraint",false);
        Results.putMessage("costFunction@weakParameterConstraint="+J.addBackgroundTerm);
        J.factor = simplexConf.getAsDouble("costFunction@factor",J.factor);
        Results.putMessage("costFunction@factor="+J.factor);

        // Initialize core optimizer
        this.simplexOptimizer = new SimplexCoreOptimizer(J);
        // options for optimizer
        //     <outerLoop maxIterations=10 absTolerance=0.01 relTolerance=0.01 />
        //outerloop
        this.simplexOptimizer.maxitSimplex = simplexConf.getAsInt("outerLoop@maxIterations", simplexOptimizer.maxitSimplex);
        Results.putMessage("outerLoop@maxIterations="+this.simplexOptimizer.maxitSimplex);
        this.simplexOptimizer.absTolSimplex =simplexConf.getAsDouble("outerLoop@absTolerance",this.simplexOptimizer.absTolSimplex);
        Results.putMessage("outerLoop@absTolerance="+this.simplexOptimizer.absTolSimplex);
        this.simplexOptimizer.relTolSimplex=simplexConf.getAsDouble("outerLoop@relTolerance",this.simplexOptimizer.relTolSimplex);
        Results.putMessage("outerLoop@relTolerance="+this.simplexOptimizer.relTolSimplex);
    }

    public void prepare() {

        //create initial simplex
        // Get initial parameters from model
        IStochModelInstance stochModelInstance = stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
        IVector initialParameters = stochModelInstance.getParameters();
        // initial uncertainty for parameters is used to start the optimization
        IStochVector parameterUncertainty = stochModelInstance.getParameterUncertainty();
        ISqrtCovariance L_par = parameterUncertainty.getSqrtCovariance();
        IVector[] searchDirections = L_par.asVectorArray();
        int p=searchDirections.length; //Number of independent directions in uncertainty; often equal to no pars
        IVector[] InitialSimplex = new IVector[p+1];
        InitialSimplex[0] = initialParameters.clone();
        for(int i=0;i<p;i++){
            InitialSimplex[i+1] = initialParameters.clone();
            InitialSimplex[i+1].axpy(initStep, searchDirections[i]);
        }
        // Initialize core optimizer
        this.simplexOptimizer.initialize(InitialSimplex);
        // options for optimizer
        //     <outerLoop maxIterations=10 absTolerance=0.01 relTolerance=0.01 />
        //outerloop
        this.simplexOptimizer.maxitSimplex = simplexConf.getAsInt("outerLoop@maxIterations", simplexOptimizer.maxitSimplex);
        Results.putMessage("outerLoop@maxIterations="+this.simplexOptimizer.maxitSimplex);
        this.simplexOptimizer.absTolSimplex =simplexConf.getAsDouble("outerLoop@absTolerance",this.simplexOptimizer.absTolSimplex);
        Results.putMessage("outerLoop@absTolerance="+this.simplexOptimizer.absTolSimplex);
        this.simplexOptimizer.relTolSimplex=simplexConf.getAsDouble("outerLoop@relTolerance",this.simplexOptimizer.relTolSimplex);
        Results.putMessage("outerLoop@relTolerance="+this.simplexOptimizer.relTolSimplex);
        // additional stopCriteria
        ConfigTree parts[] = simplexConf.getSubTrees("stopCriteria/stopCriterion");
        if (parts != null) {
            String className = null;
            double threshold;
            Class javaClass;
            Object object = null;
            for (ConfigTree part : parts) {
                className = part.getAsString("@class", null);
                threshold = part.getAsDouble("@threshold", this.simplexOptimizer.stopCritThresDefault);
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
                    this.simplexOptimizer.stopCriteria.add((IStopCriterion) object);
                    this.simplexOptimizer.stopCriteriaThreshold.add(threshold);
                } catch (ClassNotFoundException e) {
                    throw new RuntimeException("Could not create instance for " + className + ": " + e.getMessage());
                }
            }
//            this.simplexOptimizer.obsDescr = stochObserver.getObservationDescriptions();
            ITime selectionTimes = stochModelInstance.getTimeHorizon();
            this.simplexOptimizer.obsDescr = this.stochObserver.createSelection(selectionTimes).getObservationDescriptions();
        }
		stochModelInstance.finish();
	}


    public void run() {        
        // Call Simplex optimizer
    	while(this.hasNext()){
    		this.next();
    	}
    }

	/**
	 * Are there any more steps for this algorithm
	 * @return has next step
	 */
	public boolean hasNext(){
		return this.simplexOptimizer.hasNext();
	}
	
	/**
	 * Run next step of the algorithm
	 */
	public void next(){
		this.simplexOptimizer.next();
        this.bestEstimate = this.J.getBestModel();
	}

    public IModelState saveInternalState() {
        CalRestartSettings restartSettings = new CalRestartSettings("simplex");
        restartSettings.setComment("saved state for simplex algorithm");
        restartSettings.setParameters(this.simplexOptimizer.getCurrentValues());
        restartSettings.setCostValues(this.simplexOptimizer.getCurrentCosts());
        return restartSettings;
    }

    public void restoreInternalState(IModelState savedInternalState) {
        if (!(savedInternalState instanceof CalRestartSettings)) {
            throw new IllegalArgumentException("Unexpected saved State type: " + savedInternalState.getClass().getName());
        }
        CalRestartSettings restartSettings = (CalRestartSettings) savedInternalState;
        this.simplexOptimizer.initialize(restartSettings.getParameters(),
                restartSettings.getCosts());
    }

	public void releaseInternalState(IModelState savedInternalState) {
		if (!(savedInternalState instanceof CalRestartSettings)) {
			throw new IllegalArgumentException("Unexpected saved State type: " + savedInternalState.getClass().getName());
		}
		// no action needed
	}

	public IModelState loadPersistentState(File algorithmStateFile) {
		return new CalRestartSettings("simplex", algorithmStateFile);
	}

	public void finish() {
		// each created model instance has been finished individually (in prepare() and
		// in the cost function)
	}

	public IStochModelInstance getBestEstimate() {
        return bestEstimate;
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
