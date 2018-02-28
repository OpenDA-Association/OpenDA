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
import org.openda.utils.Instance;
import org.openda.utils.Results;
import org.openda.utils.ConfigTree;
import org.openda.utils.io.CalRestartSettings;

import java.io.File;
import java.util.ArrayList;
import java.util.List;


/**
 * Base class for OpenDA Dud and SparseDud algorithms
 */
public abstract class BaseDud extends Instance implements IAlgorithm {

	protected File workingDir=null;
	protected String configString=null;
	protected IStochModelFactory stochModelFactory;
	
	ConfigTree configtree = null;
	protected SimulationKwadraticCostFunction J=null;
	protected BaseDudCoreOptimizer optimizer = null;
	protected IVector pInit=null;
	protected IStochObserver stochObserver;
	
	//output
	protected IStochModelInstance bestEstimate = null;
	
	
	public void initialize(File workingDir, String[] arguments) {
		this.workingDir = workingDir;
		this.configString = arguments[0];
	}

	/**
	* Return the names of the calibration-parameters of the model
	*
	* @return A list with the names of the calibration-parameters
	*/
	protected ArrayList<String> getParameterNames() {
		// Get initial parameters from model
		Results.putProgression(this, "Retrieving initial parameters from model");
		IStochModelInstance stochModelInstance = stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
		IVector paramVector = stochModelInstance.getParameters();
		if (! (paramVector instanceof ITreeVector)) {
			throw new RuntimeException(
				"This model is not suitable for the DuD method.\n"+
				"Its parameters should be organized in an OpenDA-TreeVector-object\n"+
				"That is an object in which the id-codes of the parameters are preserved\n"+
				"Your model seems to have organized the parameters in a different Vector-type\n");
		}
		ITreeVector paramITreeVecor = (ITreeVector) paramVector;
		stochModelInstance.finish();
		return paramITreeVecor.getSubTreeVectorIds();
	}

	protected abstract BaseDudCoreOptimizer InitializeDudCoreOptimizer();
 
	/**
	* Return the id-codes of the observations
	*
	* @return The (string) identifiers of the observations
	*/
	protected String[] getObservationIds() {
		int npred = stochObserver.getCount();
		IObservationDescriptions observationDescriptions = stochObserver.getObservationDescriptions();
		String[] observationLocations  = observationDescriptions.getStringProperties("location");
		String[] observationQuantities = observationDescriptions.getStringProperties("quantity");
		String[] observationIds = new String [npred];
		for(int i=0; i<npred; i++) {
			observationIds[i]= observationLocations[i]+"."+observationQuantities[i];
		}

		String prevnam = "";
		int iprev = -1;
		for(int i=0; i<npred; i++) {
			if (! observationIds[i].equals(prevnam)) {
				Results.putProgression("predictions "+iprev+":"+(i-1)+": "+prevnam);
				prevnam = observationIds[i];
				iprev = i;
			}
		}
		Results.putProgression("predictions "+iprev+":"+(npred-1)+": "+prevnam);

		return observationIds;
	}

	public void setStochComponents(IStochObserver stochObserver, IStochModelFactory stochModelFactory) {
		this.stochObserver = stochObserver;
		this.stochModelFactory = stochModelFactory;
		
		Results.putProgression(this, "Dud initialisation (stoch. obs. and stoch. model have been set)");
		
		//parse and store config
		Results.putMessage(this, "configString = "+ configString);
		configtree = new ConfigTree(workingDir, configString);

		// Get initial parameters from model
		Results.putProgression(this, "Retrieving initial parameters from model");
		IStochModelInstance stochModelInstance = stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
		pInit = stochModelInstance.getParameters();

		Results.putProgression(this, "Starting optimizer");
		// Create costFunction
		// <costFunction weakParameterConstraint="off" class="org.openda.algorithms.SimulationKwadraticCostFunction"
		//     factor="0.5" />
		Results.putMessage(this, "costFunction@class="+ configtree.getAsString("costFunction@class","SimulationKwadraticCostFunction"));
		if(configtree.getAsString("costFunction@class","SimulationKwadraticCostFunction").contains("SimulationKwadraticCostFunction")){
			this.J = new SimulationKwadraticCostFunction(stochModelFactory,this.stochObserver);
		} else {
			throw new RuntimeException("Only implemented for one costfunction yet: org.openda.algorithms.SimulationKwadraticCostFunction");
		}

		// options for costFunctions
		J.addBackgroundTerm = configtree.getAsBoolean("costFunction@weakParameterConstraint",false);
		Results.putMessage(this, "costFunction@weakParameterConstraint="+J.addBackgroundTerm);
		J.factor = configtree.getAsDouble("costFunction@factor",J.factor);
		Results.putMessage(this, "costFunction@factor="+J.factor);
		String logFilePrefix = configtree.getAsString("costFunction@logFile", null);
		if (logFilePrefix != null) {
			J.setDebugFilePathPrefix(new File(workingDir, logFilePrefix).getAbsolutePath());
		}
		// ignore error in mean
		J.biasRemoval = configtree.getAsBoolean("costFunction@biasRemoval",J.biasRemoval);
		Results.putMessage(this, "costFunction@biasRemoval="+J.biasRemoval);
		// ignore temporal variation in errors
		J.stdRemoval = configtree.getAsBoolean("costFunction@stdRemoval",J.stdRemoval);
		Results.putMessage(this, "costFunction@stdRemoval="+J.stdRemoval);
		// Option to try parallel evaluation of runs
		J.setTryParallel(configtree.getAsBoolean("costFunction@tryParallel",false));
		Results.putMessage(this, "costFunction@tryParallel="+J.getTryParallel());

		// observation selection/filter
		ConfigTree filters[] = configtree.getSubTrees("observationFilters/observationFilter");
		if (filters != null) {
			List<IObservationSpaceFilter> obsFilters = new ArrayList<IObservationSpaceFilter>();
			List<File> obsFilterWorkingDirectory = new ArrayList<File>();
			List<String[]> obsFilterArguments = new ArrayList<String[]>();
			String obsFilterString = null;
			String className;
			File obsFilterWorkingDir = null;
			String[] arguments;
			Class javaClass;
			Object object = null;
			for (ConfigTree filter : filters) {
				className = filter.getAsString("@class", null);
				String temp = filter.getAsString("workingDirectory", obsFilterString);
				if (temp!=null) obsFilterWorkingDir = new File(this.workingDir,temp);
				String obsFilterConfigFile = filter.getAsString("configFile", obsFilterString);
				String obsFilterDebugFile = filter.getAsString("debugFile", null);
				arguments = obsFilterDebugFile != null ?
					new String[]{obsFilterConfigFile, obsFilterDebugFile} :
					new String[]{obsFilterConfigFile};
				String message = "observationFilters/observationFilter@class=" + className +
						", workingDirectory=" +
						(obsFilterWorkingDir != null ? obsFilterWorkingDir.toString() : "(not set)") +
						", configFile=" + arguments[0];
				Results.putMessage(this, message);
				System.out.println(message);
				try {
					javaClass = Class.forName(className);
					try {
						object = javaClass.newInstance();
					} catch (InstantiationException e) {
						e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
					} catch (IllegalAccessException e) {
						e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
					}
					obsFilters.add((IObservationSpaceFilter) object);
					obsFilterWorkingDirectory.add(obsFilterWorkingDir);
					obsFilterArguments.add(arguments);
				} catch (ClassNotFoundException e) {
					throw new RuntimeException("Could not create instance for " + className + ": " + e.getMessage());
				}
			}
			initializeObservationFilters(obsFilters, obsFilterWorkingDirectory, obsFilterArguments);
			this.J.setListObservationFilter(obsFilters);
			this.stochObserver=this.J.runObservationFilters(pInit); // to initialize obsSelection
		}

		// create optimizer
		this.optimizer = InitializeDudCoreOptimizer();

		// options for optimizer
		/*
		 *	   <outerLoop maxIterations=10 absTolerance=0.01 relTolerance=0.01 relToleranceLinearCost=0.01 />
		 *	   <lineSearch maxIterations=5 maxRelStepSize=10.0 >
		 *	      <backtracking shorteningFactor=0.5 startIterationNegativeLook=3 />
		 */
		//outerloop
		this.optimizer.maxit = configtree.getAsInt("outerLoop@maxIterations", optimizer.maxit);
		Results.putMessage(this, "outerLoop@maxIterations="+this.optimizer.maxit);
		this.optimizer.absTol= configtree.getAsDouble("outerLoop@absTolerance",this.optimizer.absTol);
		Results.putMessage(this, "outerLoop@absTolerance="+this.optimizer.absTol);
		this.optimizer.relTol= configtree.getAsDouble("outerLoop@relTolerance",this.optimizer.relTol);
		Results.putMessage(this, "outerLoop@relTolerance="+this.optimizer.relTol);
		this.optimizer.relTolLinCost = configtree.getAsDouble("outerLoop@relToleranceLinearCost",this.optimizer.relTolLinCost);
		Results.putMessage(this, "outerLoop@relToleranceLinearCost="+this.optimizer.relTolLinCost);
		//linesearch
		this.optimizer.maxInnerIter = configtree.getAsInt("lineSearch@maxIterations", optimizer.maxInnerIter);
		Results.putMessage(this, "lineSearch@maxIterations="+this.optimizer.maxInnerIter);
		this.optimizer.maxStep = configtree.getAsDouble("lineSearch@maxRelStepSize", optimizer.maxStep);
		Results.putMessage(this, "lineSearch@maxRelStepSize="+this.optimizer.maxStep);
		this.optimizer.minInnerNegativeLook = configtree.getAsInt("lineSearch/backtracking@startIterationNegativeLook", optimizer.minInnerNegativeLook);
		Results.putMessage(this, "lineSearch/backtracking@startIterationNegativeLook="+this.optimizer.minInnerNegativeLook);
		this.optimizer.innerScaleFac = configtree.getAsDouble("lineSearch/backtracking@shorteningFactor", optimizer.innerScaleFac);
		Results.putMessage(this, "lineSearch/backtracking@shorteningFactor="+this.optimizer.innerScaleFac);
		//constraints on parameter values
		this.optimizer.add_constraints = configtree.getAsBoolean("constraints@parameterConstraint",optimizer.add_constraints);
		Results.putMessage(this, "constraints@parameterConstraint="+this.optimizer.add_constraints);
		this.optimizer.lower = configtree.getAsString("constraints/lowerbounds@bounds", optimizer.lower);
		Results.putMessage(this, "constraints/lowerbounds@Lbounds="+this.optimizer.lower);
		this.optimizer.upper = configtree.getAsString("constraints/upperbounds@bounds", optimizer.upper);
		Results.putMessage(this, "constraints/upperbounds@Ubounds="+this.optimizer.upper);
				
		// additional stopCriteria
		ConfigTree stopcrits[] = configtree.getSubTrees("stopCriteria/stopCriterion");
		if (stopcrits != null) {
			String className;
			double threshold;
			Class javaClass;
			Object object;
			for (ConfigTree stopcrit : stopcrits) {
				className = stopcrit.getAsString("@class", null);
				threshold = stopcrit.getAsDouble("@threshold", this.optimizer.stopCritThresDefault);
				Results.putMessage(this, "stopCriteria/stopCriterion@class=" + className + ", @threshold=" + threshold);
				try {
					javaClass = Class.forName(className);
					try {
						object = javaClass.newInstance();
					} catch (InstantiationException e) {
						throw new RuntimeException("Could not create instance for " + className + ": " + e.getMessage());
					} catch (IllegalAccessException e) {
						throw new RuntimeException("Error while creating instance for " + className + ": " + e.getMessage());
					}
					this.optimizer.stopCriteria.add((IStopCriterion) object);
					this.optimizer.stopCriteriaThreshold.add(threshold);
				} catch (ClassNotFoundException e) {
					throw new RuntimeException("Class " + className + "not found : " + e.getMessage());
				}
			}
			this.optimizer.obsDescr = this.stochObserver.getObservationDescriptions();
		}

		stochModelInstance.finish();

	}

    protected void initializeObservationFilters(List<IObservationSpaceFilter> obsFilters, List<File> obsFilterWorkingDirectory,List<String[]> obsFilterArguments) {
        int i = 0;
        for (IObservationSpaceFilter obsFilter : obsFilters){
            File configFile = obsFilterWorkingDirectory.get(i);
            String[] arguments = obsFilterArguments.get(i);
            obsFilter.initialize(configFile,arguments);
            i++;
        }
    }

    public void run() {        
        // Call Dud optimizer
    	while(this.hasNext()){
    		this.next();
    	}
    }

	/**
	 * Are there any more steps for this algorithm
	 * @return has next step
	 */
	public boolean hasNext(){
		return this.optimizer.hasNext();
	}
	
	/**
	 * Run next step of the algorithm
	 */
	public void next(){
		this.optimizer.next();
        this.bestEstimate = this.J.getBestModel();
		try {
			if (!this.hasNext() && this.bestEstimate != null && this.bestEstimate.getModelRunDir() != null) {
				Results.putMessage("Optimal results are in model run dir "+ this.bestEstimate.getModelRunDir().getAbsolutePath());
			}
		} catch (Exception e) {
			// no model run dir, no logging
		}
	}


	public IStochModelInstance getBestEstimate(){
    		return this.bestEstimate;
    	}

	public void finish() {
		// each created model instance has been finished individually
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

	public void releaseInternalState(IModelState savedInternalState) {
			throw new UnsupportedOperationException(
				  "org.openda.algorithms.SparseDud.releaseInternalState(): Not implemented yet.");
	}

	public IModelState saveInternalState() {
		CalRestartSettings restartSettings = new CalRestartSettings("dud");
		restartSettings.setComment("saved state for dud algorithm");
		restartSettings.setParameters(this.optimizer.getCurrentValues());
		restartSettings.setCostValues(this.optimizer.getCurrentCosts());
		restartSettings.setPredictions(this.optimizer.getPredCurrent());
		return restartSettings;
	}

	public void restoreInternalState(IModelState savedInternalState) {
		if (!(savedInternalState instanceof CalRestartSettings)) {
			throw new IllegalArgumentException(
			"Unexpected saved State type: " + savedInternalState.getClass().getName());
		}
		CalRestartSettings restartSettings = (CalRestartSettings) savedInternalState;
		IVector[] params = restartSettings.getParameters();
		double[] costs = restartSettings.getCosts();
		IVector[] preds = restartSettings.getPredictions();
		this.optimizer.initialize(params, costs, preds);
	}

	public IModelState loadPersistentState(File algorithmStateFile) {
		return new CalRestartSettings("dud", algorithmStateFile);
	}

	public void prepare() {
        // initialize optimizer
		this.optimizer.initialize(pInit);
	}


}
