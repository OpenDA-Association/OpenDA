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

import org.openda.interfaces.*;
import org.openda.utils.Instance;
import org.openda.utils.Results;
import org.openda.utils.ConfigTree;
import org.openda.utils.Vector;
import org.openda.utils.io.CalRestartSettings;

import java.io.File;
import java.util.ArrayList;
import java.util.List;


/**
 * OpenDA Levenberg-Marquardt algorithm.
 */
public class LM extends Instance implements IAlgorithm {

	protected File workingDir=null;
	protected String configString=null;
	protected IStochModelFactory stochModelFactory;
	
	ConfigTree configtree = null;
	protected SimulationKwadraticCostFunction J=null;
	protected LMCoreOptimizer optimizer = null;
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
		
		Results.putProgression(this, "LM initialisation (stoch. obs. and stoch. model have been set)");
		
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
			String className = null;
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
				Results.putMessage(this, "observationFilters/observationFilter@class=" + className +
					", workingDirectory=" + obsFilterWorkingDir.toString() + ", configFile=" + arguments[0]);
				System.out.println("observationFilters/observationFilter@class=" + className +
					", workingDirectory=" + obsFilterWorkingDir + ", configFile=" + arguments[0]);
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
		this.optimizer = new LMCoreOptimizer(J);

		// options for optimizer
		/*
		 *	<outerLoop maxIterations="20" absTolGrad="0.01" relTolGrad="0.01" absTolStep="0.01" relTolStep="0.01"/>
		 *	<innerLoop levenbergParam="0.001" marquardtParam="10" maxIterations="5"/> <!-- here innerLoop's maxIterations is maximum unsuccessful downhill steps-->
	     *  <forwardDifference eps="[0.01]"/> <!-- vector of deltaParam used in the forward difference calculation of the Jacobian matrix. The order should be consistent with the order of the parameter vector specification in the stochModelConfig.xml-->
		 */
		//outerloop
		this.optimizer.maxit = configtree.getAsInt("outerLoop@maxIterations", optimizer.maxit);
		Results.putMessage(this, "outerLoop@maxIterations="+this.optimizer.maxit);
		this.optimizer.absTol= configtree.getAsDouble("outerLoop@absTolCostDiff", this.optimizer.absTol);
		Results.putMessage(this, "outerLoop@absTolCostDiff="+this.optimizer.absTol);
		this.optimizer.relTol= configtree.getAsDouble("outerLoop@relTolCostDiff",this.optimizer.relTol);
		Results.putMessage(this, "outerLoop@relTolCostDiff="+this.optimizer.relTol);
		this.optimizer.absTolGrad= configtree.getAsDouble("outerLoop@absTolGrad",this.optimizer.absTolGrad);
		Results.putMessage(this, "outerLoop@absTolGrad="+this.optimizer.absTolGrad);
		this.optimizer.relTolLinCost = configtree.getAsDouble("outerLoop@relToleranceLinearCost",this.optimizer.relTolLinCost);
		Results.putMessage(this, "outerLoop@relToleranceLinearCost="+this.optimizer.relTolLinCost);
		this.optimizer.computeStateJacobian = configtree.getAsBoolean("outerLoop@computeStateError", this.optimizer.computeStateJacobian);
		Results.putMessage(this, "outerLoop@computeStateJacobian="+this.optimizer.computeStateJacobian);
		//innerloop
		this.optimizer.maxInnerIter = configtree.getAsInt("innerLoop@maxIterations", optimizer.maxInnerIter);
		Results.putMessage(this, "innerLoop@maxIterations="+this.optimizer.maxInnerIter);
		//LM parameter setting
		this.optimizer.lambda = configtree.getAsDouble("innerLoop@levenbergParam",optimizer.lambda);
		Results.putMessage(this, "innerLoop@levenbergParam="+this.optimizer.lambda);
		this.optimizer.nu = configtree.getAsDouble("innerLoop@marquardtParam",optimizer.nu);
		Results.putMessage(this, "innerLoop@marquardtParam="+this.optimizer.nu);
		//forward difference parameter
		String forwardEps = configtree.getAsString("forwardDifference@eps","notSpecified");
		if (forwardEps.contains("notSpecified")){
			throw new RuntimeException("Forward difference parameter is not specified. Please check your algorithm configuration file.");
		}
		this.optimizer.fwdEps = new Vector(forwardEps);
		if (this.optimizer.fwdEps.getSize() != pInit.getSize()){
			throw new RuntimeException("The size of parameter vector in your stochModel xml file does not match the size of forward diff. parameter in the algorithm configuration file.");
		}
		Results.putMessage(this,"forwardDifference@eps="+this.optimizer.fwdEps.toString());

		// additional stopCriteria
		ConfigTree stopcrits[] = configtree.getSubTrees("stopCriteria/stopCriterion");
		if (stopcrits != null) {
			String className = null;
			double threshold;
			Class javaClass;
			Object object = null;
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
		CalRestartSettings restartSettings = new CalRestartSettings("LM");
		restartSettings.setComment("saved state for LM algorithm");
		//TODO
//		restartSettings.setParameters(this.optimizer.getCurrentValues());
//		restartSettings.setCostValues(this.optimizer.getCurrentCosts());
//		restartSettings.setPredictions(this.optimizer.getPredCurrent());
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
//		this.optimizer.initialize(params, costs, preds);
	}

	public IModelState loadPersistentState(File algorithmStateFile) {
		return new CalRestartSettings("dud", algorithmStateFile);
	}

	public void prepare() {
        // intialize optimizer
		this.optimizer.initialize(pInit);
	}


}
