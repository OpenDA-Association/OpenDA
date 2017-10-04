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

import org.openda.blackbox.wrapper.BBStochModelInstance;
import org.openda.interfaces.*;
import org.openda.utils.*;

import java.io.File;
import java.util.ArrayList;

import org.openda.interfaces.IAlgorithm;

/**
 * OpenDA Meerpeilcorrection
 */
public class Meerpeilcorrection extends Instance implements IAlgorithm {

    private IStochModelInstance mainModel = null;
    //config
    private IStochObserver stochObserver;
	private IStochModelFactory stochModelFactory;

	private int maxSteps;
    private int currentStep;

    private File workingDir;

	private String dummy;

	private String id_drymask;

	public Meerpeilcorrection() {
		this.workingDir = null;
		this.currentStep = 0;
		this.maxSteps = 1;
	}

	public void initialize(File workingDir, String[] arguments) {
        this.workingDir = workingDir;
        String configString = arguments[0];

		// parse configuration file
		Results.putMessage("configstring = " + configString);
		ConfigTree configurationAsTree = new ConfigTree(workingDir, configString);

		// config for analysis times, only option allowed:
		// <analysisTimes type="fromObservationTimes" ></analysisTimes>
		String analysisTimesType = configurationAsTree.getAsString("analysisTimes@type", "fromObservationTimes");
		Results.putMessage("analysisTimes@type="+analysisTimesType);
		boolean stepsFromObserver = analysisTimesType.equals("fromObservationTimes");
		if (!stepsFromObserver){
			throw new java.lang.RuntimeException("Only <analysisTimes type=\"fromObservationTimes\"> allowed");
		}
		// if specified, set dummy value, otherwise use default for netcdf
		dummy = configurationAsTree.getAsString("dumval","no dumval");

		// if specified, set drymask value
		id_drymask = configurationAsTree.getAsString("id_drymask", "no id_drymask");
	}

	/**
	 * Initialization function, called after initialize and before prepare.
	 * Sets the observer and the model.
	 * @param stochObserver The stoch.observer
	 * @param stochModelFactory The stoch.model factory
	 */
    public void setStochComponents(IStochObserver stochObserver, IStochModelFactory stochModelFactory) {
        this.stochObserver = stochObserver;
		this.stochModelFactory = stochModelFactory;

		// Create MainModel
		Results.putProgression("Create model");
		this.mainModel = stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Debug);

		// Check that the observer contains exactly one observation
		int nobs = this.stochObserver.getCount();
		System.out.println("Number of observations = "+nobs);
	 	if (nobs != 1) {throw new RuntimeException("Algorithm Meerpeilcorrection allows exactly one analysisTime in OBSERVER");}
	}

	/**
	 * preparations for algorithm
	 */
    public void prepare() {

	}

	/**
	 * run whole algorithm
	 */
    public void run() {
        while(this.hasNext()){
        	this.next();
        }
		this.mainModel.finish();
    }

	/**
	 * Are there any more steps for this algorithm
	 * @return has next step
	 */
	public boolean hasNext(){
		return (this.currentStep<this.maxSteps);
	}

    /**
	 * Run next step of the algorithm
	 */
	public void next(){

	if (this.currentStep >= this.maxSteps) {
		throw new java.lang.RuntimeException("Attempt to run after end of algorithm");
	}
	// recompute waterlevel: find the ExchangeItem that contains the state and add
	// the correction to this item only.
	if (this.mainModel instanceof BBStochModelInstance) {
		BBStochModelInstance mainModel = (BBStochModelInstance) this.mainModel;
		ITreeVector state_treevector = mainModel.getState();
		ArrayList<String> state_ids = state_treevector.getSubTreeVectorIds();
         // Check if state_ids consist of only 1 ExchangeItem
        if (state_ids.size() != 1) {throw new RuntimeException("Algorithm Meerpeilcorrection allows exactly one state vector in StochModel");}

		String Exch_ids[] = this.mainModel.getExchangeItemIDs();

		// Read mask file if it exists
		IVector mask = new Vector();
		if (! id_drymask.equals("no id_drymask")) {
			IExchangeItem drymask = this.mainModel.getDataObjectExchangeItem(id_drymask);
			// Define vector mask containing the drymask values
			double[] val = ((Array) drymask.getValues()).getValuesAsDoubles();
			mask = new Vector(val);
		}

		for (String id1 : state_ids) {
			for (String id2 : Exch_ids) {
				if (id2.equalsIgnoreCase(id1)) {
					IVector state = this.mainModel.getState();
					int n = state.getSize();

					// Check if size drymask equals size state vector.
					if (! id_drymask.equals("no id_drymask") && mask.getSize()!=n) {throw new RuntimeException("Drymask and state vector are different in size");}

					// first (and only) observation is the correction factor
					IVector obs = this.stochObserver.getValues();
					double alpha = obs.getValue(0);

					Vector corr = new Vector(n);
					if (dummy.contentEquals("no dumval")) {
						// add meerpeilcorrection to all positions
						corr.setConstant(alpha);
					} else {
						// only add meerpeilcorrection to non-dummy values
						double dumval = Double.valueOf(dummy);
						for (int i = 0; i < n; i++) {
							if (Math.abs (state.getValue(i) - dumval) > Double.MIN_VALUE) {
								corr.setValue(i,alpha);
							} else {
								corr.setValue(i,0.0);
							}
						}
					}

					if (id_drymask != "no id_drymask") {
						// apply meerpeil correction only in cells where mask == 1, and not when mask == 0
						corr.pointwiseMultiply(mask);
					}

					// add: state = state + corr
					state.axpy(1.0,corr);

					// Reset values of ExchangeItem containing the state
					this.mainModel.getDataObjectExchangeItem(id2).setValues(state);
					Results.putProgression("==================================================");
					if (id_drymask != "no id_drymask") {
						Results.putProgression(" Meerpeilcorrection " + alpha + " applied with mask option.");
					} else {
						Results.putProgression(" Meerpeilcorrection " + alpha + " applied");
					}
					Results.putProgression("==================================================\n");
				}
			}
		}
	} else {
		throw new RuntimeException("Algorithm meerpeilcorrection not yet tested for non BB-configurations");
	}

	ITime targetTime = this.mainModel.getTimeHorizon().getEndTime();
	this.mainModel.compute(targetTime);

	this.currentStep++;
	}

    public IModelState saveInternalState() {
        throw new UnsupportedOperationException("org.openda.algorithms.Meerpeilcorrection.saveInternalState(): Not implemented.");
    }

    public void restoreInternalState(IModelState savedInternalState) {
        throw new UnsupportedOperationException("org.openda.algorithms.Meerpeilcorrection.restoreInternalState(): Not implemented.");
    }

	public void releaseInternalState(IModelState savedInternalState) {
		throw new UnsupportedOperationException("org.openda.algorithms.Meerpeilcorrection.releaseInternalState(): Not implemented.");
	}

	public IModelState loadPersistentState(File algorithmStateFile) {
		throw new UnsupportedOperationException("org.openda.algorithms.Meerpeilcorrection.loadPersistentState(): Not implemented.");
	}

    /**
	 * Tell the algorithm that it will never be called again, so it can perform its finalization actions
	 */
	public void finish() {
		if (this.mainModel != null) {
			mainModel.finish();
			mainModel = null;
		}
	}

	public IVector getState(int iDomain) {
		if(this.mainModel!=null){
			return this.mainModel.getState(iDomain);
		}else{
			return null;
		}
	}

	/**
	 * Get the full set of state variables from a model.
	 *
	 * @return A Vector containing the values of all state variables in the model.
	 */
	public IVector getState() {
		if(this.mainModel!=null){
			return this.mainModel.getState();
		}else{
			return null;
		}
	}

	/**
	 * Get the computational time horizon of the model (begin and end time).
	 *
	 * @return The time horizon (containing begin and end time).
	 */
	public ITime getTimeHorizon() {
		return this.mainModel.getTimeHorizon();
	}

	/**
	 * Get the stochastic model instance's current simulation time stamp.
	 *
	 * @return The model's current simulation time stamp.
	 */
	public ITime getCurrentTime() {
		return this.mainModel.getCurrentTime();
	}

	public void compute(ITime targetTime) {
		throw new UnsupportedOperationException("method compute not implemented."+this.getClass().getName());
	}

}
