/* OpenDA v2.3.1 
* Copyright (c) 2016 OpenDA Association 
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
import org.openda.models.simpleModel.SimpleStochModelInstance;
import org.openda.utils.*;

import java.io.File;


/**
 * OpenDA simulation
 */
public class Simulation extends Instance implements IAlgorithm {
    private IStochModelInstance bestEstimate = null;
    //config
    private IStochObserver stochObserver;
    private IStochModelFactory stochModelFactory;
    //config
    int repeat=1;
    private int current=0;
    boolean stochParameter=false;
    boolean stochForcing=false;
    String observationFile="";
    String obsFileFormat="csv";
    boolean addNoiseToObs=false;

    private File workingDir;
    private String configString;

    public void initialize(File workingDir, String[] arguments) {
        this.workingDir = workingDir;
        configString = arguments[0];
    }

    public void setStochComponents(IStochObserver stochObserver, IStochModelFactory stochModelFactory) {
        this.stochObserver = stochObserver;
        this.stochModelFactory = stochModelFactory;
        //parse config
        Results.putMessage("configstring = "+ configString);
        ConfigTree SimulationConf = new ConfigTree(this.workingDir, configString);
        
        //read config
        this.repeat = SimulationConf.getAsInt("instance@repeat",this.repeat);
        Results.putMessage("instance@repeat="+this.repeat);
        this.stochParameter = SimulationConf.getAsBoolean("instance@stochParameter",this.stochParameter);
        Results.putMessage("instance@stochParameter="+this.stochParameter);
        this.stochForcing = SimulationConf.getAsBoolean("instance@stochForcing",this.stochForcing);
        Results.putMessage("instance@stochForcing="+this.stochForcing);
        this.observationFile = SimulationConf.getAsString("writeObservations@filename",this.observationFile);
        Results.putMessage("writeObservations@filename="+this.observationFile);
        this.obsFileFormat = SimulationConf.getAsString("writeObservations@format",this.obsFileFormat);
        Results.putMessage("writeObservations@format="+this.obsFileFormat);
        if(this.obsFileFormat.compareToIgnoreCase("csv")!=0){
        	throw new RuntimeException("The given option observation generation: (writeObservations@format="+this.obsFileFormat+" is not support. At this time we only support csv. Can only write comma-separated-value format at this time.");
        }
        this.addNoiseToObs = SimulationConf.getAsBoolean("writeObservations@addNoise",this.addNoiseToObs);
        Results.putMessage("writeObservations@addNoise="+this.addNoiseToObs);
        
    }

    public void prepare() {
        // no action
    }
                                                                                                                                                                     
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
		return (this.current<this.repeat);
	}

    /**
	 * Run next step of the algorithm
	 */
	public void next(){
		int i=this.current;
		this.current++; //for next call
		if(i>this.repeat){throw new java.lang.RuntimeException("Attempt to run after end of algorithm");}
		Results.putProgression("starting model :"+i);
        IStochModelInstance model = this.stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
		this.bestEstimate = model; //all are equal, so last one is returned
		if(this.stochParameter){
			IVector pTry= model.getParameterUncertainty().createRealization();
			IVector p = model.getParameters(); //keep vector structure from model
			p.setValues(pTry.getValues());
			model.setParameters(p);
            Results.putValue("par", p, p.getSize(), "any", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Instance);
		}
		if(this.stochForcing){
			model.setAutomaticNoiseGeneration(true);
		}
		// select relevant observations and announce these
		ITime selectionTimes = model.getTimeHorizon();
		IStochObserver obsSelection = this.stochObserver.createSelection(selectionTimes);
    	IObservationDescriptions descr = obsSelection.getObservationDescriptions();
    	model.announceObservedValues(descr);
    	// run model
    	ITime startTime  = model.getTimeHorizon().getBeginTime();
        ITime targetTime = model.getTimeHorizon().getEndTime();
        
        Results.putProgression("========================================================================\n");
        Results.putProgression(" Forecast from "+Time.asDateString(startTime)+"  to "
                + Time.asDateString(targetTime) +"("+startTime.getMJD()+"-->"+targetTime.getMJD()+")\n");
        Results.putProgression("========================================================================\n");


  
        model.compute(targetTime);
    	// get results as Vectors
    	IVector prd      = model.getObservedValues(descr);
    	IVector obs      = obsSelection.getValues();
    	IVector obsStd   = obsSelection.getStandardDeviations();
    	// residuals obs-prd
    	IVector residuals= obs.clone();
    	residuals.axpy(-1.0, prd);
    	// print output
        Results.putValue("pred", prd, prd.getSize(), "forecast step", IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Instance);
        Results.putValue("obs", obs, obs.getSize(), "forecast step", IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Instance);
        Results.putValue("residual", residuals, residuals.getSize(), "forecast step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Instance);
        Results.putValue("std_obs", obsStd, obsStd.getSize(), "forecast step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Instance);

    	/*
    	 * generate observations
    	 */
    	if(descr instanceof CsvObservationDescriptions){
    		if((this.observationFile.length()>0)&(i==0)){ // write observations to file
    			// copy observer and possibly change type
    			String keys[] = descr.getPropertyKeys();
    			IVector values[] = new IVector[keys.length];
    			for(int j=0;j<keys.length;j++){
    				if(keys[j].compareToIgnoreCase("value")!=0){
    					values[j] = descr.getValueProperties(keys[j]);
    				}else{
    					if(!this.addNoiseToObs){
    						values[j] = prd; // replace observations with predictions
    					}else{
    						IStochVector genObsSv = new StochVector(prd,obsStd);
    						values[j] = genObsSv.createRealization(); 
    					}
    				}
    			}

    			//TODO --- handle other observer types ---
    			CsvStochObserver generatedObs = new CsvStochObserver(values,keys);
    			Results.putProgression("Writing generated observations to file.");
    			generatedObs.toFile(this.workingDir, this.observationFile);
    		}
    	}else{
    		Results.putMessage("Not writing generated observations because this observer type is not supported");
    	}
    	/*
    	 * write states
    	 * TODO temporary solution for generating additional output
    	 */
    	/**/
    	if(model instanceof SimpleStochModelInstance){
    		SimpleStochModelInstance simpleModel = (SimpleStochModelInstance) model;
    		java.util.Vector<IVector> xStore = simpleModel.xStore;
    		java.util.Vector<Double> tStore = simpleModel.tStore;
    		for(int j=0;j<xStore.size();j++){
                Results.putValue("model_time", tStore.elementAt(j), 1, "forecast step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Instance);
                Results.putValue("x", xStore.elementAt(j), 1, "forecast step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Instance);
    		}
    	}
		model.finish();
	}

    public IModelState saveInternalState() {
        throw new UnsupportedOperationException("org.openda.algorithms.Simulation.restoreInternalState(): Not implemented yet.");
    }

    public void restoreInternalState(IModelState savedInternalState) {
        throw new UnsupportedOperationException("org.openda.algorithms.Simulation.restoreInternalState(): Not implemented yet.");
    }

	public void releaseInternalState(IModelState savedInternalState) {
		throw new UnsupportedOperationException("org.openda.algorithms.Simulation.releaseInternalState(): Not implemented yet.");
	}

	public IModelState loadPersistentState(File algorithmStateFile) {
		throw new UnsupportedOperationException("org.openda.algorithms.Simulation.loadPersistentState(): Not implemented yet.");
	}

	public void finish() {
		// no action needed
	}

	public IStochModelInstance getBestEstimate(){
	    	return this.bestEstimate;
	}

	
	public IVector getState() {
		if(this.bestEstimate!=null){
		return this.bestEstimate.getState();
		}else{
			return null;
		}
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
