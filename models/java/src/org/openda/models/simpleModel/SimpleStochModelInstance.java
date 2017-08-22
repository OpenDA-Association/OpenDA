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
package org.openda.models.simpleModel;


import org.openda.interfaces.*;
import org.openda.interfaces.IStochModelFactory.OutputLevel;
import org.openda.observers.TimeSeriesObservationDescriptions;
import org.openda.utils.*;
import org.openda.utils.io.FileBasedModelState;

import java.io.*;
import java.util.Arrays;


/**
 * Abstract class that implements part of the StochModelInstance interface based
 * on some assumptions about the data structures used. This class can be extended
 * to construct a toy model with little effort
 *
 * Assumes the following structure for the input file:
 * <?xml version="1.0" encoding="UTF-8"?>
 * <oscillatorConfig>
 *    <simulationTimespan>[0.0,0.05,10.0]</simulationTimespan>
 *    <!-- parameters for the model -->
 *    <parameters names="t_damp,omega">[8.0,1.5708]</parameters>
 *    <!-- uncertain parameters (for calibration) and their uncertainty -->
 *    <parameterUncertainty names="t_damp,omega">[1.0,0.1257]</parameterUncertainty>
 *    <!-- system noise for each state variable with bias and std per time-unit -->
 *    <systemNoise>{[0.0,0.0],[0.3,0.3]}</systemNoise>
 *    <!-- state vactor at initial time -->
 *    <initialState>[0.8,0.0]</initialState>
 *    <!-- std of uncertainty of state vector at initial time -->
 *    <initialStateUncertainty>[0.8,0.8]</initialStateUncertainty>
 * </oscillatorConfig>
 */
public abstract class SimpleStochModelInstance extends Instance implements IStochModelInstance, IModelAdjoint{

	//
	// parameters of the model
	//   that can be used as control variables with their statistics
	//
	protected java.util.Hashtable<String,Double> pars=new java.util.Hashtable<String,Double>();

	// Internal state of the model
	protected Vector state=null;
	protected double t; //current time
	protected int timeStep; //integer value is used for testing equality and counting

	// Parameter used for calibration
	protected java.util.Vector<String> stochParNames= new java.util.Vector<String>();
	protected StochVector stochPars=null;


	// System noise for Kalman filtering
	protected StochVector sysNoiseIntensity=null;
	protected boolean autoNoise = false;
	protected IStochVector initialStateUncertainty=null;

	// Counter for keeping track of instances
	protected static int NextinstanceNumber = 1;
	protected int thisInstanceNumber=0;

	// Output storage
	// TODO make protected again when there is another mechanism to organize the output
	public boolean storeObs=false;
	public java.util.Vector<Double> tStore = new java.util.Vector<Double>();
	public java.util.Vector<Integer> iStore = new java.util.Vector<Integer>();
	public java.util.Vector<IVector> xStore = new java.util.Vector<IVector>();
	public OutputLevel outputLevel = OutputLevel.ModelDefault;

	// Configuration
	protected File workingDir=null;
	protected String configString =null;
	protected ConfigTree conf = null;
	protected boolean saveStateToFile=false;

	public void Initialize(File workingDir, String configString){
		//
		//  GENERIC PART
		//

		// Instance counter
		this.thisInstanceNumber = NextinstanceNumber;
		NextinstanceNumber++;

		// configuration
		this.workingDir = workingDir;
		this.configString = configString;

		/*
		 * now parse configuration
		 */
		/*<?xml version="1.0" encoding="UTF-8"?>
		 * <oscillatorConfig>
		 * <simulationTimespan>[0.0,0.05,10.0]</simulationTimespan>
		 * <parameters names="t_damp,omega">[8.0,1.5708]</parameters>
		 * <parameterUncertainty names="t_damp,omega">[1.0,0.1257]</parameterUncertainty>
		 * <systemNoise>{[0.0,0.0],[0.3,0.3]}</systemNoise>
		 * <initialState>[0.8,0.0]</initialState>
		 * <initialStateUncertainty>[0.8,0.8]</initialStateUncertainty>
		 * </oscillatorConfig>
		 */
		Results.putMessage("configstring = "+ configString);
		this.conf = new ConfigTree(workingDir, configString);

		/*
		 * parse simulationTimespan
		 */
		// <simulationTimespan>[0.0,0.05,10.0]</simulationTimespan>
		String timespanString = conf.getContentString("simulationTimespan");
		if(timespanString!=null){
			IVector timespanVector = new Vector(timespanString);
			pars.put("t_start",timespanVector.getValue(0));
			pars.put("t_step",timespanVector.getValue(1));
			pars.put("t_stop",timespanVector.getValue(2));
			Results.putMessage("simulationTimespan ="+timespanVector);
		}
		// start at initial time
		this.t = pars.get("t_start");
		this.timeStep = 0; //counter starts at 0


		/*
		 *  parse parameters
		 */
		String nameString = conf.getAsString("parameters@names", "");
		String parString = conf.getContentString("parameters");
		if(parString!=null){
			IVector parValues = new Vector(parString);
			Results.putMessage("parameters@names ="+nameString+" parameters="+parValues);
			String[] names = nameString.split(",");
			for(int i=0;i<names.length;i++){
				if(this.pars.containsKey(names[i])){
					this.pars.put(names[i], parValues.getValue(i));
				}else{
					Results.putMessage("parameter "+names[i]+" was not recognized. Value was ignored.");
				}
			}
		}
		/*
		 *  parse parameterUncertainty
		 */
		// <parameterUncertainty names="t_damp,omega">[1.0,0.1257]</parameterUncertainty>
		nameString = conf.getAsString("parameterUncertainty@names", "");
		parString = conf.getContentString("parameterUncertainty");
		if(parString!=null){
			IVector parValues = new Vector(parString);
			Results.putMessage("parameterUncertainty@names ="+nameString+" parameterUncertainty="+parValues);
			String[] names = nameString.split(",");
			this.stochParNames = new java.util.Vector<String>();
			this.stochParNames.addAll(Arrays.asList(names));
			IVector stdVector = new Vector(names.length);
			IVector meanVector = new Vector(names.length);
			for(int i=0;i<names.length;i++){ // collect mean values
				if(this.pars.containsKey(names[i])){
					stdVector.setValue(i,parValues.getValue(i));
					meanVector.setValue(i,this.pars.get(names[i]));
				}else{
					Results.putMessage("parameter "+names[i]+" was not recognized. Value was ignored.");
					stdVector.setValue(i, 0.0); // no uncertainty
					meanVector.setValue(i, 0.0); // no value
				}
			}
			this.stochPars = new StochVector(meanVector,stdVector);
		}
		/*
		 *  parse systemnoise
		 */
		// <systemNoise>{[0.0,0.0],[0.3,0.3]}</systemNoise>
		String systemNoiseString = conf.getContentString("systemNoise");
		if(systemNoiseString!=null){
			this.sysNoiseIntensity = new StochVector(systemNoiseString);
			Results.putMessage("systemNoise = "+this.sysNoiseIntensity);
		}
		/*
		 *  parse state and its uncertainty
		 */
		// <initialState>[0.8,0.0]</initialState>
		// <initialStateUncertainty>[0.8,0.8]</initialStateUncertainty>
		String initialStateString = conf.getContentString("initialState");
		String initialStateStdString = conf.getContentString("initialStateUncertainty");
		if(initialStateString!=null){
			this.state = new Vector(initialStateString);
			Results.putMessage("initialState = "+this.state);
			if(initialStateStdString!=null){
				IVector std = new Vector(initialStateStdString);
				Results.putMessage("initialStateUncertainty = "+std);
				this.initialStateUncertainty = new StochVector(new Vector(std.getSize()),std);
			}
		}
	}

	public void initialize(File workingDir, String[] arguments) {
		// no action needed (handled by constructors)
	}


	public ITime getTimeHorizon() {
		Time timeHorizon = new Time(this.pars.get("t_start"), this.pars.get("t_stop"));
		timeHorizon.setStep(this.pars.get("t_step"));
		return timeHorizon;
	}

	public ITime getCurrentTime() {
		return new Time(this.t);
	}

	public IVector getState() {
		return this.state.clone();
	}

	public void axpyOnState(double alpha, IVector vector) {
		this.state.axpy(alpha, vector); // nothing special for this model
		if(this.storeObs){ // if states are stored, adjust last stored state
			this.xStore.get(this.xStore.size() - 1).setValues(state.getValues());
		}
	}

	public IVector getParameters() {
		int npar = this.stochParNames.size();
		String[] names = new String[npar];
		double[] values = new double[npar];
		for(int i=0;i<npar;i++){
			names[i] =  stochParNames.get(i);
			values[i] =  pars.get(names[i]);
		}
		return new TreeVector("parameters",names,values);
	}

	public void setParameters(IVector parameters) {
		int n = parameters.getSize();
		if(n!=stochParNames.size())
			throw new java.lang.RuntimeException("setParameters size mismatch");
		for(int i=0;i<this.stochParNames.size();i++){
			// check validity of parameters omega>0 and t_damp>0;
			pars.put(stochParNames.get(i),Math.max(parameters.getValue(i),0.0));
		}

	}

	public void axpyOnParameters(double alpha, IVector vector) {
		IVector temp = this.getParameters();
		temp.axpy(alpha, vector); //do axpy on parameters as explicit Vector-object
		this.setParameters(temp);
		// nothing special in this model
	}

	/**
	 * Compute time derivative of state at current time.
	 * This is used by the time-integration "mod.compute(t)" as the core of the model.
	 * @param xt The current state
	 * @param t Current time
	 * @return vector dt
	 */
	protected abstract IVector dx(IVector xt,double t);

	public void compute(ITime targetTime) {
		double t_step = this.pars.get("t_step");
		int nsteps = (int) Math.round( (targetTime.getMJD()-this.t)/t_step );
		IVector dxdt0;
		IVector dxdt1;
		IVector dxdt2;
		IVector dxdt3;
		IVector x = this.state;
		IVector xn;
		for(int i=0;i<nsteps;i++){
			// --> Runge-Kutta
			//System.out.print("step :"+i+" ");
			// dx0 = dx(x,t);
			dxdt0 = dx(x,t);
			// dx1 = dx(x+0.5*dx0,t+0.5*dt);
			xn = x.clone();
			xn.axpy(0.5*t_step, dxdt0);
			dxdt1 = dx(xn,t+0.5*t_step);
			// dx2 = dx(t+0.5*dt,x+0.5*dx1);
			xn = x.clone();
			xn.axpy(0.5*t_step, dxdt1);
			dxdt2 = dx(xn,t+0.5*t_step);
			// dx3 = dx(t+1.0*dt,x+1.0*dx2);
			xn = x.clone();
			xn.axpy(1.0*t_step, dxdt2);
			dxdt3 = dx(xn,t+0.5*t_step);

			//x   = x + 1/6*dt*(dx0+2*dx1+2*dx2+dx3);
			x.axpy(1.0/6.0*t_step,dxdt0);
			x.axpy(2.0/6.0*t_step,dxdt1);
			x.axpy(2.0/6.0*t_step,dxdt2);
			x.axpy(1.0/6.0*t_step,dxdt3);

			// add system noise
			if(this.autoNoise){
				IVector w = this.sysNoiseIntensity.createRealization();
				//Results.putProgression("> w="+w+" sqrt(t_step)="+Math.sqrt(t_step));
				x.axpy(Math.sqrt(t_step), w);
			}

			this.t+=t_step;
			this.timeStep++;
			//System.out.println(">>>>>>> t="+t+" x="+x);
			if(this.storeObs){ //store all states if this is requested
				this.tStore.add(this.t);
				this.xStore.add(x.clone());
				this.iStore.add(this.timeStep);
			}
			if(this.outputLevel!=OutputLevel.Suppress){
                Results.putValue("model_time", this.t, 1, "any", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
                Results.putValue("x", x, x.getSize(), "any", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
			}
		}
		this.state.setValues(x.getValues()); //TODO this probably does nothing -> CHECK!
	}

	public IStochVector getStateUncertainty() {
		return this.initialStateUncertainty;
	}

	public IStochVector getParameterUncertainty() {
		return this.stochPars;
	}

	public IStochVector[] getWhiteNoiseUncertainty(ITime time) {
		return new IStochVector[]{this.sysNoiseIntensity};
	}

	public boolean isWhiteNoiseStationary() {
		return true;
	}

	public ITime[] getWhiteNoiseTimes(ITime timeSpan) {
		throw new UnsupportedOperationException("org.openda.models.oscillator.OscillatorStochModelInstance.getWhiteNoiseTimes(): Not implemented yet.");
	}

	public IVector[] getWhiteNoise(ITime timeSpan) {
		throw new UnsupportedOperationException("org.openda.models.oscillator.OscillatorStochModelInstance.getWhiteNoise(): Not implemented yet.");
	}

	public void setWhiteNoise(IVector whiteNoise[]) {
		throw new UnsupportedOperationException("org.openda.models.oscillator.OscillatorStochModelInstance.setWhiteNoise(): Not implemented yet.");
	}

	public void axpyOnWhiteNoise(double alpha, IVector vector[]) {
		throw new UnsupportedOperationException("org.openda.models.oscillator.OscillatorStochModelInstance.axpyOnWhiteNoise(): Not implemented yet.");
	}

	public void setAutomaticNoiseGeneration(boolean value) {
		this.autoNoise = value;
	}


	public class savedState implements IModelState {
		double time=-1;
		int timestep=-1;
		Vector state=null;

		public void savePersistentState(File file) {
			try {
				FileWriter out = new FileWriter(file);
				// save state vector
				this.state.maxFullExpandLength = this.state.getSize()+1;
				String line="x="+this.state.toString();
				out.write(line+"\n");
				// save time
				line = "t="+this.time;
				out.write(line+"\n");
				// save timestep
				line = "timestep="+this.timestep;
				out.write(line+"\n");
				out.close();
			} catch (IOException e) {
				throw new RuntimeException("Could not create state file " + file.getAbsolutePath());
			}
		}
	}

	public IModelState saveInternalState() {
		savedState saveState=new savedState();
		saveState.time = this.t;
		saveState.timestep = this.timeStep;
		saveState.state = this.state.clone();
		return saveState;
	}

	public void restoreInternalState(IModelState savedInternalState) {
		savedState saveState = (savedState) savedInternalState;
		this.state = saveState.state.clone();
		this.timeStep = saveState.timestep;
		this.t = saveState.time;
		if(storeObs) {
			// removed old stored states
			this.tStore.clear();  //Get rid of old storage
			this.xStore.clear();
			this.iStore.clear();
			this.storeObs=false;
		}
	}

	public void releaseInternalState(IModelState savedInternalState) {
		if(this.saveStateToFile){
			FileBasedModelState modelState = (FileBasedModelState) savedInternalState;
			modelState.releaseState(this.workingDir);
		}else{
			savedState saveState = (savedState) savedInternalState;
			saveState.time = -1;
			saveState.timestep = -1;
			saveState.state = null;
		}
	}

	public IModelState loadPersistentState(File persistentStateFile) {
		Vector x=null;
		double t=0.0;
		int timestep=0;
		try {
			if(!persistentStateFile.exists()){
				throw new RuntimeException("Could not find file for saved state:"+ persistentStateFile.toString());
			}
			// read state and time from file
			FileReader fileReader = new FileReader(persistentStateFile);
			BufferedReader buff = new BufferedReader(fileReader);
			// Read and parse first line
			String line = buff.readLine();
			String[] keyValuePair = line.split("="); // expect eg x=[1.0,2.0,3.0]
			x = new Vector(keyValuePair[1]);
			// Read and parse second line
			line = buff.readLine();
			keyValuePair = line.split("="); // expect eg t=1.0
			t = Double.parseDouble(keyValuePair[1]);
			// Read and parse third line
			line = buff.readLine();
			keyValuePair = line.split("="); // expect eg timestep=3
			timestep = Integer.parseInt(keyValuePair[1]);
			buff.close();
		} catch (IOException e) {
			Results.putMessage("Exception: " + e.getMessage());
			throw new RuntimeException("Error reading restart file for model");
		}
		// set state and time
		savedState saveState=new savedState();
		saveState.time = t;
		saveState.timestep = timestep;
		saveState.state = x;
		return saveState;
	}

	public void finish() {
		// no action needed (yet)
	}


	public void printStoredStates(){
		System.out.println("--------------");
		for(int i=0;i<this.iStore.size();i++){
			System.out.println("t="+this.tStore.get(i)+" x="+this.xStore.get(i));
		}
		System.out.println("--------------");
	}


	public IVector getObservedValues(IObservationDescriptions descr) {
		IVector result = new Vector(descr.getObservationCount());
		IVector obsTimes = null;

		if(descr instanceof TimeSeriesObservationDescriptions){
			obsTimes=((TimeSeriesObservationDescriptions)descr).obs.getAllTimes();
		}else{
			obsTimes=descr.getValueProperties("time");
		}

		IVector obsIndex = descr.getValueProperties("index");
		if(obsIndex==null){
			obsIndex = descr.getValueProperties("xPosition");
		}

		IVector transformIndex;
		try {
		  transformIndex = descr.getValueProperties("transform");
	    } catch (RuntimeException e) {
			transformIndex = null;
		}

		Time tHor = new Time(this.pars.get("t_start"), this.pars.get("t_stop"));
		tHor.setStep(this.pars.get("t_step"));

		if(this.storeObs){ //work from stored states
			for(int i=0;i<descr.getObservationCount();i++){
				// at which timestep is this obs
				long iObs = tHor.getTimeStep(new Time(obsTimes.getValue(i)));
				// find corresponding storage location
				int thisTIndex = this.iStore.indexOf((int)iObs); //time index in storage
				if(thisTIndex<0){
					throw(new RuntimeException("model.getValues: time out of range for observation nr. "+i));
				}
				int thisXIndex = (int) obsIndex.getValue(i);
				if((thisXIndex<0)|(thisXIndex>=this.state.getSize())){
					throw(new RuntimeException("model.getValues: index out of range for "
							+" observation nr. "+i
							+" index= "+thisXIndex ));
				}
				//System.out.println("i="+i+" it="+thisTIndex+" ind= "+thisXIndex);
				double thisValue = this.xStore.get(thisTIndex).getValue(thisXIndex);
				// transform values if needed
				if(transformIndex!=null){
					if(transformIndex.getValue(i)==2){ // magic number for quadratic observations
						thisValue = thisValue*thisValue;
					}
				}
				result.setValue(i, thisValue);
			}
		}else{ // only current state is available
			for(int i=0;i<descr.getObservationCount();i++){
				int thisXIndex = (int) obsIndex.getValue(i);
				//TODO if(){ //check time
				double thisValue = this.state.getValue(thisXIndex);
				// transform values if needed
				if(transformIndex!=null){
					if(transformIndex.getValue(i)==2){ // magic number for quadratic observations
						thisValue = thisValue*thisValue;
					}
				}
				result.setValue(i, thisValue);
			}

		}

		return result;
	}

	public IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance){
		/* Return identity localization */
		// System.out.println("Return identity localization");
		int nObs = observationDescriptions.getObservationCount();
		IVector locvec = new Vector(this.getState().getSize());
		IVector[] result = new Vector[nObs];

		for (int iObs=0; iObs<nObs; iObs++){
			locvec.setConstant(1.0);
			result[iObs] = locvec;
		}
		return result;

		//throw new UnsupportedOperationException("org.openda.models.simpleModel.SimpleStochModelInstance.getObservedLocalization(): Not implemented yet.");
	}

	public void announceObservedValues(IObservationDescriptions observationDescriptions) {
		// simplistic: we store the whole state at each time!
		this.tStore.clear();  //Get rid of old storage
		this.xStore.clear();
		this.iStore.clear();
		this.storeObs=true;
		this.tStore.add(this.t);
		this.xStore.add(this.state.clone());
		this.iStore.add(this.timeStep);
		//TODO add check for indices
	}

	public IVector getStateScaling() {
		// no real scaling implemented,though we could use omega
		IVector result = new Vector(this.getState().getSize());
		result.setConstant(1.0);
		return result;
	}

	public IVector[] getStateScaling(IObservationDescriptions observationDescriptions) {
		int m=observationDescriptions.getObservationCount();
		// no real scaling implemented,Schur product makes no sense here!
		IVector result[] = new Vector[m];
		for(int i=0;i<m;i++){
			result[i] = new Vector(this.getState().getSize());
			result[i].setConstant(1.0);
		}
		return result;
	}

	public String toString(){
		String result = this.getClass().getName()+" "+this.thisInstanceNumber;
		result+= "\n   : t= "+this.getCurrentTime().toString();
		result+= "\n   : x= "+this.getState();
		result+= "\n   : p= "+new Vector(this.getParameters());
		return result;
	}

	/*
	 *
	 * Exchange items
	 *
	 */

	public String[] getExchangeItemIDs() {
		throw new UnsupportedOperationException("org.openda.models.simpleModel.SimpleStochModelInstance.getExchangeItemIDs(): Not implemented yet.");
	}

    public String[] getExchangeItemIDs(IPrevExchangeItem.Role role) {
        if (role == IPrevExchangeItem.Role.InOut) {
            return getExchangeItemIDs();
        }
        throw new UnsupportedOperationException("org.openda.models.simpleModel.SimpleOscillatorStochModelInstance.getExchangeItemIDs(): Role selection not implemented yet.");
    }

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		throw new UnsupportedOperationException("org.openda.models.simpleModel.SimpleOscillatorStochModelInstance.getDataObjectExchangeItem(): Not implemented yet.");
	}

	public IPrevExchangeItem getExchangeItem(String exchangeItemID){
		throw new UnsupportedOperationException("org.openda.models.simpleModel.SimpleOscillatorStochModelInstance.getExchangeItem(): Not implemented yet.");
	}

	/**
	 * Get the resulting delta in the model values that correspond to a number of observations, given
	 * a modification of the model state (delta).
	 *
	 * @param deltaState          The delta on the state.
	 * @param observationMetaData The observation items for which the tangent has to be computed.
	 *
	 * @return vector resulting delta in the model values corresponding to each observation given in the descriptions
	 */
	public IVector applyObservationTangent(IVector deltaState, IObservationDescriptions observationMetaData){
		IVector result = new Vector(observationMetaData.getObservationCount());
		IVector obsTimes = null;
		if(observationMetaData instanceof org.openda.utils.CsvObservationDescriptions){
			obsTimes=observationMetaData.getValueProperties("time");
		}else if(observationMetaData instanceof TimeSeriesObservationDescriptions){
			obsTimes=((TimeSeriesObservationDescriptions)observationMetaData).obs.getAllTimes();
		}else{
			throw new RuntimeException("SimpleModel can not handle this type of observations ");
		}
		IVector obsIndex = observationMetaData.getValueProperties("index");
		if(obsIndex==null){
			obsIndex = observationMetaData.getValueProperties("xPosition");
		}
		IVector transformIndex = observationMetaData.getValueProperties("transform");

		Time tHor = new Time(this.pars.get("t_start"), this.pars.get("t_stop"));
		tHor.setStep(this.pars.get("t_step"));
		for(int i=0;i<observationMetaData.getObservationCount();i++){
			int thisXIndex = (int) obsIndex.getValue(i);
			//TODO if(){ //check time
			double deltaXIndex = deltaState.getValue(thisXIndex);
			// transform values if needed
			if(transformIndex!=null){
				if(transformIndex.getValue(i)==2){ // magic number for quadratic observations
					double Xindex = this.state.getValue(thisXIndex);
					deltaXIndex = 2.0*deltaXIndex*Xindex;
				}
			}
			result.setValue(i, deltaXIndex);
		}
		return result;
	}


	/**
	 * Get the resulting adjoint model values that correspond to a number of lamby values for the observations.
	 *
	 * @param lambaY       The lambdaY adjoint values for the observations, for which the
	 *                     lambaX vector for the model state has to be computed.
	 * @param observationMetaData The observation items for which the tangent has to be computed.
	 * @return The lambaX vector for the state
	 */
	public IVector applyObservationAdjoint(IVector lambaY, IObservationDescriptions observationMetaData){
		IVector result = this.state.clone();
		result.setConstant(0.0);
		IVector obsTimes = null;
		if(observationMetaData instanceof org.openda.utils.CsvObservationDescriptions){
			obsTimes=observationMetaData.getValueProperties("time");
		}else if(observationMetaData instanceof TimeSeriesObservationDescriptions){
			obsTimes=((TimeSeriesObservationDescriptions)observationMetaData).obs.getAllTimes();
		}else{
			throw new RuntimeException("SimpleModel can not handle this type of observations ");
		}
		IVector obsIndex = observationMetaData.getValueProperties("index");
		if(obsIndex==null){
			obsIndex = observationMetaData.getValueProperties("xPosition");
		}
		IVector transformIndex = observationMetaData.getValueProperties("transform");

		Time tHor = new Time(this.pars.get("t_start"), this.pars.get("t_stop"));
		tHor.setStep(this.pars.get("t_step"));
		for(int i=0;i<observationMetaData.getObservationCount();i++){
			int thisXIndex = (int) obsIndex.getValue(i);
			//TODO if(){ //check time
			double value = lambaY.getValue(i);
			// transform values if needed
			if(transformIndex!=null){
				if(transformIndex.getValue(i)==2){ // magic number for quadratic observations
					double XIndex = this.state.getValue(thisXIndex);
					value *= 2.0*XIndex;
				}
			}
			result.setValue(thisXIndex, result.getValue(thisXIndex)+value);
		}
		return result;
	}

}
