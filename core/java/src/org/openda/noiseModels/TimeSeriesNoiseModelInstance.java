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
package org.openda.noiseModels;
import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.timeseries.TimeSeries;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.*;
import org.openda.interfaces.IPrevExchangeItem.Role;
import org.openda.interfaces.IStochModelFactory.OutputLevel;
import org.openda.utils.*;
import org.openda.utils.io.FileBasedModelState;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;

/**
 * Module for generating noise for timeseries with Gaussian distribution and exponential time-correlation.
 * This module is intended for coupling to another model, usualy to extend a physical model with some uncertainties
 *
 * Assumes the following structure for the input file:
 * <?xml version="1.0" encoding="UTF-8"?>
 * <timeSeriesNoiseModelConfig>
 *    <simulationTimespan timeFormat="dateTimeString">201008241130,201008241140,...,201008242350</simulationTimespan>
 *    <!--
 *    <simulationTimespan timeFormat="mjd">48259.0,48259.125,...,48260.00</simulationTimespan>
 *    -->
 *
 *   <timeSeries    location="aberdeen"
 *                  position="(-2.045543,57.361939)"
 *                  quantity="waterlevel"
 *                  unit="m"
 *                  source="generated_noise"
 *                  timezone="GMT"  <!-- just metadata, not used -->
 *                  height="0.0"
 *                  id="waterlevel@aberdeen"
 *                  standardDeviation="0.05"
 *                  timeCorrelationScale="3.0"
 *                  timeCorrelationScaleUnit="hours" (days,hours,minutes,seconds)
 *                  initialValue="0.0"
 *   </timeSeries>
 *   <!-- most values have defaults -->
 *   <timeSeries    location="scheveningen"
 *                  quantity="waterlevel"
 *                  standardDeviation="0.05"
 *                  timeCorrelationScale="3.0"
 *                  timeCorrelationScaleUnit="hours" (days,hours,minutes,seconds)
 *   </timeSeries>
 * </timeSeriesNoiseModelConfig>
 *
 * @author verlaanm
 */
public class TimeSeriesNoiseModelInstance extends Instance implements IStochModelInstance {

	protected java.util.Hashtable<String,Double> pars=new java.util.Hashtable<String,Double>();

	protected HashMap<String, TimeSeries> outputSeries=new HashMap<String, TimeSeries>();
    protected ArrayList<String> ids = new ArrayList<String>();
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
	protected StochVector initialStateUncertainty=null;
	protected IVector timeCorrelationPerTimeStep = null;
	protected IVector standardDeviation = null;

	// Counter for keeping track of instances
	protected static int NextinstanceNumber = 1;
	protected int thisInstanceNumber=0;

	// Configuration
	protected File workingDir=null;
	protected String configString =null;
	protected boolean saveStateToFile=false;
	protected ConfigTree conf;
	
	public OutputLevel outputLevel=OutputLevel.Suppress;

	private Time timeHorizon = null;

	public TimeSeriesNoiseModelInstance(ITime timeHorizon) {
		this.timeHorizon = new Time(timeHorizon);
	}

	public void initialize(File workingDir, String[] arguments) {
		// Instance counter
		this.thisInstanceNumber = NextinstanceNumber;
		NextinstanceNumber++;

		// configuration
		this.workingDir = workingDir;
		this.configString = arguments[0];

		/*
		 * now parse configuration
		 */
		Results.putMessage("configstring = "+ configString);
		conf = new ConfigTree(workingDir, configString);

		/*
		 * parse simulationTimespan
		 *
		 *    <simulationTimespan timeFormat="dateTimeString">201008241130,201008241140,...,201008242350</simulationTimespan>
		 *    <!--
		 *    <simulationTimespan timeFormat="mjd">48259.0,48259.125,...,48260.00</simulationTimespan>
		 */
		String timespanString = conf.getContentString("simulationTimespan");
		double startTime     = 0.0;
		double endTime       = 100.0;
		double incrementTime = 1.0;
		if(timespanString!=null){
			Results.putMessage("analysisTimes="+timespanString);
			String timeFormat = conf.getAsString("simulationTimespan@timeFormat", "dateTimeString");
			if(timespanString.indexOf("...")<0){
				throw new RuntimeException("TimeSeriesNoiseModel expects a simulationTimeSpan formatted as eg 201008241130,201008241140,...,201008242350");
			}
			Time[] simulationSequence;
			if(timeFormat.equals("dateTimeString")){
				simulationSequence = Time.Mjds2Times(TimeUtils.dateTimeSequenceString2Mjd(timespanString));
			}else{ //use Mjd
				simulationSequence = Time.Mjds2Times(TimeUtils.MjdSequenceString2Mjd(timespanString));
			}
			int nTimes=simulationSequence.length;
			startTime     = simulationSequence[0].getBeginMJD();
			endTime       = simulationSequence[nTimes-1].getBeginMJD();
			incrementTime = simulationSequence[1].getBeginMJD()-simulationSequence[0].getBeginMJD();
		}
		// overrule with external settings
		if(this.timeHorizon!=null){
			if(!Double.isInfinite(this.timeHorizon.getBeginMJD())){
				startTime=this.timeHorizon.getBeginMJD();
			}
			if(!Double.isInfinite(this.timeHorizon.getEndMJD())){
				endTime=this.timeHorizon.getEndMJD();
			}
			//Note: if the timeHorizon is set from outside, then the timeHorizon can have a timeStep set to NaN,
			//if the method modelInstance.getTimeHorizon() returns a timeHorizon without a valid timeStep, i.e. if the
			//modelInstance is not able to determine the timeStep of the model. This is the case when using
			//for instance the BBModelInstance. Currently the only workaround for this is as follows:
			//In addition to the timeHorizon set from outside, configure a dummy simulationTimeSpan in the noise model
			//config file. Then the difference between the first two times is used as the timeStep for the noise model.
			if(!Double.isNaN(this.timeHorizon.getStepMJD())){
				incrementTime=this.timeHorizon.getStepMJD();
			}
		}


		pars.put("t_start",startTime);
		pars.put("t_step",incrementTime);
		pars.put("t_stop",endTime);
		// start at initial time
		this.t = pars.get("t_start");
		this.timeStep = 0; //counter starts at 0

		/*
		 * Parse input per point
		 *
		 *  <timeSeries    location="aberdeen"
		 *                  position="(-2.045543,57.361939)"
		 *                  quantity="waterlevel"
		 *                  unit="m"
		 *                  source="generated_noise"
		 *                  timezone="GMT"  <!-- just metadata, not used -->
		 *                  height="0.0"
		 *                  id="waterlevel@aberdeen"
		 *                  standardDeviation="0.05"
		 *                  timeCorrelationScale="3.0"
		 *                  timeCorrelationScaleUnit="hours" (days,hours,minutes,seconds)
		 *                  initialValue="0.0"
		 *   </timeSeries>
		 */
		ConfigTree seriesTrees[] = conf.getSubTrees("timeSeries");
		this.state = new Vector(seriesTrees.length);
		this.timeCorrelationPerTimeStep = new Vector(seriesTrees.length);
		this.standardDeviation = new Vector(seriesTrees.length);

		for(int i=0;i<seriesTrees.length;i++){
			// create empty series
			TimeSeries tempSeries= new TimeSeries();
			// quantity
			String keyword = "quantity";
			String stringValue = "unknown_quantity";
			stringValue = seriesTrees[i].getAsString("@"+keyword, stringValue);
			stringValue=stringValue.trim();
			//System.out.println(""+keyword+"="+stringValue);
			tempSeries.setQuantity(stringValue);
			// unit
			keyword = "unit";
			stringValue = "unknown_unit";
			stringValue = seriesTrees[i].getAsString("@"+keyword, stringValue);
			stringValue=stringValue.trim();
			//System.out.println(""+keyword+"="+stringValue);
			tempSeries.setUnit(stringValue);
			// source
			keyword = "source";
			stringValue = "unkown_source";
			stringValue = seriesTrees[i].getAsString("@"+keyword, stringValue);
			stringValue=stringValue.trim();
			//System.out.println(""+keyword+"="+stringValue);
			tempSeries.setSource(stringValue);
			// timezone
			keyword = "timezone";
			stringValue = "UTC";
			stringValue = seriesTrees[i].getAsString("@"+keyword, stringValue);
			stringValue=stringValue.trim();
			//System.out.println(""+keyword+"="+stringValue);
			tempSeries.setProperty(keyword,stringValue);
			// height
			keyword = "height";
			double doubleValue = Double.NaN;
			doubleValue = seriesTrees[i].getAsDouble("@"+keyword, doubleValue);
			//System.out.println(""+keyword+"="+doubleValue);
			tempSeries.setHeight(doubleValue);
			// location
			keyword = "location";
			stringValue ="loc_"+i;
			stringValue = seriesTrees[i].getAsString("@"+keyword, stringValue);
			stringValue=stringValue.trim();
			//System.out.println(""+keyword+"="+stringValue);
			tempSeries.setLocation(stringValue);
			// position
			keyword = "position";
			stringValue = "";
			stringValue = seriesTrees[i].getAsString("@"+keyword, stringValue);
			double x=0.0;
			double y=0.0;
			if(!stringValue.equals("")){
				try {
					stringValue=stringValue.trim();
					String partString = stringValue.substring(1,stringValue.length()-2);
					String parts[] = partString.split(",");
					x = Double.parseDouble(parts[0]);
					y = Double.parseDouble(parts[1]);
				} catch (Exception e) {
					throw new RuntimeException("Problem parsing position '"+stringValue+"' "+e.getMessage());
				}
			}
			//System.out.println(""+keyword+"=("+x+","+y+")");
			tempSeries.setPosition(x, y);
			// id
			keyword = "id";
			stringValue =tempSeries.getLocation()+"."+tempSeries.getQuantityId();;
			stringValue = seriesTrees[i].getAsString("@"+keyword, stringValue);
			stringValue=stringValue.trim();
			//System.out.println(""+keyword+"="+stringValue);
			tempSeries.setId(stringValue);
			// initialValue
			keyword = "initialValue";
			double initialValue = 0.0;
			initialValue = seriesTrees[i].getAsDouble("@"+keyword, initialValue);
			tempSeries.setProperty(keyword, ""+initialValue );
			this.state.setValue(i, initialValue);
			// standardDeviation
			keyword = "standardDeviation";
			double std = 0.05;
			std = seriesTrees[i].getAsDouble("@"+keyword, std);
			//System.out.println(""+keyword+"="+std);
			tempSeries.setProperty(keyword,""+std);
			// timeCorrelationScale="3.0"
			keyword = "timeCorrelationScale";
			double timeCorrelationScale = 1.0;
			timeCorrelationScale = seriesTrees[i].getAsDouble("@"+keyword, timeCorrelationScale);
			tempSeries.setProperty(keyword, ""+timeCorrelationScale );
			// timeCorrelationScaleUnit="hours" (days,hours,minutes,seconds)
			keyword = "timeCorrelationScaleUnit";
			stringValue = "days";
			stringValue = seriesTrees[i].getAsString("@"+keyword, stringValue);
			stringValue=stringValue.trim();
			if (stringValue.toLowerCase().contentEquals("days")){
				timeCorrelationScale = timeCorrelationScale*1.0;
			}else if(stringValue.toLowerCase().contentEquals("hours")){
				timeCorrelationScale = timeCorrelationScale/24.0;
			}else if(stringValue.toLowerCase().contentEquals("minutes")){
				timeCorrelationScale = timeCorrelationScale/24.0/60.0;
			}else if(stringValue.toLowerCase().contentEquals("seconds")){
				timeCorrelationScale = timeCorrelationScale/24.0/60.0/60.0;
			}else{
				throw new RuntimeException("unknown timeCorrelationScaleUnit."
						+" Expected (days,hours,minutes,seconds),but found "+stringValue);
			}
			tempSeries.setProperty(keyword, stringValue);

			// noise configuration
			double alpha = Math.exp(-incrementTime/timeCorrelationScale);
			this.timeCorrelationPerTimeStep.setValue(i, alpha);
			this.standardDeviation.setValue(i, std*Math.sqrt(1-alpha*alpha));

			// empty contents
			tempSeries.setTimes(new double[0]);
			tempSeries.setValues(new double[0]);

			//store series
			String id=tempSeries.getId();
			this.outputSeries.put(id, tempSeries);
			tempSeries=null;
			this.ids.add(id);
		} // loop over series i

		this.sysNoiseIntensity = new StochVector(new Vector(seriesTrees.length) , this.standardDeviation);
	}

	public IPrevExchangeItem getExchangeItem(String exchangeItemID) {
		return outputSeries.get(exchangeItemID);
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
	}

	public void compute(ITime targetTime) {
		double t_step = this.pars.get("t_step");
		int nsteps = (int) Math.round( (targetTime.getMJD()-this.t)/t_step);
		IVector x = this.state;
		int nextIndex=0;
		double[][] allValues = new double[this.ids.size()][];
		double[] times;
		for(int seriesIndex=0;seriesIndex<this.ids.size();seriesIndex++){
			allValues[seriesIndex] = new double[nsteps+1];
		}
		times=new double[nsteps+1];
		//also store initial value
		for(int seriesIndex=0;seriesIndex<this.ids.size();seriesIndex++){
			allValues[seriesIndex][0] = x.getValue(seriesIndex);
		}
		times[0]=this.t;
		nextIndex++;

		for(int i=0;i<nsteps;i++){
			// --> AR(1)
			//System.out.print("step :"+i+" ");
			x.pointwiseMultiply(this.timeCorrelationPerTimeStep);

			// add system noise
			if(this.autoNoise){
				addRealization(x);
			}

			this.t+=t_step;
			this.timeStep++;
			// output to writer
			if(this.outputLevel!=OutputLevel.Suppress){
                Results.putValue("time_noise_series", this.t, 1, "any", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
                Results.putValue("x_noise_series", x, x.getSize(), "any", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
			}
			// buffer for exchangeItems
			for(int seriesIndex=0;seriesIndex<this.ids.size();seriesIndex++){
				allValues[seriesIndex][nextIndex] = x.getValue(seriesIndex);
			}
			times[nextIndex]=this.t;
			nextIndex++;
		}
		this.state.setValues(x.getValues());

		for(int seriesIndex=0;seriesIndex<this.ids.size();seriesIndex++){
			String id=this.ids.get(seriesIndex);
			TimeSeries series=this.outputSeries.get(id);
			series.setData(times, allValues[seriesIndex]);
		}

	}

	public IVector[] getObservedLocalization(
			IObservationDescriptions observationDescriptions, double distance) {
		// TODO Auto-generated method stub
		return null;
	}

	public class savedState implements IModelState {
		boolean coldStart = false;
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
				if (this.coldStart) {
					out.write("COLD_START\n");
				} else {
				// save time
				line = "t="+this.time;
				out.write(line+"\n");
				// save timestep
				line = "timestep="+this.timestep;
				out.write(line+"\n");
				}

				out.close();
			} catch (IOException e) {
				throw new RuntimeException("Could not create state file " + file.getAbsolutePath());
			}
		}
	}

	public IModelState saveInternalState() {
		savedState saveState=new savedState();
		if (this.timeStep == -1) {
			saveState.coldStart = true;
		} else {
			saveState.coldStart = false;
		}
		saveState.time = this.t;
		saveState.timestep = this.timeStep;
		saveState.state = this.state.clone();
		return saveState;
	}

	public void restoreInternalState(IModelState savedInternalState) {
		savedState saveState = (savedState) savedInternalState;
		this.state = saveState.state.clone();
		if (saveState.coldStart) {
			//if cold state start, then always start at the startTime of the timeHorizon. 
			this.timeStep = 0;
			this.t = this.pars.get("t_start");
		} else {
			this.timeStep = saveState.timestep;
			this.t = saveState.time;
		}
	}

	public void releaseInternalState(IModelState savedInternalState) {
		if(this.saveStateToFile){
			FileBasedModelState modelState = (FileBasedModelState) savedInternalState;
			modelState.releaseState(this.workingDir);
		}else{
			savedState saveState = (savedState) savedInternalState;
			saveState.coldStart = false;
			saveState.time = -1;
			saveState.timestep = -1;
			saveState.state = null;
		}
	}

	public IModelState loadPersistentState(File persistentStateFile) {
		Vector x=null;
		boolean coldStart = false;
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
			if (line != null && BBUtils.isColdStart(line.trim())) {
				coldStart = true;
				t = -1;
				timestep = -1;
			} else {
			keyValuePair = line.split("="); // expect eg t=1.0
			t = Double.parseDouble(keyValuePair[1]);
			// Read and parse third line
			line = buff.readLine();
			keyValuePair = line.split("="); // expect eg timestep=3
			timestep = Integer.parseInt(keyValuePair[1]);
			}

			buff.close();
		} catch (IOException e) {
			Results.putMessage("Exception: " + e.getMessage());
			throw new RuntimeException("Error reading restart file for model: " + persistentStateFile.getAbsolutePath());
		}
		// set state and time
		savedState saveState=new savedState();
		saveState.coldStart = coldStart;
		saveState.time = t;
		saveState.timestep = timestep;
		saveState.state = x;
		return saveState;
	}

	public void finish() {
		// nothing needed
	}

	public String[] getExchangeItemIDs() {
		return this.ids.toArray(new String[this.ids.size()]);
	}

	public String[] getExchangeItemIDs(Role role) {
		String[] result;
		if(role==Role.Output){
			result=this.getExchangeItemIDs();
		}else{
			result=new String[0];
		}
		return result;
	}

	public IExchangeItem getDataObjectExchangeItem(String exchangeItemID) {
		// TODO Auto-generated method stub
		return null;
	}

	public IInstance getParent() {
		// TODO Auto-generated method stub
		return null;
	}

	public IVector getParameters() {
		IVector result = new Vector(this.stochParNames.size());
		for(int i=0;i<this.stochParNames.size();i++){
			result.setValue(i, pars.get(stochParNames.get(i)));
		}
		return result;
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
		temp.axpy(alpha, vector); //do axpy on parameters as axplicit Vector-object
		this.setParameters(temp);
		// nothing special in this model
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
		throw new UnsupportedOperationException("org.openda.noiseModels.TimeSeriesNoiseModelInstance.getWhiteNoiseTimes(): Not implemented yet.");
	}

	public IVector[] getWhiteNoise(ITime timeSpan) {
		throw new UnsupportedOperationException("org.openda.noiseModels.TimeSeriesNoiseModelInstance.getWhiteNoise(): Not implemented yet.");
	}

	public void setWhiteNoise(IVector[] whiteNoise) {
		throw new UnsupportedOperationException("org.openda.noiseModels.TimeSeriesNoiseModelInstance.setWhiteNoise(): Not implemented yet.");
	}

	public void axpyOnWhiteNoise(double alpha, IVector[] vector) {
		throw new UnsupportedOperationException("org.openda.noiseModels.TimeSeriesNoiseModelInstance.axpyOnWhiteNoise(): Not implemented yet.");
	}

	public void setAutomaticNoiseGeneration(boolean value) {
		this.autoNoise = value;
	}

	public IVector getObservedValues(
			IObservationDescriptions observationDescriptions) {
		throw new UnsupportedOperationException("org.openda.noiseModels.TimeSeriesNoiseModelInstance.getObservedValues(): Not implemented yet.");
	}

	public void announceObservedValues(
			IObservationDescriptions observationDescriptions) {
		throw new UnsupportedOperationException("org.openda.noiseModels.TimeSeriesNoiseModelInstance.announceObservedValues(): Not implemented yet.");
	}

	public IVector getStateScaling() {
		// no real scaling implemented,though we could use omega
		IVector result = new Vector(this.getState().getSize());
		result.setConstant(1.0);
		return result;
	}

	public IVector[] getStateScaling(
			IObservationDescriptions observationDescriptions) {
		int m=observationDescriptions.getObservationCount();
		// no real scaling implemented,Schur product makes no sense here!
		IVector result[] = new Vector[m];
		for(int i=0;i<m;i++){
			result[i] = new Vector(this.getState().getSize());
			result[i].setConstant(1.0);
		}
		return result;
	}

	public File getModelRunDir() {
		// TODO Auto-generated method stub
		return null;
	}
	
	protected void addRealization(IVector x){
		IVector w = this.sysNoiseIntensity.createRealization();
		//Results.putProgression("> w="+w+" sqrt(t_step)="+Math.sqrt(t_step));
		x.axpy(1.0, w);
	}
	

}
