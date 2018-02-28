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
package org.openda.noiseModels;

import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.ArrayExchangeItem;
import org.openda.exchange.ArrayGeometryInfo;
import org.openda.exchange.QuantityInfo;
import org.openda.exchange.TimeInfo;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.*;
import org.openda.interfaces.IPrevExchangeItem.Role;
import org.openda.interfaces.IStochModelFactory.OutputLevel;
import org.openda.noiseModels.SpatialCorrelationStochVector.CoordinatesType;
import org.openda.utils.*;
import org.openda.utils.io.FileBasedModelState;

import java.io.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 * Module for generating noise for timeseries of 2D maps with Gaussian distribution and
 * exponential time-correlation. The spatial correlation is Gaussian
 * This module is intended for coupling to another model, usually to extend a physical model
 * with some uncertainties.
 * To speed up calculations the coordinates can be treated as separable. This means that correlations
 * are computed for both spatial directions separately. This is much faster, but is only approximate for 
 * spherical coordinates, especially near the poles.
 *
 * Assumes the following structure for the input file:
 * <?xml version="1.0" encoding="UTF-8"?>
 * <mapsNoiseModelConfig>
 *    <simulationTimespan timeFormat="dateTimeString">201008241200,201008241210,...,201008242350</simulationTimespan>
 *    <noiseItem id="windU" quantity="wind-u" unit="m/s" height="10.0"
 *   	  standardDeviation="1.0" timeCorrelationScale="12.0" timeCorrelationScaleUnit="hours"
 *        initialValue="0.0" horizontalCorrelationScale="500" horizontalCorrelationScaleUnit="km" >
 *        <grid type="cartesian" coordinates="wgs84" separable="true">
 *	        <x>-5,0,...,5</x>
 *	 		<y>50,55,...,60</y>
 *	 	  </grid>
 *	  </noiseItem>
 * </mapsNoiseModelConfig>
 *
 * @author verlaanm
 */
public class MapsNoiseModelInstance extends Instance implements IStochModelInstance {

	protected java.util.Hashtable<String,Double> pars=new java.util.Hashtable<String,Double>();

	protected HashMap<String, ArrayExchangeItem> outputSeries=new HashMap<String, ArrayExchangeItem>();
    protected ArrayList<String> ids = new ArrayList<String>();
	protected HashMap<String, Double> initialValues=new HashMap<String, Double>();
	protected HashMap<String, Double> standardDeviations=new HashMap<String, Double>();
	protected HashMap<String, Double> standardDeviationWhiteNoise=new HashMap<String, Double>();
	protected HashMap<String, Double> timeCorrelationScales=new HashMap<String, Double>();
	protected HashMap<String, Double> horizontalCorrelationScales=new HashMap<String, Double>();
	protected HashMap<String, Integer> xSize=new HashMap<String, Integer>();
	protected HashMap<String, Integer> ySize=new HashMap<String, Integer>();
	// Internal state of the model
	protected TreeVector state=null;
	protected double t; //current time
	protected int timeStep; //integer value is used for testing equality and counting

	// Parameter used for calibration
	protected java.util.Vector<String> stochParNames= new java.util.Vector<String>();
	protected StochVector stochPars=null;

	// System noise for Kalman filtering
	protected IStochVector sysNoiseIntensity=null;
	protected boolean autoNoise = false;
	protected IStochVector initialStateUncertainty=null;
	protected TreeVector timeCorrelationPerTimeStep = null;

	// Counter for keeping track of instances
	protected static int NextinstanceNumber = 1;
	protected int thisInstanceNumber=0;

	// Configuration
	protected File workingDir=null;
	protected String configString =null;
	protected boolean saveStateToFile=false;

	public OutputLevel outputLevel=OutputLevel.Suppress;
	protected Time timeHorizon = null;
	private Map<String, SpatialCorrelationCovariance[]> eiCovarianceMap;



	public MapsNoiseModelInstance(ITime timeHorizon) {
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
		ConfigTree conf = new ConfigTree(workingDir, configString);

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
			if(!timespanString.contains("...")){
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
		 * Parse input per item
		 * 
		 *   <noiseItem id="windU" 
		 *              quantity="wind-u" 
		 *              unit="m/s" height="10.0"
		 *   	        standardDeviation="1.0" 
		 *              timeCorrelationScale="12.0" timeCorrelationScaleUnit="hours"
		 *              initialValue="0.0" 
		 *              horizontalCorrelationScale="500" horizontalCorrelationScaleUnit="km" >
		 *        <grid type="cartesian" coordinates="wgs84" separable="true" >
		 *	        <x>-5,0,...,5</x>
		 *	 		<y>50,55,...,60</y>
		 *	 	  </grid>
		 *	  </noiseItem>
		 */
		ConfigTree itemTrees[] = conf.getSubTrees("noiseItem");
		this.state = new TreeVector("state");
		this.timeCorrelationPerTimeStep = new TreeVector("timeCorrelationPerTimeStep");
		StochTreeVector systemNoise = new StochTreeVector("systemNoise");
		this.sysNoiseIntensity = systemNoise;
		
		for(int i=0;i<itemTrees.length;i++){
			// id
			String keyword = "id";
			String stringValue = "";
			stringValue = itemTrees[i].getAsString("@"+keyword, stringValue);
			String id = stringValue.trim();
			if(id.length()==0){
				throw new RuntimeException("MapsNoiseModelInstance: missing id for item nr"+(i+1));
			}
			System.out.println("Id="+id);
			// create empty item
			ArrayExchangeItem tempItem= new ArrayExchangeItem(id,Role.Output);
			// quantity
			keyword = "quantity";
			stringValue = "unknown_quantity";
			stringValue = itemTrees[i].getAsString("@"+keyword, stringValue);
			stringValue=stringValue.trim();
			//System.out.println(""+keyword+"="+stringValue);
			QuantityInfo tempQ = new QuantityInfo(stringValue,"dummyUnit");
			// unit
			keyword = "unit";
			stringValue = "unknown_unit";
			stringValue = itemTrees[i].getAsString("@"+keyword, stringValue);
			stringValue=stringValue.trim();
			//System.out.println(""+keyword+"="+stringValue);
			tempQ.setUnit(stringValue);
			tempItem.setQuantityInfo(tempQ);

			// height
			keyword = "height";
			double doubleValue = Double.NaN;
			doubleValue = itemTrees[i].getAsDouble("@"+keyword, doubleValue);
			//System.out.println(""+keyword+"="+doubleValue);
			//tempItem.setHeight(doubleValue); TODO

			/* spatial grid
			 *        <grid type="cartesian" coordinates="wgs84" separable="true">
			 *	        <x>-5,0,...,5</x>
			 *	 		<y>50,55,...,60</y>
			 *	 	  </grid>
			 */
			keyword = "grid@type";
			stringValue = "cartesian";
			stringValue = itemTrees[i].getAsString(keyword, stringValue);
			if(!stringValue.toLowerCase().startsWith("cart")){
				throw new RuntimeException("MapsNoisModelInstance - grid@type : only type cartesian is supported,"
						+" but found "+stringValue);
			}
			keyword = "grid@coordinates";
			stringValue = "wgs84";
			stringValue = itemTrees[i].getAsString(keyword, stringValue);
			CoordinatesType coordsType;
			if (stringValue.toLowerCase().contentEquals("xy")){
				coordsType = CoordinatesType.XY;
			}else if(stringValue.toLowerCase().contentEquals("wgs84")){
				coordsType = CoordinatesType.WGS84;
			}else{
				throw new RuntimeException("unknown grid@coordinates."
						+" Expected (WGS84,XY),but found "+stringValue);
			}
			keyword = "grid@separable";
			boolean separableDefault =true; // for lat-lon grids this is an approximation, but much faster
			boolean separable = itemTrees[i].getAsBoolean(keyword, separableDefault);

			// grid values
			String xValuesString = itemTrees[i].getContentString("grid/x");
			String yValuesString = itemTrees[i].getContentString("grid/y");
			double x[];
			double y[];
			try {
				x = parseGridOneDim(xValuesString);				
			} catch (Exception e) {
				throw new RuntimeException("Problems parsing x-grid");
			}
			try {
				y = parseGridOneDim(yValuesString);				
			} catch (Exception e) {
				throw new RuntimeException("Problems parsing y-grid");
			}			
			System.out.println("x="+xValuesString);
			System.out.println("y="+yValuesString);
			this.xSize.put(id, x.length);
			this.ySize.put(id, y.length);
			Array latitudeArray = new Array(y,new int[]{y.length},true);
			Array longitudeArray = new Array(x,new int[]{x.length},true);
			int latitudeValueIndices[] = new int[]{1};  // we use the order time, lat, lon according to CF
			int longitudeValueIndices[] = new int[]{2}; 
			QuantityInfo latitudeQuantityInfo = null; //TODO
			QuantityInfo longitudeQuantityInfo = null;
			ArrayGeometryInfo tempG = new ArrayGeometryInfo(latitudeArray, latitudeValueIndices, latitudeQuantityInfo, 
					   longitudeArray, longitudeValueIndices, longitudeQuantityInfo,null,null,null);
			tempItem.setGeometryInfo(tempG);
			
			// initialValue
			keyword = "initialValue";
			double initialValue = 0.0;
			initialValue = itemTrees[i].getAsDouble("@"+keyword, initialValue);
			TreeVector statePart = new TreeVector(id, new Vector(x.length*y.length),y.length,x.length);
			this.initialValues.put(id, initialValue);
			statePart.setConstant(initialValue);
			this.state.addChild(statePart);
			// standardDeviation
			keyword = "standardDeviation";
			double std = 0.05;
			std = itemTrees[i].getAsDouble("@"+keyword, std);
			//System.out.println(""+keyword+"="+std);
			this.standardDeviations.put(id, std );

			// timeCorrelationScale="3.0"
			keyword = "timeCorrelationScale";
			double timeCorrelationScale = 1.0;
			timeCorrelationScale = itemTrees[i].getAsDouble("@"+keyword, timeCorrelationScale);
			// timeCorrelationScaleUnit="hours" (days,hours,minutes,seconds)
			keyword = "timeCorrelationScaleUnit";
			stringValue = "days";
			stringValue = itemTrees[i].getAsString("@"+keyword, stringValue);
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
			this.timeCorrelationScales.put(id, timeCorrelationScale ); //scaled to days
			// spatial correlation : horizontalCorrelationScale
			keyword = "horizontalCorrelationScale";
			double horizontalCorrelationScale = 0.0;
			horizontalCorrelationScale = itemTrees[i].getAsDouble("@"+keyword, horizontalCorrelationScale);
			// spatial correlation : horizontalCorrelationScaleUnit
			keyword = "horizontalCorrelationScaleUnit";
			stringValue = "m";
			stringValue = itemTrees[i].getAsString("@"+keyword, stringValue);
			stringValue=stringValue.trim();
			if (stringValue.toLowerCase().contentEquals("m")){
				horizontalCorrelationScale = horizontalCorrelationScale*1.0;
			}else if(stringValue.toLowerCase().contentEquals("km")){
				horizontalCorrelationScale = horizontalCorrelationScale*1000.;
			}else if(stringValue.toLowerCase().contentEquals("cm")){
				horizontalCorrelationScale = horizontalCorrelationScale*0.01;
			}else{
				throw new RuntimeException("unknown horizontalCorrelationScaleUnit."
						+" Expected (m,km,cm),but found "+stringValue);
			}
			this.horizontalCorrelationScales.put(id, horizontalCorrelationScale );

			// noise configuration
			double alpha = Math.exp(-incrementTime/timeCorrelationScale);
			double stdWhiteNoise = std*Math.sqrt(1-alpha*alpha);
			this.standardDeviationWhiteNoise.put(id, stdWhiteNoise);
			TreeVector alphaPart = statePart.clone();
			alphaPart.setConstant(alpha);
			this.timeCorrelationPerTimeStep.addChild(alphaPart);
			// stochVector for generation of white noise
			double x2[]=new double[x.length*y.length];
			double y2[]=new double[x.length*y.length];
			expandGrid(x,y,x2,y2);
			addSystemNoiseChild(systemNoise, id, coordsType, separable, x, y, horizontalCorrelationScale, stdWhiteNoise, x2, y2);
			// 
			// output storage
			//
			this.outputSeries.put(id, tempItem);
			tempItem=null;
			this.ids.add(id);
		} // loop over series i
	}

	private void addSystemNoiseChild(StochTreeVector systemNoise, String id, CoordinatesType coordsType, boolean separable, double[] x, double[] y, double horizontalCorrelationScale, double stdWhiteNoise, double[] x2, double[] y2) {
		if (separable) {
			SpatialCorrelationCovariance[] preCalculatedCovarianceMatrix = eiCovarianceMap != null ? eiCovarianceMap.get(id) : null;
			Spatial2DCorrelationStochVector spatial2DCorrelationStochVector = new Spatial2DCorrelationStochVector(coordsType, stdWhiteNoise, horizontalCorrelationScale, x, y, preCalculatedCovarianceMatrix);
			if (eiCovarianceMap != null && preCalculatedCovarianceMatrix == null) {
				SpatialCorrelationCovariance[] covariance = spatial2DCorrelationStochVector.getSharableCovariance();
				SpatialCorrelationCovariance[] put = eiCovarianceMap.put(id, covariance);
				assert put == null;
			}
			systemNoise.addChild(spatial2DCorrelationStochVector);
			return;
		}
		SpatialCorrelationCovariance[] preCalculatedCovarianceMatrix = eiCovarianceMap != null ? eiCovarianceMap.get(id) : null;
		SpatialCorrelationStochVector spatialCorrelationStochVector = new SpatialCorrelationStochVector(coordsType, stdWhiteNoise, horizontalCorrelationScale, x2, y2, preCalculatedCovarianceMatrix);
		if (eiCovarianceMap != null && preCalculatedCovarianceMatrix == null) {
			SpatialCorrelationCovariance[] covariance = spatialCorrelationStochVector.getSharableCovariance();
			SpatialCorrelationCovariance[] put = eiCovarianceMap.put(id, covariance);
			assert put == null;
		}
		systemNoise.addChild(spatialCorrelationStochVector);
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
		if(nsteps<0){
			throw new RuntimeException("MapNoisemodel cannot compute backwards in time, ie target<currentTime");
		}
		IVector x = this.state;
		
		Array allValues[] = new Array[this.ids.size()];
		for(int itemIndex=0;itemIndex<this.ids.size();itemIndex++){
			//ArrayBasedExchangeItem item =this.outputSeries.get(ids.get(itemIndex));
			int xn=this.xSize.get(ids.get(itemIndex));
			int yn=this.ySize.get(ids.get(itemIndex));
			allValues[itemIndex] = new Array(new int[]{nsteps+1,yn,xn});
		}
		double[] times=new double[nsteps+1];
		// save state for output
		int nextIndex=0;
		for(int itemIndex=0;itemIndex<this.ids.size();itemIndex++){
			String id=this.ids.get(itemIndex);
			ITreeVector statePart = ((TreeVector) x).getSubTreeVector(id);
			allValues[itemIndex].setSlice(statePart.getValues(), 0, nextIndex, nextIndex);
		}
		times[nextIndex]=this.t;
		nextIndex++;

		for(int i=0;i<nsteps;i++){
			// --> AR(1)
			//System.out.print("step :"+i+" ");
			x.pointwiseMultiply(this.timeCorrelationPerTimeStep);

			// add system noise
			if(this.autoNoise){
				IVector w = this.sysNoiseIntensity.createRealization();
				//Results.putProgression("> w="+w+" sqrt(t_step)="+Math.sqrt(t_step));
				Results.putValue("white_noise", w, w.getSize(), "noisemodel_2d_"+this.thisInstanceNumber,  IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
				x.axpy(1.0, w);
			}

			this.t+=t_step;
			this.timeStep++;

			if(this.outputLevel!=OutputLevel.Suppress){
                Results.putValue("model_time", this.t, 1,"noisemodel_2d_"+this.thisInstanceNumber, IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
                Results.putValue("x", x, x.getSize(),"noisemodel_2d_"+this.thisInstanceNumber, IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
			}

			// save state for output
			for(int itemIndex=0;itemIndex<this.ids.size();itemIndex++){
				String id=this.ids.get(itemIndex);
				ITreeVector statePart = ((TreeVector) x).getSubTreeVector(id);
				allValues[itemIndex].setSlice(statePart.getValues(), 0,nextIndex, nextIndex);
			}
			times[nextIndex]=this.t;
			nextIndex++;
			
		}
		this.state.setValues(x.getValues());

		// output storage set times and values for this compute
		for(int itemIndex=0;itemIndex<this.ids.size();itemIndex++){
			ArrayExchangeItem item =this.outputSeries.get(ids.get(itemIndex));
			item.setArray(allValues[itemIndex]);
			TimeInfo timeInfo = new TimeInfo(times);
			item.setTimeInfo(timeInfo);
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
		IVector state=null;

		public void savePersistentState(File file) {
			try {
				FileWriter out = new FileWriter(file);
				// save state vector
				Vector tempVec = new Vector(this.state); //TODO create a more readable restart file
				int n = this.state.getSize()+1;
				String line="x="+tempVec.toString(n);
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
		// ODA-615 Give error message on ArrayIndexOutOfBoundsException when restoring internal state in OpenDA in FEWS
		try {
			this.state.setValues(saveState.state.getValues());
		} catch (ArrayIndexOutOfBoundsException e) {
			if (state.getValues().length != saveState.state.getValues().length) {
				String message = "State '" + state.getId() + "' does not have the same size " + state.getValues().length + " as saved state size " + saveState.state.getValues().length;
				Results.putProgression(message);
				throw new RuntimeException(message, e);
			}
			throw new RuntimeException("Array out of bounds exception when restoring internal state", e);
		}
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
		return (String[]) this.ids.toArray(new String[this.ids.size()]);
	}

	public String[] getExchangeItemIDs(Role role) {
		String[] result=null;
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
		throw new UnsupportedOperationException("org.openda.noiseModels.MapsNoisModelInstance.getWhiteNoiseTimes(): Not implemented yet.");
	}

	public IVector[] getWhiteNoise(ITime timeSpan) {
		throw new UnsupportedOperationException("org.openda.noiseModels.MapsNoisModelInstance.getWhiteNoise(): Not implemented yet.");
	}

	public void setWhiteNoise(IVector[] whiteNoise) {
		throw new UnsupportedOperationException("org.openda.noiseModels.MapsNoisModelInstance.setWhiteNoise(): Not implemented yet.");
	}

	public void axpyOnWhiteNoise(double alpha, IVector[] vector) {
		throw new UnsupportedOperationException("org.openda.noiseModels.MapsNoisModelInstance.axpyOnWhiteNoise(): Not implemented yet.");
	}

	public void setAutomaticNoiseGeneration(boolean value) {
		this.autoNoise = value;
	}

	public IVector getObservedValues(
			IObservationDescriptions observationDescriptions) {
		throw new UnsupportedOperationException("org.openda.noiseModels.MapsNoisModelInstance.getObservedValues(): Not implemented yet.");
	}

	public void announceObservedValues(
			IObservationDescriptions observationDescriptions) {
		throw new UnsupportedOperationException("org.openda.noiseModels.MapsNoisModelInstance.announceObservedValues(): Not implemented yet.");
	}

	public IVector getStateScaling() {
		// no real scaling implemented.
		IVector result = new Vector(this.getState().getSize());
		result.setConstant(1.0);
		return result;
	}

	public IVector[] getStateScaling(
			IObservationDescriptions observationDescriptions) {
		int m=observationDescriptions.getObservationCount();
		// no real scaling implemented. TODO add Schur product!
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
	
	//
	// Local utilities
	//
	
	/**
	 * Parse a string with a list of numbers eg 1.0,2.0,3.0
	 * or 1.0,2.0,...,10.0 for sequence with fixed steps
	 * into a double[] array
	 * @param valuesString String containing the double values
	 * @return values as doubles
	 */
	public static double[] parseGridOneDim(String valuesString){
		if(valuesString.equals("")){ //TODO add 0,5,...20
			throw new RuntimeException("MapsNoisModelInstance: missing grid");
		}
		valuesString=valuesString.trim();
		String parts[] = valuesString.split(",");
		double[] result = null;
		if(!valuesString.contains("...")){  // eg 1.0,2.0,3.0
			int n=parts.length;
			result = new double[n];
			for(int j=0;j<n;j++){
				result[j]=Double.parseDouble(parts[j]);
			}
		}else{
			double start = Double.parseDouble(parts[0]);
			double step = Double.parseDouble(parts[1])-start;
			double stop = Double.parseDouble(parts[3]);
			int n=(int)Math.round((stop-start)/step)+1;
			result = new double[n];
			for(int i=0;i<n;i++){
				result[i]=start+i*step;
			}
		}
		return result;
	}
	
	protected void expandGrid(double x[],double y[],double x2[],double y2[]){
		int index=0;
		for(int i=0;i<x.length;i++){
			for(int j=0;j<y.length;j++){
				x2[index] = x[i];
				y2[index] = y[j];
				index++;
			}
		}
	}

	public void setEiCovarianceMap(Map<String, SpatialCorrelationCovariance[]> eiCovarianceMap) {
		this.eiCovarianceMap = eiCovarianceMap;
	}
}
