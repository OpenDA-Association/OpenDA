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
package org.openda.algorithms.kalmanFilter;

import org.openda.blackbox.config.BBUtils;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.*;
import org.openda.utils.ConfigTree;
import org.openda.utils.Instance;
import org.openda.utils.Results;
import org.openda.utils.Time;
import org.openda.utils.io.FileBasedModelState;
import org.openda.utils.performance.OdaGlobSettings;
import org.openda.utils.performance.OdaTiming;

import java.io.File;
import java.io.FileFilter;
import java.text.ParseException;
import java.util.HashMap;

import static org.openda.utils.performance.OdaGlobSettings.getTimePrecision;

/**
 * @author Martin Verlaan
 * Common parts for sequential data-assimilation methods.
 */
public abstract class AbstractSequentialAlgorithm extends Instance implements IAlgorithm {

	OdaTiming timerNext, timerNext_ObsSel, timerNext_Compute, timerNext_forecast, timerNext_getState;
	OdaTiming timerNext_resultWriter, timerNext_getObsDescr, timerNext_getObsVal, timerNext_analysis;

	protected static final String ALGORITHM_RESTART_TEMP_DIR_PREFIX = "algorithm_restart_tempdir_";
	protected static final String ENSEMBLE_MEMBER_RESTART_FILE_PREFIX = "ensemble_";
	protected static final String MAIN_MODEL_RESTART_FILE_PREFIX = "mainmodel_";

	// class data
	protected File workingDir = null;
	protected String configString = null;
	protected IStochModelFactory stochModelFactory = null;
	protected IStochObserver stochObserver = null;
	protected ConfigTree configurationAsTree=null;

	protected int nSteps = 0;
	protected int thisStep = 0;
	protected boolean stepsFromObserver = true;
	protected boolean continueWithoutObservation=false;
	protected ITime analysisTimes[] = null;

	protected boolean skipAtInitialTime=false;
	protected boolean skipAtFinalTime=false;

	//IStochModelInstance mod = null;
	protected ITime currentTime = null;
	protected ITime modelSpan = null;
	protected ITime finalTime = null;
	protected boolean FinalStepNoAnalysis=false;

	//restart data
	protected HashMap<String, IStochModelInstance>   restartData   = new HashMap<String, IStochModelInstance>();
		
	// Kalman preparations
	// - the actual analysis is performed by the filter
	protected HashMap<String, IVector> gainVectors = new HashMap<String, IVector>();
	protected HashMap<String, String> obsId = new HashMap<String, String>();
	protected HashMap<String, Double> obsTimeOffset = new HashMap<String, Double>();

	
	//collecting data for writing output
	private IVector currentState = null;
	public double cost = 0.0;
	private int iteration=1;
	// mainModel is used for collecting output of the final estimate, eg. the ensemble-average for an
	// ensemble Kalman filter
	public IStochModelInstance mainModel=null; // this model should contain the results for generating output
	public boolean stochParameter=false;
	public boolean stochForcing=false;
	public boolean stochInit=false;
	private String timeIncrement;
	private String timeOffset;
	private String timeUnit;


	/*
		 * The following classes need to be implemented by inheriting classes.
		 */

	/**
	 * Preparations for the algorithm. Put the part of the initialization that require significant computation
	 * here. This method is called after initialize
	 */
	public abstract void prepare();

	/**
	 * Compute forecast for the sequential algorithm. The mainModel is run automatically.
	 * @param observations The stoch.observer
	 * @param targetTime Time stamp to compute to
	 */
	public abstract void forecast(IStochObserver observations, ITime targetTime);

	/**
	 * Assimilate the selected observations.
	 * It is required to update the mainModel as well, since this will be used for output.
	 * @param observations The stoch.observer
	 * @param obsValues Values for the observations as a vector
	 * @param predictions Model values corresponding to the observations before the analysis step
	 * @param mainModel Main model for output (as defined in AbstractSequentialAlgorithm)
	 * @param analysisTime timelabel for the analysis (not necessarilly the time of the observations)
	 */
	public abstract void analysis(IStochObserver observations, IVector obsValues, 
			IVector predictions, IStochModelInstance mainModel, ITime analysisTime);
	
	/**
	 * Finish the algorithm. Let it call the last model(ensemble's) finish method
	 * Finish the stochObserver.
	 */
	public void finish() {
		if (this.mainModel != null) {
			mainModel.finish();
			mainModel = null;
		}
	}

	public void initialize(File workingDir, String[] arguments) {
		this.workingDir = workingDir;
		this.configString = arguments[0];

		/*
		 * parse xml configuration3333
		 */
		Results.putMessage("configstring = " + this.configString);
		this.configurationAsTree = new ConfigTree(workingDir, configString);

		// config for analysis times
		// default	<analysisTimes type="fromObservationTimes" ></analysisTimes>
		// or	<analysisTimes type="fixed" timeFormat="dateTimeString" >201008241130,201008241140,...,201008242350</analysisTimes>
		// or	<analysisTimes type="fixed" >201008240000,201008240300,201008240600</analysisTimes>
		// or	<analysisTimes type="fixed" timeFormat="mjd" >48259.0,48259.125,48259.25</analysisTimes>

        // Additional options to skip first or last assimilation time instance
		// <analysisTimes skipAtInitialTime="true" skipAtFinalTime="true" type="fixed" continueWithoutObservation="true" timeFormat="mjd" >48259.0,48259.125,48259.25</analysisTimes>

		String analysisTimesType = this.configurationAsTree.getAsString("analysisTimes@type", "fromObservationTimes");
		Results.putMessage("analysisTimes@type="+analysisTimesType);
		this.stepsFromObserver = analysisTimesType.equals("fromObservationTimes");
		skipAtInitialTime = this.configurationAsTree.getAsBoolean("analysisTimes@skipAtInitialTime",false);
	    skipAtFinalTime   = this.configurationAsTree.getAsBoolean("analysisTimes@skipAtFinalTime",false);
		continueWithoutObservation   = this.configurationAsTree.getAsBoolean("analysisTimes@continueWithoutObservation",false);

		if(!this.stepsFromObserver){
			//parse sequence
			String sequenceString = this.configurationAsTree.getContentString("analysisTimes");
			Results.putMessage("analysisTimes@type="+analysisTimesType);
			String timeFormat = this.configurationAsTree.getAsString("analysisTimes@timeFormat", "dateTimeString");
			timeIncrement = this.configurationAsTree.getAsString("analysisTimes@timeIncrement","");
			timeOffset = this.configurationAsTree.getAsString("analysisTimes@timeOffset","0");
			timeUnit = this.configurationAsTree.getAsString("analysisTimes@timeUnit","");
			if (timeIncrement.contentEquals("")){
				if(timeFormat.equals("dateTimeString")){
					this.analysisTimes = Time.Mjds2Times(TimeUtils.dateTimeSequenceString2Mjd(sequenceString));
				}else{ //use Mjd
					this.analysisTimes = Time.Mjds2Times(TimeUtils.MjdSequenceString2Mjd(sequenceString));
				}
			} else {
				//do nothing, because this.analysisTimes are determined in the setStochComponents
				if (timeUnit.contentEquals("")){
					throw new RuntimeException("Parameter timeUnit is not specified in the algorithm configuration file." +
							" When parameter timeIncrement is specified, timeUnit should be specified."+
							" Supported time units are day, hour, minute, and second");
				}
			}
		}
		// mainModel
		// mainModel@stochparameter default false
		// mainModel@stochForcing default false
		// mainModel@stochInit default false
		this.stochParameter = this.configurationAsTree.getAsBoolean("mainModel@stochParameter",this.stochParameter);
		Results.putMessage("mainModel@stochParameter="+this.stochParameter);
		this.stochForcing = this.configurationAsTree.getAsBoolean("mainModel@stochForcing",this.stochForcing);
		Results.putMessage("mainModel@stochForcing="+this.stochForcing);
		this.stochInit = this.configurationAsTree.getAsBoolean("mainModel@stochInit",this.stochInit);
		Results.putMessage("mainModel@stochInit="+this.stochInit);
	}

	/**
	 * Initialization function. Is called after initialize and before prepare.
	 * Sets the obs and model.
	 * @param stochObserver The stoch.observer
	 * @param stochModelFactory The stoch.model factory
	 */
	public void setStochComponents(IStochObserver stochObserver, IStochModelFactory stochModelFactory){
		this.stochObserver = stochObserver;
		this.stochModelFactory = stochModelFactory;

		// model for collecting output
		Results.putMessage("Creating mainModel");
		this.mainModel = this.stochModelFactory.getInstance(IStochModelFactory.OutputLevel.ModelDefault);
		
		if(this.stochInit){
			IStochVector stochInit = this.mainModel.getStateUncertainty();
			IVector init = stochInit.createRealization();
			this.mainModel.axpyOnState(1.0, init);
			init.free();
			Results.putMessage("   Add noise to initial state");
		}
		if(this.stochParameter){
			IStochVector stochPars = this.mainModel.getParameterUncertainty();
			IVector pars = stochPars.createRealization();
			this.mainModel.setParameters(pars);
			pars.free();
			Results.putMessage("   Add noise to parameters p="+pars);
		}
		if(this.stochForcing){
			this.mainModel.setAutomaticNoiseGeneration(true);
			Results.putMessage("   Add noise to forcing");
		}else{
			this.mainModel.setAutomaticNoiseGeneration(false);
			Results.putMessage("   Do not add noise to forcing");
		}

		this.setCurrentState(this.mainModel.getState());

		this.modelSpan = this.mainModel.getTimeHorizon();
		this.currentTime = this.mainModel.getCurrentTime();
		this.finalTime = modelSpan.getEndTime();

		Results.putMessage("model time " + this.currentTime + " " + this.finalTime);

		// figure out analysis times
		if(this.stepsFromObserver){
			ITime selectionSpan = (new Time(modelSpan)).extendInterval(OdaGlobSettings.getTimePrecision());
			this.analysisTimes = this.stochObserver.createSelection(selectionSpan).getTimes();
			if (continueWithoutObservation) {
				Results.putProgression("analysisTimes acquired from OBSERVER.");
				if (this.analysisTimes == null || this.analysisTimes.length == 0) {
					Results.putProgression("Algorithm will continue even without observation.");
				}
			} else {
				if (this.analysisTimes == null || this.analysisTimes.length == 0) {
					throw new RuntimeException("No analysisTimes found in OBSERVER for model time span " +
							selectionSpan.toString());
				}
				Results.putMessage("analysisTimes acquired from OBSERVER:" + this.analysisTimes[0].getMJD()
						+" -- "+this.analysisTimes[this.analysisTimes.length-1].getMJD() );
			}
		}else{
			if (!timeIncrement.contentEquals("")){
				double deltaAnalysisTime = Double.parseDouble(this.timeIncrement);
				double startOffset = Double.parseDouble(this.timeOffset);
				if (this.timeUnit.contentEquals("day")){
					// timeUnit is already consistent with the other time variables, do nothing here.
				} else if (this.timeUnit.contentEquals("hour")){
						double hourToDay = 1.0d / 24.0d;
						deltaAnalysisTime = deltaAnalysisTime * hourToDay;
						startOffset = startOffset * hourToDay;
				} else if(this.timeUnit.contentEquals("minute")){
					double minuteToDay = 1/ 60 / 24;
					deltaAnalysisTime = deltaAnalysisTime * minuteToDay;
					startOffset = startOffset * minuteToDay;
				} else if(this.timeUnit.contentEquals("second")){
					double secondToDay = 1 / 60 / 60 / 24;
					deltaAnalysisTime = deltaAnalysisTime * secondToDay;
					startOffset = startOffset * secondToDay;
				} else {
					throw new RuntimeException("timeUnit " + this.timeUnit + " for analyses times is supported. "+
							"Supported units are day, hour, minute, and second");
				}
				double startTime = modelSpan.getBeginTime().getMJD();
				double endTime = modelSpan.getEndTime().getMJD();
				int nAnalysisTime = (int) (Math.floor((endTime-startTime-startOffset)/deltaAnalysisTime)+1);
				double[] analysisTimes = new double[nAnalysisTime];
				for (int iAnalysisTime=0; iAnalysisTime<nAnalysisTime; iAnalysisTime++){
					analysisTimes[iAnalysisTime] = startTime+startOffset+iAnalysisTime*deltaAnalysisTime;
				}
				this.analysisTimes = Time.Mjds2Times(analysisTimes);
			} else {
				this.analysisTimes = Time.createSelection(this.analysisTimes,modelSpan);
			}
			if (this.analysisTimes == null || this.analysisTimes.length == 0) {
				throw new RuntimeException("No analysisTimes found in model time span " +
						modelSpan.toString());
			}
			Results.putMessage("analysisTimes acquired from MODEL:" + this.analysisTimes[0].getMJD()
					+" -- "+this.analysisTimes[this.analysisTimes.length-1].getMJD() );
		}
		
		// If necessary, add a final forecast step in order to forecast to model end time.
        if (this.analysisTimes != null) {
            this.nSteps = this.analysisTimes.length;
        } else { // no observations
            this.nSteps = 0;
        }

		if ( this.nSteps > 0 ){
			int n=this.nSteps;
			if ((this.finalTime.getMJD() - this.analysisTimes[n-1].getMJD()) > getTimePrecision()){
				// Add an additional analysis time and step
				ITime temp[] = new ITime[n+1];
				for(int i=0;i<n;i++){
					temp[i]=this.analysisTimes[i];
					this.analysisTimes[i]=null;
				}
				temp[n]=this.finalTime;
				this.analysisTimes=temp;
				this.nSteps = this.analysisTimes.length;
			}
		} else {
			if (!continueWithoutObservation) {
				throw new RuntimeException("No analysisTimes found in model time span " +
						modelSpan.toString() + ", while continueWithoutObservation=false (default).");
			}
			// Add one additional step and set FinalStepNoAnalysis
			this.FinalStepNoAnalysis=false;
			if ((this.finalTime.getMJD()-modelSpan.getBeginTime().getMJD())>getTimePrecision()){
				this.nSteps++;
				this.FinalStepNoAnalysis=true;
			}
		}
	}

	/**
	 * Main routine this method is called to start the computation
	 */
	public void run() {
		while (this.hasNext()) {
			this.next();
		}
		System.out.println("done");
	}

	/**
	 * Are there any more steps for this algorithm
	 *
	 * @return has next step
	 */
	public boolean hasNext() {
		return this.thisStep < this.nSteps;
	}

	/**
	 * Run next step of the algorithm
	 */
	public void next() {
		if (timerNext==null){
			timerNext              = new OdaTiming("Timesteps");
		    timerNext_ObsSel       = new OdaTiming("Select Obs");
		    timerNext_Compute      = new OdaTiming("compute main model");
		    timerNext_forecast     = new OdaTiming("forecast");
			timerNext_getState     = new OdaTiming("getState");
			timerNext_getObsDescr  = new OdaTiming("getObsDescr");
			timerNext_getObsVal    = new OdaTiming("getObsVal");
			timerNext_resultWriter = new OdaTiming("resultWriter");
			timerNext_analysis     = new OdaTiming("analysis");

			timerNext.AddSubTimer(timerNext_ObsSel);
			timerNext.AddSubTimer(timerNext_Compute);
			timerNext.AddSubTimer(timerNext_forecast);
            timerNext.AddSubTimer(timerNext_getState);
			timerNext.AddSubTimer(timerNext_getObsDescr);
			timerNext.AddSubTimer(timerNext_getObsVal);
			timerNext.AddSubTimer(timerNext_resultWriter);
			timerNext.AddSubTimer(timerNext_analysis);
		}

		timerNext.start();

		if (this.thisStep == this.nSteps || this.FinalStepNoAnalysis) { // final forecast
			ITime time = this.finalTime;
			ITime selectionStart = this.currentTime;
			ITime selectionEnd = this.finalTime;
			Time selectionSpan =new Time(selectionStart,selectionEnd);
			timerNext_ObsSel.start();
			IStochObserver selection = this.stochObserver.createSelection(selectionSpan);
			timerNext_ObsSel.stop();
			// Also do the final forecast of the background run!
			timerNext_Compute.start();
			this.mainModel.compute(time);
			timerNext_Compute.stop();
			if(this.finalTime.after(this.currentTime)){
				timerNext_forecast.start();
				this.forecast(selection,time);
				timerNext_forecast.stop();
			}
			if (continueWithoutObservation) {
				this.currentTime = time; // NOTE: may be slightly different from mod.getCurrentTime()
			}
		} else if (this.thisStep < this.nSteps) { // more data to process
			// select proper subset of observations
			double timePrecision = OdaGlobSettings.getTimePrecision();
			ITime time = this.analysisTimes[this.thisStep];
			ITime selectionStart = time;
			ITime selectionEnd = time;
			if(!this.stepsFromObserver){
				if(this.thisStep==0){
					selectionStart=this.currentTime;
				}else{
					double prevAnalysisTime = this.analysisTimes[this.thisStep-1].getMJD();
					selectionStart = new Time(prevAnalysisTime+0.5d*timePrecision,prevAnalysisTime+0.5d*timePrecision,0.0d);
				}
			}
			Time selectionSpan =new Time(selectionStart,selectionEnd);
			if(selectionSpan.durationInDays()<timePrecision){
				selectionSpan = selectionSpan.extendInterval(timePrecision);
			}
			timerNext_ObsSel.start();
			IStochObserver selection = this.stochObserver.createSelection(selectionSpan);
            timerNext_ObsSel.stop();
			//
			// forecast until next time
			//
			if(time.after(this.currentTime)){
				Results.putProgression("========================================================================\n");
				Results.putProgression(" Forecast from " + TimeUtils.mjdToString(this.currentTime.getMJD()) +"UTC "
						+ " to "+ TimeUtils.mjdToString(time.getMJD()) +"UTC "+ " ("+this.currentTime.getMJD()+"-->"+time.getMJD()+") \n");
				Results.putProgression("========================================================================\n");

				//		    System.out.println(" Computing main model");
				this.mainModel.announceObservedValues(selection.getObservationDescriptions());
				if(time.isSpan()){
					time = new Time(time.getMJD());
				}
				timerNext_Compute.start();
				this.mainModel.compute(time);
				timerNext_Compute.stop();
				Results.putProgression("- mainModel \n");
				timerNext_forecast.start();
				this.forecast(selection,time); // NOTE: the model generally computes until the timestep closest to time.
				timerNext_forecast.stop();

			}
			this.currentTime = time; // NOTE: may be slightly different from mod.getCurrentTime()

			if (! OdaGlobSettings.getProductionRun()) {
				timerNext_getState.start();
				this.setCurrentState(this.mainModel.getState());
				timerNext_getState.stop();
				timerNext_resultWriter.start();
                Results.putValue("x_f_central", this.currentState, this.currentState.getSize(), "forecast step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
				timerNext_resultWriter.stop();
				Results.putIterationReport(this, this.iteration, this.cost, this.currentState);
			}
			else {
				System.out.println("Production run skipping output of x_f_central");
			}

			this.iteration++;
			//
			// analysis or assimilation of observations
			//
			if(selection.getCount()>0 && ! this.skipAssimilation()){
				Results.putProgression("========================================================================\n");
				Results.putProgression(" analysis at " + TimeUtils.mjdToString(this.currentTime.getMJD()) + "UTC "
						+"("+this.currentTime.getMJD()+") \n");
				Results.putProgression("========================================================================\n");
                Results.putValue("analysis_time", this.currentTime.getMJD(), 1, "analysis step", IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Step);
				timerNext_getObsDescr.start();
				IObservationDescriptions descriptions = selection.getObservationDescriptions();
				timerNext_getObsDescr.stop();
				timerNext_getObsVal.start();
				IVector pred_f = this.mainModel.getObservedValues(descriptions);
                timerNext_getObsVal.stop();
                Results.putValue("pred_f_central", pred_f, pred_f.getSize(), "analysis step", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Step);
				IVector obsValues = selection.getExpectations();
                Results.putValue("obs", obsValues, obsValues.getSize(), "analysis step", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Step);

				// delegates the actual analysis to derived types
				timerNext_analysis.start();
				this.analysis(selection,obsValues,pred_f,this.mainModel,time);
				timerNext_analysis.stop();

				if (! OdaGlobSettings.getProductionRun()) {
					timerNext_getState.start();
					this.setCurrentState(this.mainModel.getState());
					timerNext_getState.stop();
					timerNext_resultWriter.start();
                    Results.putValue("x_a", currentState, currentState.getSize(), "analysis step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
				    timerNext_resultWriter.stop();
				}
				else {
					System.out.println("Production run skipping output of x_a");
				}
				timerNext_getObsDescr.start();
				descriptions = selection.getObservationDescriptions();
				timerNext_getObsDescr.stop();
				timerNext_getObsVal.start();
				IVector pred_a = this.mainModel.getObservedValues(descriptions);
				timerNext_getObsVal.stop();
                Results.putValue("pred_a_central", pred_a, pred_a.getSize(), "analysis step", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Step);
				// x_a(0.0) = [0.7934845787950873,-0.2203770147469745]
				// x_a(1.0) = [0.062480816290748115,-1.162523726176716]
				// x_a(2.0) = [-0.63087340375669,-0.031969773366867724]
				// x_f(3.0) = [-0.06523415909048982,0.8794164975160441]
				// x_a(3.0) = [-0.05274006691241355,0.8734518283428025]
				// x_f(4.0) = [0.4883251128495437,0.015644647888595195]
				//  obs(4) time,index,value,std 4.0,0.0,0.484356631972216,0.1
				// x_a(4.0) = [0.487583754493449,0.001979067139630265] !!!!DIFFERENT
				// x_a(5.0) = [0.04728113014269084,-0.7364495663857354]
				// x_a(6.0) = [-0.3680801099801977,-0.014517524380774596]
				// x_a(7.0) = [-0.024511614880335753,0.4585507366976051]
				// x_a(8.0) = [0.3004959006687179,-0.03516144887136381]
				// x_a(9.0) = [0.03089998185149922,-0.4218756611976039]
				// x_a(10.0) = [-0.23157396137433794,0.02820564537471584]
				timerNext_getState.start();
				this.setCurrentState(this.mainModel.getState());
				timerNext_getState.stop();
				timerNext_resultWriter.start();
                Results.putValue("x_f_central", this.currentState, this.currentState.getSize(), "analysis step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
				timerNext_resultWriter.stop();
				Results.putIterationReport(this, this.iteration, this.cost, this.currentState);

			}
			//this.currentState = this.model.getState(); //TODO get a consistent state
			Results.putIterationReport(this, this.iteration, this.cost, this.currentState);
			this.iteration++;
			// possibly write restart file
		}
		this.thisStep++;
		System.gc();
		timerNext.stop();
	}

	public IVector getState(){
		return this.mainModel.getState();
	}

	// Determine whether this assimilation step must be skipped
	// (optional user input for first and last time instance)
	private boolean skipAssimilation(){
	if (skipAtInitialTime){
		   if (this.thisStep==0) {
			   Results.putProgression("Skip Analysis at initial time\n");
			   return true;
		   }
		}
		if (skipAtFinalTime){
		   if (this.thisStep==this.nSteps-1){
			   Results.putProgression("Skip Analysis at final time\n");
			   return true;
		   }
		}
		return false;
	}

	//
	// Restarts
	//
	public IModelState saveInternalState() {
		// Create model instance state files, and add them state files to model state
		String timeLabel = TimeUtils.mjdToString(this.currentTime.getMJD());
		String restartTempDirName = ALGORITHM_RESTART_TEMP_DIR_PREFIX + timeLabel;
		File restartTempDir = new File(this.workingDir,restartTempDirName);
		if(restartTempDir.exists()){
			Results.putProgression("Removing temporary directory for restart files: "+restartTempDir.getAbsolutePath());
			for(File file:restartTempDir.listFiles()){
				file.delete();
			}
			restartTempDir.delete();
		}
		Results.putProgression("Create temporary directory for restart files: "+restartTempDir.getAbsolutePath());
		if(!restartTempDir.mkdirs()){
			throw new RuntimeException("Could not create temporary directory for restart files: "+restartTempDirName);
		}
		// combine instance files into one algorithm state 
		FileBasedModelState algorithmState = new FileBasedModelState();
		algorithmState.setDirContainingModelstateFiles(restartTempDir.getAbsoluteFile());
		//mainModel
		IModelState mainModelState = this.mainModel.saveInternalState();
		File mainModelRestartFile = new File(restartTempDir, MAIN_MODEL_RESTART_FILE_PREFIX + timeLabel + ".zip");
		mainModelState.savePersistentState(mainModelRestartFile.getAbsoluteFile());
		this.mainModel.releaseInternalState(mainModelState);
		// mainModelRestartFile may have gotten a model dependent extension, so list files in directory
		File[] filelist = restartTempDir.listFiles();
	    for (File file : filelist) {
			algorithmState.addFile(file.getAbsoluteFile());
		}
		//instances
		for (String instanceName: this.restartData.keySet()) {
			System.out.println("saving state for id="+instanceName);
			IStochModelInstance instance = this.restartData.get(instanceName);
			IModelState instanceState = instance.saveInternalState();
			File instanceRestartFile = new File(restartTempDir, ENSEMBLE_MEMBER_RESTART_FILE_PREFIX + timeLabel + "_" + instanceName + ".zip");
			instanceState.savePersistentState(instanceRestartFile.getAbsoluteFile());
			instance.releaseInternalState(instanceState);
			algorithmState.addFile(instanceRestartFile.getAbsoluteFile());
		}
		return algorithmState;
	}
	
	public void restoreInternalState(IModelState savedInternalState) {
		// first unzip the algorithm state file
		if (!(savedInternalState instanceof FileBasedModelState)) {
			throw new IllegalArgumentException("Unknown state type (" + savedInternalState.getClass().getName() +
					" for " + this.getClass().getName() + ".restoreInternalState");
		}
		FileBasedModelState modelState = (FileBasedModelState) savedInternalState;
		//unpack zip to temporary directory
		String restartTempDirName = ALGORITHM_RESTART_TEMP_DIR_PREFIX + "restore";
		File restartTempDir = new File(this.workingDir,restartTempDirName);
		if(restartTempDir.exists()){
			Results.putProgression("Removing temporary directory for restart files: "+restartTempDir.getAbsolutePath());
			for(File file:restartTempDir.listFiles()){
				file.delete();
			}
			restartTempDir.delete();
		}
		Results.putProgression("Create temporary directory for restart files: "+restartTempDir.getAbsolutePath());
		if(!restartTempDir.mkdirs()){
			throw new RuntimeException("Could not create temporary directory for restart files: " + restartTempDir.getAbsolutePath());
		}

		modelState.setDirContainingModelstateFiles(restartTempDir.getAbsoluteFile());
		modelState.restoreState();

		// mainModel
		FileFilter mainModelRestartFileFilter = new PrefixFileFilter(MAIN_MODEL_RESTART_FILE_PREFIX);
		File tempFiles[] = restartTempDir.listFiles(mainModelRestartFileFilter);
		if(tempFiles.length!=1){
			for(int i = 0; i < tempFiles.length; i++){
				String fileName = tempFiles[i].getName();
				String parts[] = fileName.split("_");
				if (parts[2].startsWith("part")){
					parts[2] = parts[2].replaceAll("part\\d{4}","");
					String filename = parts[0]+"_"+parts[1]+parts[2];
					tempFiles[i] = new File(restartTempDir.getAbsolutePath(), filename);
				} else {
			    	throw new RuntimeException("was expecting " +tempFiles.length+ " parts of mainmodel to restore," +
							"but something is wrong in dir " + restartTempDir.getAbsolutePath());
				}
			}
		}
		File mainModelStateFile = tempFiles[0];
		Results.putMessage("Restoring restart file: "+mainModelStateFile.getAbsolutePath());
		IModelState mainModelState = this.mainModel.loadPersistentState(mainModelStateFile);
		this.mainModel.restoreInternalState(mainModelState);
		this.setCurrentState(this.mainModel.getState());

		// ensemble
		FileFilter ensembleMemberRestartFileFilter = new PrefixFileFilter(ENSEMBLE_MEMBER_RESTART_FILE_PREFIX);
		tempFiles = restartTempDir.listFiles(ensembleMemberRestartFileFilter);
		if(tempFiles.length>=2){
			String timeString = null;
			for(File file: tempFiles){
				String fileName = file.getName();
				if (fileName.toUpperCase().contains("COLD_START") || fileName.toUpperCase().contains("COLD START")) {
					throw new RuntimeException(getClass().getSimpleName()
							+ ": names of restart files are not allowed to contain 'COLD_START' or 'COLD START'."
							+ " Instead use 'COLDSTART' (without the underscore and without the space) in name of restart file "
							+ file.getAbsolutePath());
				}

				String parts[] = fileName.split("_");
				//remove file extension.
				parts[3] = parts[3].substring(0, parts[3].lastIndexOf("."));
				String key=parts[2]+"_"+parts[3];
				timeString = parts[1];
				//for a warm state start start run at the time of the restart state, so update this.currentTime.
				//for a cold state start start run at the startTime of the timeHorizon, so do not change this.currentTime.
				if (!BBUtils.isColdStart(timeString)) {
					try {
						this.currentTime = new Time(TimeUtils.date2Mjd(timeString));
					} catch (ParseException e) {
						throw new RuntimeException("Problems parsing time '" + timeString + "' in name of restart file " + file.getAbsolutePath()
								+ ". Message was: " + e.getMessage(), e);
					}
				}

				IStochModelInstance member = this.restartData.get(key);
				if(member==null){
					throw new RuntimeException("Restart file found for member '" + key
							+ "' but this member is not present in this ensemble run. Either change configuration or change restart files. See "
							+ restartTempDir.getAbsolutePath());
				}
				Results.putMessage("Restoring restart file: "+file.getAbsolutePath());
				IModelState memberState = member.loadPersistentState(file);
				member.restoreInternalState(memberState);
			}

			//for a warm state start start run at the time of the restart state, so update this.currentTime.
			//for a cold state start start run at the startTime of the timeHorizon, so do not change this.currentTime.
			if (!BBUtils.isColdStart(timeString)) {
				try {
					Results.putMessage("Setting algorithm time to: "+timeString);
					this.currentTime = new Time(TimeUtils.date2Mjd(timeString));
				} catch (ParseException e) {
					throw new RuntimeException("Problems parsing time '" + timeString + "' from restart files in dir "
							+ restartTempDir.getAbsolutePath() + ". Message was: " + e.getMessage(), e);
				}
			}
		}

		// find correct algorithm step
		this.thisStep=0;
		if (!this.FinalStepNoAnalysis) {
		   while((thisStep<this.nSteps)&&(this.currentTime.after(this.analysisTimes[this.thisStep]))){
			   this.thisStep++;
		   }
		}

		//remove temporary directory
		if(restartTempDir.exists()){
			Results.putProgression("Remove temporary directory for restart files after reading: "+restartTempDir.getAbsolutePath());
			for(File file:restartTempDir.listFiles()){
				file.delete();
			}
			restartTempDir.delete();
		}
	}

	public void releaseInternalState(IModelState savedInternalState) {
		if(savedInternalState instanceof FileBasedModelState){
			FileBasedModelState state = (FileBasedModelState)savedInternalState;
			File restartTempDir = state.getDirContainingModelStateFiles();
			if(restartTempDir.exists()){
				Results.putProgression("Removing temporary directory for restart files: "+restartTempDir.getAbsolutePath());
				for(File file:restartTempDir.listFiles()){
					file.delete();
				}
				restartTempDir.delete();
			}
		}
	}

	public IModelState loadPersistentState(File algorithmStateFile) {
		FileBasedModelState modelState = new FileBasedModelState();
		modelState.setZippedStateFile(algorithmStateFile);
		return modelState;
	}

	public class PrefixFileFilter implements FileFilter{
		String prefix=null;
		public PrefixFileFilter(String prefix){
			this.prefix = prefix;
		}
		public boolean accept(File pathname) {
			return pathname.getName().startsWith(prefix);
		}
	}
	
	
	public ITime getTimeHorizon() {
		return this.modelSpan;
	}

	
	public ITime getCurrentTime() {
		return this.currentTime;
	}

	
	public void compute(ITime targetTime) {
		while(this.hasNext() && targetTime.after(this.currentTime)){
			this.next();
		}
	}

	public IStochModelInstance getMainModel() {
		return mainModel;
	}

	public void setCurrentState(IVector state){
	   // Explicitly free currentState to avoid memory leaks
	   // in case of lazy GC and native implementations
	   	if (this.currentState!=null) this.currentState.free();
		this.currentState=state;
	}

	public IVector getCurrentState(){
		   // Explicitly free currentState to avoid memory leaks
		   // in case of lazy GC and native implementations
			return this.currentState;
	}


}
