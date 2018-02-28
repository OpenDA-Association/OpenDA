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
package org.openda.algorithms.kalmanFilter;

import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.*;
import org.openda.utils.Results;
import org.openda.utils.Time;
import org.openda.utils.io.KalmanGainStorage.StorageType;
import org.openda.utils.performance.OdaGlobSettings;
import org.openda.utils.performance.OdaTiming;

import java.io.File;

/**
 * @author Martin Verlaan
 * Implements the common parts for Ensemble-based sequential methods, such as the Ensemble Kalman filter.
 */
public abstract class AbstractSequentialEnsembleAlgorithm extends AbstractSequentialAlgorithm {

	// class data
	String TimerID="Ens Forecast";
	OdaTiming timerForecast;
	OdaTiming timerForecast_s1, timerForecast_s2, timerForecast_s3, timerForecast_s4;


	int ensembleSize = 100;
	protected IStochModelInstance ensemble[] = null;

	public boolean ensembleStochParameter=false;
	public boolean ensembleStochForcing=true;
	public boolean ensembleStochInit=true;

	// some facilities for Kalman filtering
	// - the actual analysis is performed by the filter
	protected ITime saveGainTimes[] = null; //save Kalman gain at these times
	protected String gainStorageDirPrefix = "kgStorage_";
	protected String gainStorageXmlFileName = "kalmanGainStorage.xml";
	protected StorageType gainStorageFileType = StorageType.automatic;
	protected int gainStorageMaxXmlStore = 40;

	// Localization
	enum LocalizationMethodType{none,hamill,autoZhang}
	LocalizationMethodType localizationMethod=LocalizationMethodType.none;
    double distance;

	//Smoothed gain matrix
	double timeRegularisationPerDay = 0.0;


	
	public void initialize(File workingDir, String[] arguments) {

		super.initialize(workingDir,arguments);

		this.ensembleSize = this.configurationAsTree.getAsInt("ensembleSize",this.ensembleSize);
	    String localization = this.configurationAsTree.getAsString("localization","none");
		System.out.println("Selected localization method:"+localization);
		if ( localization.toLowerCase().equals("none") ) {
			this.localizationMethod = LocalizationMethodType.none;
		}
		else if (localization.toLowerCase().equals("hamill")){
		   this.localizationMethod = LocalizationMethodType.hamill;
           this.distance = this.configurationAsTree.getAsDouble("distance",0.0);
           if (this.distance <= 0.0) throw new RuntimeException("Found Localization method 'Hamill' but no 'distance' (> 0) specified.");
		}
		else if (localization.toLowerCase().equals("autozhang")){
			this.localizationMethod = LocalizationMethodType.autoZhang;
		}
		else {
			throw new RuntimeException("Configured localization type '" + localization +  "' is not supported");
		}


		this.timeRegularisationPerDay= this.configurationAsTree.getAsDouble("gainMatrixSmoother@timeRegularisationPerDay",0.0);

		Results.putMessage("this.ensembleSize="+this.ensembleSize);
        if (this.ensembleSize < 2) throw new RuntimeException("Found ensemble size " + this.ensembleSize + ". This value must be at least 2.");

		// ensembleModel
		// ensembleModel@stochparameter default false
		// ensembleModel@stochForcing default true
		// ensembleModel@stochInit default true
		this.ensembleStochParameter = this.configurationAsTree.getAsBoolean("ensembleModel@stochParameter",this.ensembleStochParameter);
		Results.putMessage("ensembleModel@stochParameter="+this.ensembleStochParameter);
		this.ensembleStochForcing = this.configurationAsTree.getAsBoolean("ensembleModel@stochForcing",this.ensembleStochForcing);
		Results.putMessage("ensembleModel@stochForcing="+this.ensembleStochForcing);
		this.ensembleStochInit = this.configurationAsTree.getAsBoolean("ensembleModel@stochInit",this.ensembleStochInit);
		Results.putMessage("ensembleModel@stochInit="+this.ensembleStochInit);

		// config for times for saving kalman gain
		// default	<saveGain><times type="none" ></times></saveGain>
		// or	<saveGain><times type="fixed" timeFormat="dateTimeString" >201008241130,201008241140,...,201008242350<\times></saveGain>
		// or	<saveGain><times type="fixed" >201008240000,201008240300,201008240600<\times></saveGain>
		// or	<saveGain><times type="fixed" timeFormat="mjd" >48259.0,48259.125,48259.25<\times></saveGain>
		//
		// <saveGain>
		//    <times type="fixed" timeFormat="mjd" >0.0,5.0,10.0</times>
		//    <file dirPrefix="kgStorage_" fileName="kalmanGainStorage.xml" fileType="automatic" xmlTypeMaxSize="40" />
		//      or
		//      <file dirPrefix="kgStorage_" fileName="kalmanGainStorage.xml" fileType="xml" />
		//      <file dirPrefix="kgStorage_" fileName="kalmanGainStorage.xml" fileType="netcdf" />
		// </saveGain>
		String saveGainTimesType = this.configurationAsTree.getAsString("saveGain/times@type", "none");
		Results.putMessage("saveGain/times@type="+saveGainTimesType);
		boolean fixedTimes = saveGainTimesType.equals("fixed");
		if(fixedTimes){
			//parse sequence
			String sequenceString = this.configurationAsTree.getContentString("saveGain/times");
			Results.putMessage("saveGain/times@type="+saveGainTimesType);
			String timeFormat = this.configurationAsTree.getAsString("saveGain/times@timeFormat", "dateTimeString");
			if(timeFormat.equals("dateTimeString")){
				this.saveGainTimes = Time.Mjds2Times(TimeUtils.dateTimeSequenceString2Mjd(sequenceString));
			}else{ //use Mjd
				this.saveGainTimes = Time.Mjds2Times(TimeUtils.MjdSequenceString2Mjd(sequenceString));
			}
			//    <file dirPrefix="kgStorage_" fileName="kalmanGainStorage.xml" fileType="automatic" xmlTypeMaxSize="40" />
			//      or
			//      <file dirPrefix="kgStorage_" fileName="kalmanGainStorage.xml" fileType="xml" />
			//      <file dirPrefix="kgStorage_" fileName="kalmanGainStorage.xml" fileType="netcdf" />
			String storageTypeString=this.configurationAsTree.getAsString("saveGain/file@fileType", this.gainStorageFileType.toString());
			if(storageTypeString.equalsIgnoreCase("netcdf")){this.gainStorageFileType=StorageType.netcdf;}
			if(storageTypeString.equalsIgnoreCase("xml")){this.gainStorageFileType=StorageType.xml;}
			Results.putMessage("saveGain/file@fileType="+this.gainStorageFileType);
			this.gainStorageDirPrefix=this.configurationAsTree.getAsString("saveGain/file@dirPrefix", this.gainStorageDirPrefix);
			Results.putMessage("saveGain/file@dirPrefix="+gainStorageDirPrefix);
			this.gainStorageXmlFileName=this.configurationAsTree.getAsString("saveGain/file@fileName", this.gainStorageXmlFileName);
			Results.putMessage("saveGain/file@fileName="+gainStorageXmlFileName);
			this.gainStorageMaxXmlStore=this.configurationAsTree.getAsInt("saveGain/file@xmlTypeMaxSize", this.gainStorageMaxXmlStore);
			Results.putMessage("saveGain/file@xmlTypeMaxSize="+gainStorageMaxXmlStore);
		}else{
			this.saveGainTimes = new Time[0];
		}
	}

	/**
	 * Intialization function. Is called after initialize and before prepare.
	 * Sets the obs and model.
	 * @param stochObserver The stoch.observer
	 * @param stochModelFactory The stoch.model factory
	 */
	public void setStochComponents(IStochObserver stochObserver, IStochModelFactory stochModelFactory){
		super.setStochComponents(stochObserver, stochModelFactory);
		// create ensemble of Models!!!
		this.ensemble = new IStochModelInstance[this.ensembleSize];
		for (int i = 0; i < this.ensembleSize; i++) {
			Results.putMessage("Creating ensemble model "+i);
			this.ensemble[i] = this.stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);

			if(this.ensembleStochInit){
				IStochVector eInit = this.ensemble[i].getStateUncertainty();
				if (eInit != null) {
					this.ensemble[i].axpyOnState(1.0, eInit.createRealization());
					Results.putMessage("   Add noise to initial state");
				}
			}

			if(this.ensembleStochParameter){
				IStochVector stochPars = this.ensemble[i].getParameterUncertainty();
				IVector pars = stochPars.createRealization();
				this.ensemble[i].setParameters(pars);
				Results.putMessage("   Add noise to parameters p="+pars);
			}
			if(this.ensembleStochForcing){
				this.ensemble[i].setAutomaticNoiseGeneration(true);
				Results.putMessage("   Add noise to forcing");
			}else{
				this.ensemble[i].setAutomaticNoiseGeneration(false);
			}

			// add to list for restarts
			this.restartData.put("member_"+i, this.ensemble[i]);
		}
	}

	
	public void prepare() {
	}

	
	public void forecast(IStochObserver observations, ITime targetTime) {
           System.gc();
		if (timerForecast == null) {
			timerForecast = new OdaTiming(TimerID+" Total");
			timerForecast_s1 = new OdaTiming(TimerID+" Announce");
			timerForecast_s2 = new OdaTiming(TimerID+" Compute");
			timerForecast_s3 = new OdaTiming(TimerID+" GetState");
			timerForecast_s4 = new OdaTiming(TimerID+" Result writing");
            timerForecast.AddSubTimer(timerForecast_s1);
			timerForecast.AddSubTimer(timerForecast_s2);
			timerForecast.AddSubTimer(timerForecast_s3);
			timerForecast.AddSubTimer(timerForecast_s4);
		}


		if (timerForecast_s1 == null) {timerForecast_s1 = new OdaTiming(TimerID+" Announce");}
		if (timerForecast_s2 == null) {timerForecast_s2 = new OdaTiming(TimerID+" Compute");}
		if (timerForecast_s3 == null) {timerForecast_s3 = new OdaTiming(TimerID+" GetState");}
		if (timerForecast_s4 == null) {timerForecast_s4 = new OdaTiming(TimerID+" Result writing");}

		timerForecast.start();

		// run each model until target time

		Results.putProgression("computation for member ("+this.ensembleSize+"):");

		for (int i = 0; i < this.ensembleSize; i++) {
		    timerForecast_s1.start();
			if(observations.getCount()>0){
				this.ensemble[i].announceObservedValues(observations.getObservationDescriptions());
			}
			timerForecast_s1.stop();
			timerForecast_s2.start();
			this.ensemble[i].compute(targetTime);
			timerForecast_s2.stop();
			timerForecast_s4.start();
			Results.putProgression("- member " + i);
			timerForecast_s4.stop();
		}

		// get the states from each model

		if (! OdaGlobSettings.getProductionRun()){
			for (int i = 0; i < this.ensembleSize; i++) {

				timerForecast_s3.start();
				IVector xi_i = this.ensemble[i].getState();
				timerForecast_s3.stop();
				timerForecast_s4.start();
                Results.putValue("xi_f_"+i, xi_i, xi_i.getSize(), "forecast step", IResultWriter.OutputLevel.All, IResultWriter.MessageType.Step);
				timerForecast_s4.stop();
		    	xi_i.free();
			}
		}
		else {
			System.out.println("PRODUCTION RUN: Get state skipped and xi_f_.. results are not written");
		}

		Results.putProgression("\n");
		timerForecast.stop();
	}

	// delegate further
	public abstract void analysis(IStochObserver obs, IVector obsValues, IVector predictions,
			IStochModelInstance mod, ITime analysisTime);

	public void finish() {
		super.finish();
		//finish the ensemble models (main model has already been finished in call to super.finish()).
		for (int i = 0, ensembleLength = this.ensemble.length; i < ensembleLength; i++) {
			if (this.ensemble[i] != null) {
				this.ensemble[i].finish();
				this.ensemble[i] = null;
			}
		}
           System.gc();
	}

	/*
	 *
	 * local utilities
	 *
	 */

	/*
	 * Compute the average of an ensemble of Vectors
	 */

	public static IVector ensembleAverage(IVector in[]) {
		IVector result = null;
		if (in.length > 0) {
			result = in[0].clone();
			for (int i = 1; i < in.length; i++) {
				result.axpy(1.0, in[i]);
			}
			result.scale(1.0 / in.length);
		}
		return result;
	}

	/*
	 * Compute the average of an ensemble of Vectors
	 */
	public static IVector ensembleAverage(IVector in[], int q) {
		IVector result = null;
		if(q>in.length){
			q=in.length;
		}
		if (q > 0) {
			result = in[0].clone();
			for (int i = 1; i < q; i++) {
				result.axpy(1.0, in[i]);
			}
			result.scale(1.0 / in.length);
		}
		return result;
	}


	/*
	 * Subtract a Vector with the average from each Vector in ensemble @param
	 * Vector[] inout : ensemble @param Vector avarage : vector with average to
	 * subtract
	 */
	public static void removeAverage(IVector[] inout, IVector average) {
		for (IVector anInout : inout) {
			anInout.axpy(-1.0, average);
		}
	}

	/*
	 * Compute standard-deviations for an ensemble (with average removed) @param
	 * Vector[] in : ensemble @return Vector std : vector with
	 * standard-deviations
	 */
	public static IVector ensembleStd(IVector[] in) {
		IVector avg = ensembleAverage(in);

//		if (avg instanceof Vector && in instanceof Vector[]){
//			System.out.println("Nieuwe snelle std");
//			return Vector.ensembleStd((Vector []) in, (Vector) avg);
//		}
//		else {
			//System.out.println("Nee niet snel de naam is "+in[0].getClass().getName());
			IVector result = null;
			int n = in.length;
			if (n > 0) {
				result = in[0].clone();
				result.setConstant(0.0);
				for (int i = 0; i < n; i++) {
					IVector temp = in[i].clone();
					temp.axpy(-1.0, avg);
					temp.pointwiseMultiply(temp);
					result.axpy(1.0, temp);
					temp.free();
				}
				result.scale(1.0 / (n - 1.0));
				result.sqrt();
			}
			avg.free();
			return result;
//		}
	}

	/*
		 * Compute standard-deviations for an ensemble with zero mean (mean is already removed)
		 * @param
		 * Vector[] in : ensemble @return Vector std : vector with
		 * standard-deviations
		 */
		public static IVector ensembleStdZeroMean(IVector[] in) {

				IVector result = null;
				int n = in.length;
				if (n > 0) {
					result = in[0].clone();
					result.setConstant(0.0);
					for (int i = 0; i < n; i++) {
						IVector temp = in[i].clone();
						temp.pointwiseMultiply(temp);
						result.axpy(1.0, temp);
						temp.free();
					}
					result.scale(1.0 / (n - 1.0));
					result.sqrt();
				}
				return result;
		}


	public boolean saveGainAtThisTime(ITime time){
		boolean result=false;
		if(this.saveGainTimes!=null){
			for (ITime saveGainTime : this.saveGainTimes) {
				double currentTime = (time.getBeginTime().getMJD() + time.getEndTime().getMJD()) / 2d;
				if (Math.abs(currentTime - saveGainTime.getMJD()) < OdaGlobSettings.getTimePrecision()) {
					result = true;
				}
			}
		}
		return result;
	}

}
