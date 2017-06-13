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
package org.openda.algorithms.kalmanFilter;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.*;
import org.openda.observers.ObserverUtils;
import org.openda.utils.ConfigTree;
import org.openda.utils.Results;
import org.openda.utils.io.KalmanGainStorage;
import org.openda.utils.performance.OdaGlobSettings;

import java.io.File;
import java.text.ParseException;

/**
 * Steady state Kalman filter implementation as introduced already in the original papers of Kalman and Bucy.
 * This implementation also allows asynchronous assimilation (obervations at multiple times are processed in one batch).
 * The steady state approach assumes a regular pattern of observations and a linear model. In practice, you can violate
 * these assumptions to some extent, but there are limits to what you can get away with.
 * 
 * Input syntax
 * 	<readGain>
 *		<dirPrefix>../gain_</dirPrefix>
 *		<time timeFormat="dateTimeString">201008240000</time>
 *		<!-- or <time timeFormat="mjd">10.0</time>
 *		<file>enkf_oscillator_gain.xml</file>
 *	</readGain>
 *
 * @author verlaanm
 *
 */
public class SteadyStateFilter extends AbstractSequentialAlgorithm {
	// config data for reading
	String gainDirPrefix = null;
	String gainFileName = null;
    double[] gainTimeMjd;
    double[] readGainTime;
    int steadyStateTimeCounter = 0;

	public void initialize(File workingDir, String[] arguments) {
		super.initialize(workingDir, arguments);
		/*
		 * Input syntax
		 * 	<readGain>
		 *		<dirPrefix>../gain_</dirPrefix>
		 *		<time timeFormat="dateTimeString" readGainTime="201008240000">201008240000</time>
		 *		<time timeFormat="dateTimeString" readGainTime="201008260000">201008300000</time>
		 *		<!-- or <time timeFormat="mjd" readGainTime="5.0">10.0</time>
		 *		<file>enkf_oscillator_gain.xml</file>
		 *	</readGain>
		 */
		gainDirPrefix = this.configurationAsTree.getContentString("readGain/dirPrefix");
		Results.putMessage("readGain/dirPrefix="+gainDirPrefix);
		if(gainDirPrefix!=null){
            ConfigTree partsSteadyState[] = this.configurationAsTree.getSubTrees("readGain/time");
            int nGain = partsSteadyState.length;
            this.gainTimeMjd = new double[nGain];

            // check if different Kalman gain at different times are required:
            String isVariousKalmanGain = this.configurationAsTree.getAsString("readGain/time@readGainTime", null);
            if (isVariousKalmanGain==null){
                this.readGainTime = null;
            } else {
                this.readGainTime = new double[nGain];
            }

            int i = 0;
            for (ConfigTree part : partsSteadyState){
                String timeFormat = part.getAsString("@timeFormat", "dateTimeString");
                Results.putMessage("readGain/time@timeFormat="+timeFormat);
                String timeSteadyStateSimulation = part.getAsString("@readGainTime", null);
                Results.putMessage("readGain/time@readGainTime="+timeSteadyStateSimulation);
                String timeString = part.getContentString("");
                Results.putMessage("readGain/time="+timeString);
                if(timeFormat.equals("dateTimeString")){
                    try {
                        this.gainTimeMjd[i] = TimeUtils.date2Mjd(timeString);
                    } catch (ParseException e) {
                        throw new RuntimeException("readGain/time expected a formatted time YYYYMMDDhhmm, but a parse error occurred :"
                                +timeString+"\n");
                    }
                }else{ //use Mjd
                    try {
                        this.gainTimeMjd[i] = Double.parseDouble(timeString);
                    } catch (NumberFormatException e) {
                        throw new RuntimeException("readGain/time expected a double for the Mjd, but a parse error occurred :"
                                +timeString+"\n");
                    }
                }

                if (isVariousKalmanGain!=null){
                    if(timeFormat.equals("dateTimeString")){
                        if (timeSteadyStateSimulation != null) {
                            Results.putMessage("readGain/time@readGainTime="+timeSteadyStateSimulation);
                            try {
                                this.readGainTime[i] = TimeUtils.date2Mjd(timeSteadyStateSimulation);
                            } catch (ParseException e) {
                                throw new RuntimeException("readGain/time@readGainTime expected a formatted time YYYYMMDDhhmm, but a parse error occurred :"
                                        +timeString+"\n");
                            }
                        } else {
                            throw new RuntimeException("readGain/time@readGainTime is expected, but it is not found at line\"+i+\".\n");
                        }
                    } else {
                        if (timeSteadyStateSimulation != null) {
                            Results.putMessage("readGain/time@readGainTime="+timeSteadyStateSimulation);
                            try {
                                this.readGainTime[i] = Double.parseDouble(timeSteadyStateSimulation);
                            } catch (NumberFormatException e) {
                                throw new RuntimeException("readGain/time@readGainTime expected a double for the Mjd, but a parse error occurred :"
                                        +timeString+"\n");
                            }
                        } else {
                            throw new RuntimeException("readGain/time@readGainTime is expected, but it is not found at line"+i+".\n");
                        }
                    }
                }

                String timeStampString = TimeUtils.mjdToString(this.gainTimeMjd[i]);
                    //MVL replaced: new SimpleDateFormat("yyyyMMdd_HHmmss").format(Time.timeStampToDate(gainTimeMjd));
                File gainDir = new File(this.workingDir,this.gainDirPrefix+timeStampString);
                if(!gainDir.isDirectory()){
                    throw new RuntimeException("Directory for storage of Kalman gain does not exist :"
                            +gainDir.getAbsolutePath()+"\n");
                }
                this.gainFileName = this.configurationAsTree.getContentString("readGain/file");
                Results.putMessage("readGain/configFile="+this.gainFileName);
                File gainFile = new File(gainDir,gainFileName);
                if(!gainFile.exists()){
                    throw new RuntimeException("File for Kalman gain storage was not found :"+gainFile.getAbsolutePath()+"\n");
                }
                i++;
            }
		}
		
		//now read the first gain file into memory
        steadyStateTimeCounter++;
		KalmanGainStorage gainStorage = new KalmanGainStorage(this.workingDir, this.gainTimeMjd[0]);
		gainStorage.setStorageDirPrefix(this.gainDirPrefix);
		gainStorage.setKalmanGainStorageXmlFileName(this.gainFileName);
		gainStorage.readKalmanGain( this.getCurrentState());
		String[] obsIds = gainStorage.getObservationIds();
		double[] obsTimeOffsets = gainStorage.getObservationOffsetInDays();
		IVector[] KVecs = gainStorage.getKalmanGainColumns();
		for(int i=0;i<obsIds.length;i++){
			String gainVectorId = obsIds[i]+":"+Math.round(obsTimeOffsets[i]*24.0*3600.0); //conversion to seconds
			this.gainVectors.put(gainVectorId, KVecs[i]);
			this.obsId.put(gainVectorId, obsIds[i]);
			this.obsTimeOffset.put(gainVectorId, 0.0);
		}
	}
	
	
	public void prepare() {
		// TODO Auto-generated method stub

	}

	
	public void forecast(IStochObserver observations, ITime targetTime) {
		// TODO nothing to do, because forecast of mainModel is done automatically

	}

	
	public void analysis(IStochObserver observations, IVector obsValues, IVector predictions,
			IStochModelInstance mainModel, ITime analysisTime) {
        // replace the Kalman gain if necessary
        if (isReplaceGainAtThisTime(analysisTime)){
            replaceKalmanGain();
        }
		// data for matching observations to columns of the Kalman gain
		ObserverUtils obsUtils = new ObserverUtils(observations);
		String[] obsIds = obsUtils.getObsIds();
		double[] obsTimeOffsets = obsUtils.getObsTimeOffsets(analysisTime.getMJD());
		// obs-pred
		IVector innovation = observations.getExpectations();
		innovation.axpy(-1.0, predictions);
		// x_a = x_f + K(y-H x_f)
		IVector x = this.getCurrentState();
		IVector delta = x.clone(); //vector of same type; content is overwritten
		delta.scale(0.0);
		
        Results.putValue("x_f", x, x.getSize(), "analysis step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
        Results.putValue("pred_f", predictions, predictions.getSize(), "analysis step", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Step);

		int m=innovation.getSize();
		for(int i=0;i<m;i++){
			// find matching column in steady-state gain
			String gainVectorId = obsIds[i]+":"+Math.round(obsTimeOffsets[i]*24.0*3600.0); //conversion to seconds
			Results.putProgression("processing obs "+gainVectorId+"\n");
			// add to analysis increment for this obs
			if(this.gainVectors.containsKey(gainVectorId)){
				delta.axpy(innovation.getValue(i), this.gainVectors.get(gainVectorId));
			}else{
				throw new RuntimeException("No matching column found for observation with id="
						+obsIds[i]+"and offset="+obsTimeOffsets[i]+"\n");
			}
		}
		this.mainModel.axpyOnState(1.0, delta);
		
		IVector pred_a = this.mainModel.getObservedValues(observations.getObservationDescriptions());
        Results.putValue("pred_a", pred_a, pred_a.getSize(), "analysis step", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Step);

	}

    private boolean isReplaceGainAtThisTime(ITime time){
        boolean result=false;
        if(this.readGainTime!=null){
            for(int i=0;i<this.readGainTime.length;i++){
                double currentTime = (time.getBeginTime().getMJD() + time.getEndTime().getMJD())/2d;
                if(Math.abs(currentTime-this.readGainTime[i])< OdaGlobSettings.getTimePrecision()){
                    result=true;
                    steadyStateTimeCounter=i;
                }
            }
        }
        return result;
    }

    private void replaceKalmanGain() {
        if (this.readGainTime!=null){
            KalmanGainStorage gainStorage = new KalmanGainStorage(this.workingDir, this.gainTimeMjd[steadyStateTimeCounter]);
            gainStorage.setStorageDirPrefix(this.gainDirPrefix);
            gainStorage.setKalmanGainStorageXmlFileName(this.gainFileName);
            gainStorage.readKalmanGain(this.getCurrentState());
            String[] obsIds = gainStorage.getObservationIds();
            double[] obsTimeOffsets = gainStorage.getObservationOffsetInDays();
            IVector[] KVecs = gainStorage.getKalmanGainColumns();
            for(int i=0;i<obsIds.length;i++){
                String gainVectorId = obsIds[i]+":"+Math.round(obsTimeOffsets[i]*24.0*3600.0); //conversion to seconds
                this.gainVectors.put(gainVectorId, KVecs[i]);
                this.obsId.put(gainVectorId, obsIds[i]);
                this.obsTimeOffset.put(gainVectorId, 0.0);
            }
        }
    }

}
