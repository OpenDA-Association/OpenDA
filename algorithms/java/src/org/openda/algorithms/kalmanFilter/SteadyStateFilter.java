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
package org.openda.algorithms.kalmanFilter;
import org.openda.exchange.timeseries.TimeUtils;
import org.openda.interfaces.*;
import org.openda.observers.ObserverUtils;
import org.openda.utils.ConfigTree;
import org.openda.utils.Matrix;
import org.openda.utils.Results;
import org.openda.utils.SortUtils;
import org.openda.utils.io.KalmanGainStorage;
import org.openda.utils.performance.OdaGlobSettings;

import java.io.File;
import java.text.ParseException;
import java.util.*;

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
	private String gainDirPrefix = null;
	private String gainFileName = null;
	private double[] gainTimeMjd;
	private double[] readGainTime;
	private int steadyStateTimeCounter = 0;
	private double skipAssimilationStandardDeviationFactor = Double.POSITIVE_INFINITY;
	private boolean estimateMissingObservations = false;
	private double[][] hk;
	private String[] gainStorageObservationIdsArray;
	private String[] gainVectorIdArray;

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
		this.skipAssimilationStandardDeviationFactor = this.configurationAsTree.getAsDouble("skipAssimilationStandardDeviationFactor", Double.POSITIVE_INFINITY);
		this.estimateMissingObservations = this.configurationAsTree.getAsBoolean("estimateMissingObservations", false);

		//now read the first gain file into memory
        steadyStateTimeCounter++;
		KalmanGainStorage gainStorage = new KalmanGainStorage(this.workingDir, this.gainTimeMjd[0]);
		gainStorage.setStorageDirPrefix(this.gainDirPrefix);
		gainStorage.setKalmanGainStorageFileName(this.gainFileName);
		if (gainFileName.endsWith(".nc")) gainStorage.setColumnFileType(KalmanGainStorage.StorageType.netcdf_cf);
		gainStorage.readKalmanGain( this.getCurrentState());
		hk = gainStorage.getHk();
		gainStorageObservationIdsArray = gainStorage.getObservationIds();
		double[] gainStorageObsTimeOffsets = gainStorage.getObservationOffsetInDays();
		IVector[] KVecs = gainStorage.getKalmanGainColumns();
		gainVectorIdArray = new String[gainStorageObservationIdsArray.length];
		for(int i = 0; i< gainStorageObservationIdsArray.length; i++){
			String gainVectorId = gainStorageObservationIdsArray[i] + ":" + Math.round(gainStorageObsTimeOffsets[i] * 24.0 * 3600.0); //conversion to seconds
			gainVectorIdArray[i] = gainVectorId;
			this.gainVectors.put(gainVectorId, KVecs[i]);
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
		String[] availableObsGainVectorIdArray = getAvailableObsGainVectorIdArray(obsIds, obsTimeOffsets);

		// obs-pred
		IVector observationValues = observations.getExpectations();
		IVector innovation = observationValues.clone();
		innovation.axpy(-1.0, predictions);
		IVector standardDeviations = observations.getStandardDeviations();
		// x_a = x_f + K(y-H x_f)
		IVector x = this.getCurrentState();
		IVector delta = x.clone(); //vector of same type; content is overwritten
		delta.scale(0.0);
		
        Results.putValue("x_f", x, x.getSize(), "analysis step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
        Results.putValue("pred_f", predictions, predictions.getSize(), "analysis step", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Step);

		int m=innovation.getSize();
		IVector pred_a = this.mainModel.getObservationOperator().getObservedValues(observations.getObservationDescriptions());

		HKCalculator hkCalculator = estimateMissingObservations && hk != null ? new HKCalculator(availableObsGainVectorIdArray, this.gainVectors) : null;

		for (int i = 0; i < m; i++) {
			// find matching column in steady-state gain
			// Use relative time stamps in ids to distinguish between observations for asynchronous filtering
			String gainVectorId = obsIds[i] + ":" + Math.round(obsTimeOffsets[i] * 24.0 * 3600.0); //conversion to seconds
			Results.putProgression("processing obs " + gainVectorId + "\n");
			// add to analysis increment for this obs
			if (this.gainVectors.containsKey(gainVectorId)) {
				IVector gainVector = this.gainVectors.get(gainVectorId);
				if (gainVector.getSize() != delta.getSize())
					Results.putMessage("Warning: Kalman Gain does not have exact same size as current state, this can cause suboptimal results. Please check if the contents of the state match the contents of the Kalman Gain.");
				// Skip assimilation when observations and predictions differ more than observation standard deviations times skipAssimilationStandardDeviationFactor
				double innovationValue = innovation.getValue(i);
				if (skipAssimilationStandardDeviationFactor != Double.POSITIVE_INFINITY && Math.abs(innovationValue) > skipAssimilationStandardDeviationFactor * standardDeviations.getValue(i)) {
					Results.putProgression("Info: Skipping assimilation for " + gainVectorId + " because innovation > (skipAssimilationStandardDeviationFactor * obs standard deviation). Observed value = " + observationValues.getValue(i) + ", model prediction value " + pred_a.getValue(i) + ", skipAssimilationStandardDeviationFactor = " + skipAssimilationStandardDeviationFactor + ", obs stdv = " + standardDeviations.getValue(i) + "\n");
					continue;
				}
				System.out.println("Innovation value for " + obsIds[i] + ": " + innovationValue);
				delta.axpy(innovationValue, gainVector);
				if (hkCalculator != null && hkCalculator.hasObservationsAvailable()) hkCalculator.setDAvailableValue(gainVectorId, innovationValue);
			} else {

				if (hk == null) throw new RuntimeException("No matching column found for observation with id=" + obsIds[i] + "and offset=" + obsTimeOffsets[i] + "\n");
			}
		}
		System.out.println();

		if (hkCalculator != null && hkCalculator.hasMissingObservations()) hkCalculator.compensateForMissingObservationsWithHK(obsTimeOffsets, delta);
		this.mainModel.axpyOnState(1.0, delta);
		Results.putValue("pred_a", pred_a, pred_a.getSize(), "analysis step", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Step);
	}

	private static String[] getAvailableObsGainVectorIdArray(String[] obsIds, double[] obsTimeOffsets) {
		String[] availableObsGainVectorIdArray = new String[obsIds.length];
		for (int i = 0; i < obsIds.length; i++) {
			// use relative time stamps in ids to distinguish between observations for asynchronous filtering
			String gainVectorId = obsIds[i] + ":" + Math.round(obsTimeOffsets[i] * 24.0 * 3600.0); //conversion to seconds
			availableObsGainVectorIdArray[i] = gainVectorId;
		}
		return availableObsGainVectorIdArray;
	}

	private static Matrix createIMinusM2(int numberOfMissingObservations, Matrix m2) {
		Matrix iMinusM2 = new Matrix(numberOfMissingObservations, numberOfMissingObservations);
		for (int row = 0; row < numberOfMissingObservations; row++) {
			for (int column = 0; column < numberOfMissingObservations; column++) {
				double m2Value = m2.getValue(row, column);
				double iMinusM2Value = row == column ? 1 - m2Value : -m2Value;
				iMinusM2.setValue(row, column, iMinusM2Value);
			}
		}
		return iMinusM2;
	}

	private void fillM1M2Matrices(ArrayList<Integer> missingObservationIndices, ArrayList<Integer> availableObservationIndicesGainStorage, Matrix m1, Matrix m2) {
		for (int rowIndex = 0; rowIndex < missingObservationIndices.size(); rowIndex++) {
			Integer missingObservationRowIndex = missingObservationIndices.get(rowIndex);
			for (int columnIndex = 0; columnIndex < availableObservationIndicesGainStorage.size(); columnIndex++) {
				Integer availableObservationColumnIndex = availableObservationIndicesGainStorage.get(columnIndex);
				m1.setValue(rowIndex, columnIndex, hk[missingObservationRowIndex][availableObservationColumnIndex]);
			}
			for (int columnIndex = 0; columnIndex < missingObservationIndices.size(); columnIndex++) {
				Integer missingObservationColumnIndex = missingObservationIndices.get(columnIndex);
				m2.setValue(rowIndex, columnIndex, hk[missingObservationRowIndex][missingObservationColumnIndex]);
			}
		}
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
            gainStorage.setKalmanGainStorageFileName(this.gainFileName);
            gainStorage.readKalmanGain(this.getCurrentState());
            String[] obsIds = gainStorage.getObservationIds();
            double[] obsTimeOffsets = gainStorage.getObservationOffsetInDays();
            IVector[] KVecs = gainStorage.getKalmanGainColumns();
			for (int i = 0; i < obsIds.length; i++) {
				// use relative time stamps in ids to distinguish between observations for asynchronous filtering
				String gainVectorId = obsIds[i] + ":" + Math.round(obsTimeOffsets[i] * 24.0 * 3600.0);
				this.gainVectors.put(gainVectorId, KVecs[i]);
			}
        }
    }

	class HKCalculator {
		private final String[] availableObsGainVectorIdArray;
		private final HashMap<String, IVector> gainVectors;
		ArrayList<Integer> missingObservationIndices = new ArrayList<>();
		ArrayList<Integer> availableObservationIndicesGainStorage = new ArrayList<>();
		ArrayList<Integer> availableObservationIndicesObserver = new ArrayList<>();
		private int numberOfMissingObservations;
		private int numberOfAvailableObservations;
		private Matrix m1;
		private Matrix m2;
		private Matrix dAvailable;

		public HKCalculator(String[] availableObsGainVectorIdArray, HashMap<String, IVector> gainVectors) {
			this.availableObsGainVectorIdArray = availableObsGainVectorIdArray;
			this.gainVectors = gainVectors;
			createMatrices();
		}

		private void fillIndicesArrays() {
			for (int i = 0; i < gainVectorIdArray.length; i++) {
				String gainVectorId = gainVectorIdArray[i];
				int indexOf = SortUtils.indexOfString(this.availableObsGainVectorIdArray, gainVectorId);
				if (indexOf >= 0) {
					this.availableObservationIndicesGainStorage.add(i);
					this.availableObservationIndicesObserver.add(indexOf);
					continue;
				}
				System.out.printf("Observation for %s missing%n", gainVectorId);
				this.missingObservationIndices.add(i);
			}
		}

		public void createMatrices() {
			fillIndicesArrays();
			numberOfMissingObservations = missingObservationIndices.size();
			numberOfAvailableObservations = gainStorageObservationIdsArray.length - numberOfMissingObservations;
			m1 = new Matrix(numberOfMissingObservations, numberOfAvailableObservations);
			m2 = new Matrix(numberOfMissingObservations, numberOfMissingObservations);
			fillM1M2Matrices(missingObservationIndices, availableObservationIndicesGainStorage, m1, m2);
			dAvailable = new Matrix(this.numberOfAvailableObservations, 1);
		}

		public boolean hasObservationsAvailable() {
			return this.numberOfAvailableObservations > 0;
		}

		public void setDAvailableValue(String gainVectorId, double innovationValue) {
			int indexOf = SortUtils.indexOfString(availableObsGainVectorIdArray, gainVectorId);
			int indexOfDAvailable = availableObservationIndicesObserver.indexOf(indexOf);
			System.out.println("Index in dAvailable " + indexOfDAvailable);
			System.out.println();
			dAvailable.setValue(indexOfDAvailable, 0, innovationValue);
		}

		public boolean hasMissingObservations() {
			return this.numberOfMissingObservations > 0;
		}

		private void compensateForMissingObservationsWithHK(double[] obsTimeOffsets, IVector delta) {
			Matrix iMinusM2 = createIMinusM2(this.numberOfMissingObservations, this.m2);
			System.out.println("Trying to compensate for missing observations using HK from kalman gain storage");
			Matrix inverseIMinusM2 = iMinusM2.inverse();
			Matrix m1DAvailable = this.m1.mult(this.dAvailable);
			Matrix dMissing = inverseIMinusM2.mult(m1DAvailable);
		/*System.out.println("M1: " + m1);
		System.out.println("M2: " + m2);
		System.out.println("inverseIMinusM2: " + inverseIMinusM2);
		System.out.println("dAvailable: " + dAvailable);
		System.out.println("m1DAvailable: " + m1DAvailable);
		System.out.println("dMissing: " + dMissing);*/
			for (int i = 0; i < this.missingObservationIndices.size(); i++) {
				Integer missingObservationIndex = this.missingObservationIndices.get(i);
				String missingObservationId = gainStorageObservationIdsArray[missingObservationIndex];
				String gainVectorId = missingObservationId + ":" + Math.round(obsTimeOffsets[i] * 24.0 * 3600.0); //conversion to seconds
				Results.putProgression("processing obs " + gainVectorId + "\n");
				// add to analysis increment for this obs
				if (this.gainVectors.containsKey(gainVectorId)) {
					IVector gainVector = this.gainVectors.get(gainVectorId);
					double calculatedInnovation = dMissing.getValue(i, 0);
					System.out.println("Calculated innovation for " + missingObservationId + ": " + calculatedInnovation + " index in dMissing: " + i);
					delta.axpy(calculatedInnovation, gainVector);
				}
			}
			System.out.println();
		}
	}
}
