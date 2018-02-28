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
import org.openda.interfaces.*;
import org.openda.observers.ObserverUtils;
import org.openda.utils.Matrix;
import org.openda.utils.Results;
import org.openda.utils.io.KalmanGainStorage;
import org.openda.utils.performance.OdaTiming;

/**
 * @author Martin Verlaan Traditional Ensemble Kalman filter as introduced by
 *         Evensen and Burgers. This is a classical implementation processing
 *         all observations at once. There is no Schur-produkt for spatial
 *         truncation.
 */
@Deprecated
public class EnKFOld extends AbstractSequentialEnsembleAlgorithm {

	boolean synchronizeForTiming = false;
	OdaTiming timerTotal        = null;
	OdaTiming timerGetObs       = null;
	OdaTiming timerGetStates    = null;
	OdaTiming timerAverage      = null;
	OdaTiming timerStd          = null;
	OdaTiming timerResults      = null;
	OdaTiming timerLinalg       = null;
	OdaTiming timerLocalization = null;
	OdaTiming timerStoreGain    = null;
	OdaTiming timerGainMult     = null;
	OdaTiming timerSynchronize  = null;
	OdaTiming timerAdjustMean   = null;


		
	public void analysis(IStochObserver obs, IVector obsValues, IVector predictions,
			IStochModelInstance mainModel, ITime analysisTime) {
			//CtaUtils.print_native_memory("Enkf start",1);
        System.gc();
		{
			if ( timerTotal == null)        {
				timerTotal        = new OdaTiming("analysis");
				timerSynchronize  = new OdaTiming("Synchronize");
				timerGetObs       = new OdaTiming("getObsVal");
				timerGetStates    = new OdaTiming("getState");
				timerAverage      = new OdaTiming("compute Average");
				timerStd          = new OdaTiming("compute stdev");
				timerResults      = new OdaTiming("resultWriting");
				timerLinalg       = new OdaTiming("linear algebra");
				timerLocalization = new OdaTiming("localization");
				timerStoreGain    = new OdaTiming("store gain");
				timerGainMult     = new OdaTiming("multiply with gain");
				timerAdjustMean   = new OdaTiming("adjustMean");

				timerTotal.AddSubTimer(timerSynchronize);
				timerTotal.AddSubTimer(timerGetObs);
				timerTotal.AddSubTimer(timerGetStates);
				timerTotal.AddSubTimer(timerAverage);
				timerTotal.AddSubTimer(timerStd);
				timerTotal.AddSubTimer(timerResults);
				timerTotal.AddSubTimer(timerLinalg);
				timerTotal.AddSubTimer(timerLocalization);
				timerTotal.AddSubTimer(timerStoreGain);
				timerTotal.AddSubTimer(timerGainMult);
				timerTotal.AddSubTimer(timerAdjustMean);
			}


			timerTotal.start();
			IVector[] xi = new IVector[this.ensembleSize];
			IVector[] pred = new IVector[this.ensembleSize];

			// Debug force synchronization
			if (synchronizeForTiming){
				timerSynchronize.start();
				this.ensemble[this.ensembleSize-1].getTimeHorizon();
				timerSynchronize.stop();
			}

			timerGetStates.start();
			for (int i = 0; i < this.ensembleSize; i++) {
				// collect ensemble
				xi[i] = this.ensemble[i].getState();
			}
			timerGetStates.stop();

			timerGetObs.start();
			for (int i = 0; i < this.ensembleSize; i++) {
				// collect predictions
				pred[i] = this.ensemble[i].getObservedValues(obs.getObservationDescriptions());
                Results.putValue("pred_f_"+i, pred[i], pred[i].getSize() , "analysis step", IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Step);
			}
			timerGetObs.stop();

			int m = obs.getCount(); // number of observations
			int n = xi[0].getSize(); // length of the state vector
			int q = this.ensembleSize; // number of ensemble members
			// compute ensemble average and subtract it from ensemble
			timerAverage.start();
			IVector xiAvg = ensembleAverage(xi);
			timerAverage.stop();
			this.setCurrentState(xiAvg.clone());
			timerStd.start();
			IVector xiStd = ensembleStd(xi);
			timerStd.stop();
			timerResults.start();
            Results.putValue("x_f", xiAvg, xiAvg.getSize(), "analysis step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
            Results.putValue("std_x_f", xiStd, xiStd.getSize(), "analysis step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
			xiStd.free();
			timerResults.stop();
			removeAverage(xi, xiAvg);
			xiAvg.free();
			timerAverage.start();
			IVector predAvg = ensembleAverage(pred);
			timerAverage.stop();
			timerResults.start();
            Results.putValue("pred_f", predAvg, predAvg.getSize(), "analysis step", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Step);
			IVector pred_f_std = ensembleStd(pred);
            Results.putValue("pred_f_std", pred_f_std, pred_f_std.getSize(), "analysis step", IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Step);
			pred_f_std.free();
			timerResults.stop();
			removeAverage(pred, predAvg);
			IVector obsVal = obs.getExpectations();
			// compute Kalman gain
			// D = HPH+R = (1/(q-1))PRED*PRED'+sqrtR*sqrtR' : covariance of
			// innovations
			timerLinalg.start();
			Matrix predMat = new Matrix(pred);
			predMat.scale(Math.sqrt(1.0 / (q - 1.0)));
			// System.out.println("predMat="+predMat);
			Matrix D = new Matrix(m, m);
			D.multiply(1.0, predMat, predMat, 0.0, false, true);
			IMatrix sqrtR = obs.getSqrtCovariance().asMatrix();
			timerLinalg.stop();
			timerResults.start();
            Results.putValue("sqrt_r", sqrtR, sqrtR.getNumberOfColumns() * sqrtR.getNumberOfRows() , "analysis step", IResultWriter.OutputLevel.All, IResultWriter.MessageType.Step);
			timerResults.stop();
			timerLinalg.start();
			D.multiply(1.0, sqrtR, sqrtR, 1.0, false, true);
			timerLinalg.stop();
			timerResults.start();
            Results.putValue("hpht_plus_r", D, D.getNumberOfColumns() * D.getNumberOfRows() , "analysis step", IResultWriter.OutputLevel.All, IResultWriter.MessageType.Step);

			Results.putProgression("length of state vector: " + n + ".");
			Results.putProgression("number of observations: " + m + ".");
			timerResults.stop();
			timerLinalg.start();
			// K = XI*PRED'*inv(D) = XI * Xfac
			for(int i=0;i<q;i++){
				xi[i].scale(Math.sqrt(1.0 / (q - 1.0)));
			}

			// System.out.println("PHT="+K);
			Matrix inverseD = D.inverse();
			// System.out.println("inverseD="+inverseD);

			// version without large matrices
			// K = XI * E with E=PRED'*inv(D)
			Matrix E = new Matrix(q,m);
			E.multiply(1.0, predMat, inverseD, 0.0, true, false);
			IVector Kvecs[] = new IVector[m];
			timerLinalg.stop();
			for(int i=0;i<m;i++){
				timerLinalg.start();
				Kvecs[i] = xi[0].clone(); //HERE !!!
				Kvecs[i].scale(0.0);
				for(int j=0;j<q;j++){
					Kvecs[i].axpy(E.getValue(j, i),xi[j]);
				}
				timerLinalg.stop();
				timerResults.start();
                Results.putValue("k_"+i, Kvecs[i], Kvecs[i].getSize() , "analysis step", IResultWriter.OutputLevel.All, IResultWriter.MessageType.Step);
				timerResults.stop();
			}

			// Compute H*K for linear update of predictions, since for blackbox models the predictions
			// are not upadted until after the next forecast
			// H*K = PRED*PRED'*inv(D)
			timerLinalg.start();
			Matrix K_pred = new Matrix(m,m);
			K_pred.multiply(1.0, predMat, E, 0.0, false, false);
			// pred_a_linear = predAvg + K_pred*(obsVal-predAvg)
			IVector innovAvg = obsVal.clone();
			innovAvg.axpy(-1, predAvg);
			IVector pred_a_linear = predAvg.clone();
			K_pred.rightMultiply(1.0, innovAvg, 1.0, pred_a_linear);
			innovAvg.free();
			timerLinalg.stop();
			timerResults.start();
            Results.putValue("pred_a_linear", pred_a_linear, pred_a_linear.getSize() , "analysis step", IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Step);
			timerResults.stop();
			pred_a_linear.free();

			// Apply localization Method similar to
			// Hamill, T., J. S. Whitaker, and C. Snyder, 2001:
			// Distance-dependent filtering of back- ground error covariance estimates
			if (this.localizationMethod==LocalizationMethodType.hamill){
				timerLocalization.start();
				System.out.print("Applying localization method according to Hamill\n");
				// Get the localization correlation matrix for the ensemble
				IVector[] rho = this.ensemble[0].getObservedLocalization(obs.getObservationDescriptions(), distance);
				for(int i=0; i<rho.length; i++){
				   Kvecs[i].pointwiseMultiply(rho[i]);
				   rho[i].free();
				}
				timerLocalization.stop();
			}

			// store kalman gain for future use in this object
			if(this.saveGainTimes!=null){
				timerStoreGain.start();
				ObserverUtils obsUtils = new ObserverUtils(obs);
				String[] obsIds = obsUtils.getObsIds();
				double[] obsTimeOffsets = obsUtils.getObsTimeOffsets(analysisTime.getMJD());


				// store gain when requested to file
				if(saveGainAtThisTime(this.currentTime)){

					//NOTE: the following six lines have been inserted inside the if(saveGainAtThisTime
					// They used to be invoked each analysis time step.
					// This meant, that between two KGstorage times, the gainvectors were
					// continually updated. Moreover, all observations that occur sometime between
					// two KG times would have produces a KG vector. Was this on purpose?
						for(int i=0;i<obsVal.getSize();i++){
					   String gainVectorId = obsIds[i]+":"+Math.round(obsTimeOffsets[i]*24.0*3600.0); //days to seconds
					   if (this.gainVectors.get(gainVectorId)!=null){this.gainVectors.get(gainVectorId).free();}
					   this.gainVectors.put(gainVectorId, Kvecs[i]);
					   this.obsId.put(gainVectorId, obsIds[i]);
					   this.obsTimeOffset.put(gainVectorId, obsTimeOffsets[i]);


					}

					double gainTimeStampAsMJD = analysisTime.getMJD();
					if (analysisTime.isSpan()) {
						// analysis time is a stamp with an accuracy range around it, take the 'middle'
						gainTimeStampAsMJD =
								(analysisTime.getBeginTime().getMJD() + analysisTime.getEndTime().getMJD()) / 2d;
					}
					KalmanGainStorage gainStorage = new KalmanGainStorage(workingDir, gainTimeStampAsMJD);
					gainStorage.setMaxKeepVectorInXMLSize(this.gainStorageMaxXmlStore);
					gainStorage.setStorageDirPrefix(this.gainStorageDirPrefix);

					gainStorage.setKalmanGainStorageXmlFileName(this.gainStorageXmlFileName);
					gainStorage.setColumnFileType(this.gainStorageFileType);
					gainStorage.setComment("algorithm = "+this.getClass().getSimpleName()+"\n"
							+"model = "+this.mainModel.getClass().getSimpleName()+"\n"
							+"observer = "+obs.getClass().getSimpleName()+"\n");

					gainStorage.writeKalmanGain(this.gainVectors, this.obsId, this.obsTimeOffset);
									this.gainVectors.clear();
									this.obsId.clear();

				}
				timerStoreGain.stop();
			}
			timerGainMult.start();
			// correct each member
			for (int i = 0; i < this.ensembleSize; i++) {
				IVector innovation = obs.getRealizations();
				innovation.axpy(-1.0, pred[i]);
				innovation.axpy(-1.0, predAvg);
				IVector delta = xi[0].clone(); //vector of same type; content is overwritten
				delta.scale(0.0);
				for(int j=0;j<m;j++){
					delta.axpy(innovation.getValue(j), Kvecs[j]);
				}
				Results.putValue("delta", delta, delta.getSize(), "analysis step", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Step);
				ensemble[i].axpyOnState(1.0, delta);
				delta.free();
				innovation.free();
			}
			for (int j=0; j<m;j++){Kvecs[j].free();}
			// collect output for this analysis
			timerGainMult.stop();
			timerGetStates.start();
			for (int i = 0; i < this.ensembleSize; i++) {
				xi[i].free();
				xi[i] = this.ensemble[i].getState();
                Results.putValue("xi_a_"+i, xi[i], xi[i].getSize(), "analysis step", IResultWriter.OutputLevel.All, IResultWriter.MessageType.Step);
			}
			timerGetStates.stop();
			timerGetObs.start();
			for (int i = 0; i < this.ensembleSize; i++) {
				// collect predictions
				pred[i] = this.ensemble[i].getObservedValues(obs
						.getObservationDescriptions());
                Results.putValue("pred_a_"+i, pred[i], pred[i].getSize() , "analysis step", IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Step);
			}
			timerGetObs.stop();

			timerAdjustMean.start();
			xiAvg = ensembleAverage(xi);
			// adjust mainModel (adjust mainModel to mean analyzed state)
			IVector deltaMain = xiAvg.clone();
			IVector xMain = mainModel.getState();
			deltaMain.axpy(-1.0,xMain);
			xMain.free();
			this.mainModel.axpyOnState(1.0, deltaMain);
			deltaMain.free();
			timerAdjustMean.stop();

			timerStd.start();
			xiStd = ensembleStd(xi);
			timerStd.stop();
			timerResults.start();
            Results.putValue("std_x_a", xiStd, xiStd.getSize() , "analysis step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
			xiStd.free();
			timerResults.stop();
			timerAverage.start();
			predAvg.free();
			predAvg = ensembleAverage(pred);
			timerAverage.stop();
			timerResults.start();
            Results.putValue("pred_a", predAvg, predAvg.getSize() , "analysis step", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Step);
			timerResults.stop();
			// extra output for checking
			//removeAverage(xi, xiAvg);
			//Matrix LaMat = new Matrix(xi);
			//LaMat.scale(Math.sqrt(1.0 / (q - 1.0)));
			// Free Vectors
			for (int i = 0; i < this.ensembleSize; i++){xi[i].free();}
			xiAvg.free();
			predAvg.free();

			timerTotal.stop();
		}
	   // CtaUtils.print_native_memory("Enkf end",1);
        System.gc();
	}
}
