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
import org.openda.interfaces.*;
import org.openda.observers.ObserverUtils;
import org.openda.utils.Matrix;
import org.openda.utils.Results;
import org.openda.utils.io.KalmanGainStorage;
import org.openda.utils.performance.OdaTiming;

/**
 * @author Nils van Velzen based in the initial OpenDA implementation by Martin Verlaan
 *         Traditional Ensemble Kalman filter as introduced by
 *         Evensen and Burgers. This is a classical implementation processing
 *         all observations at once. There is no Schur-produkt for spatial
 *         truncation.
 */
public class EnKF extends AbstractSequentialEnsembleAlgorithm {

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

	SmoothedGainMatrix  smoothedGainMatrix = null;


    protected class SmoothedGainMatrix{
		private IVector[] lastGainMatrix;
		private double previousAnalisysTime;
		private double timeRegularisationPerDay;

		public SmoothedGainMatrix(double timeRegularisationPerDay){
		    this.previousAnalisysTime= Double.NaN;
			this.lastGainMatrix=null;
			this.timeRegularisationPerDay=timeRegularisationPerDay;
		}

		public void SmoothGain(IVector[] K, double timeRegularisationPerDay, ITime analysisTime){
			int nCols=K.length;
			if (lastGainMatrix==null) {

				lastGainMatrix= new IVector[nCols];
				for (int iCol=0; iCol<nCols; iCol++ ){
					lastGainMatrix[iCol]=K[iCol].clone();
				}

			}
			else {
				double dTime=analysisTime.getMJD()-this.previousAnalisysTime;
				double exponent=dTime/timeRegularisationPerDay;
				double alpha=Math.pow(timeRegularisationPerDay,exponent);
				System.out.println("Gain smoothing alpha="+alpha);
				for (int iCol=0; iCol<nCols; iCol++){
					lastGainMatrix[iCol].scale(alpha);
					lastGainMatrix[iCol].axpy((1.0-alpha),K[iCol]);
					K[iCol].setConstant(0.0);
					K[iCol].axpy(1.0,lastGainMatrix[iCol]);
				}
			}
			this.previousAnalisysTime=analysisTime.getMJD();
		}
		protected void free(){
			if (this.lastGainMatrix!=null){
				for (int iCol=0; iCol<this.lastGainMatrix.length ; iCol++){
					this.lastGainMatrix[iCol].free();
				}
				this.lastGainMatrix=null;
			}
		}
	}



	protected EnsembleVectors getEnsembleVectorsState(boolean analysis){
		EnsembleVectors ensVectors =new EnsembleVectors();

        // Get StateVectors
	 	ensVectors.ensemble = new IVector[this.ensembleSize];
		timerGetStates.start();
		for (int i = 0; i < this.ensembleSize; i++) {
			// collect ensemble
			ensVectors.ensemble[i] = this.ensemble[i].getState();
		}
		timerGetStates.stop();

		timerResults.start();
		if (analysis){
			for (int i = 0; i < this.ensembleSize; i++) {
  				Results.putValue("xi_a_"+i, ensVectors.ensemble[i], ensVectors.ensemble[i].getSize(), "analysis step", IResultWriter.OutputLevel.All, IResultWriter.MessageType.Step);
			}
		}
		timerResults.stop();

		// Compute ensemble average
		timerAverage.start();
		ensVectors.mean = ensembleAverage(ensVectors.ensemble);
		timerAverage.stop();

		// Remove average
		removeAverage(ensVectors.ensemble, ensVectors.mean);
		
		// Compute ensemble standard deviation
		timerStd.start();
		ensVectors.standardDeviation = ensembleStdZeroMean(ensVectors.ensemble);
		timerStd.stop();

		timerResults.start();
		if (analysis){
           Results.putValue("std_x_a", ensVectors.standardDeviation, ensVectors.standardDeviation.getSize() , "analysis step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
		} else {
			Results.putValue("x_f", ensVectors.mean, ensVectors.mean.getSize(), "analysis step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
   			Results.putValue("std_x_f", ensVectors.standardDeviation, ensVectors.standardDeviation.getSize(), "analysis step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
		}
		timerResults.stop();

		return ensVectors;
	}
	
	protected EnsembleVectors getEnsembleVectorsPrediction(IObservationDescriptions observationDescriptions, boolean analysis){
		EnsembleVectors ensVectors =new EnsembleVectors();

        // Get StateVectors
	 	ensVectors.ensemble = new IVector[this.ensembleSize];
		timerGetObs.start();
		for (int i = 0; i < this.ensembleSize; i++) {
			// collect ensemble
			ensVectors.ensemble[i] = this.ensemble[i].getObservedValues(observationDescriptions);
		}
		timerGetObs.stop();

		timerResults.start();
		for (int i = 0; i < this.ensembleSize; i++) {
			// collect predictions
			if (analysis){
				Results.putValue("pred_a_"+i, ensVectors.ensemble[i], ensVectors.ensemble[i].getSize() , "analysis step", IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Step);
			}
			else {
		   		Results.putValue("pred_f_"+i, ensVectors.ensemble[i], ensVectors.ensemble[i].getSize() , "analysis step", IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Step);
			}
		}
		timerResults.stop();

		// Compute ensemble average
		timerAverage.start();
		ensVectors.mean = ensembleAverage(ensVectors.ensemble);
		timerAverage.stop();

		// Remove average
		removeAverage(ensVectors.ensemble, ensVectors.mean);
		
		// Compute ensemble standard deviation
		timerStd.start();
		ensVectors.standardDeviation = ensembleStdZeroMean(ensVectors.ensemble);
		timerStd.stop();

		timerResults.start();
		if (analysis){
			Results.putValue("pred_a", ensVectors.mean, ensVectors.mean.getSize() , "analysis step", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Step);
		}
		else {
			Results.putValue("pred_f", ensVectors.mean, ensVectors.mean.getSize(), "analysis step", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Step);
   			Results.putValue("pred_f_std", ensVectors.standardDeviation, ensVectors.standardDeviation.getSize(), "analysis step", IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Step);
		}
		timerResults.stop();

		return ensVectors;
	}


	private void updateModelWithGain(IStochObserver obs, EnsembleVectors ensemblePredictions, EnsembleVectors ensembleVectors, IVector[] Kvecs){
		timerGainMult.start();
		// correct each member
		for (int i = 0; i < this.ensembleSize; i++) {
			//one innovation for each observation point.
			IVector innovation = obs.getRealizations();
			innovation.axpy(-1.0, ensemblePredictions.ensemble[i]);
			innovation.axpy(-1.0, ensemblePredictions.mean);
			//delta has length of state vector.
			IVector delta = ensembleVectors.ensemble[0].clone(); //vector of same type; content is overwritten
			delta.scale(0.0);
			for(int j=0;j<Kvecs.length;j++){
				delta.axpy(innovation.getValue(j), Kvecs[j]);
			}
			Results.putValue("delta_"+i, delta, delta.getSize(), "analysis step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
			//K_k*(D - H*A^f_k)) = delta
			//A^a_k = A^f_k + delta
			//for each element in state vector add corresponding element of delta.
			ensemble[i].axpyOnState(1.0, delta);
			delta.free();
			innovation.free();
		}
		timerGainMult.stop();
	}

	protected void applyLocalizationToGain(IStochObserver obs, IVector[] Kvecs, EnsembleVectors ensemblePredictionsForecast, EnsembleVectors ensembleVectorsForecast){

		timerLocalization.start();
		if (this.localizationMethod==LocalizationMethodType.hamill){

			localizationHamill(obs, Kvecs);
		}
		else if (this.localizationMethod==LocalizationMethodType.autoZhang){
			localizationZhang(obs, Kvecs, ensemblePredictionsForecast, ensembleVectorsForecast);
		}
		timerLocalization.stop();
	}

	protected  void localizationHamill(IStochObserver obs, IVector[] Kvecs){
		localizationHamill(obs, Kvecs, null, null, true);
	}

	protected void localizationHamill(IStochObserver obs, IVector[] Kvecs, int[] colIndexOutput, String resultWriterSubTreevectorID, boolean write_output){
		// Apply localization Method similar to
		// Hamill, T., J. S. Whitaker, and C. Snyder, 2001:
		// Distance-dependent filtering of back- ground error covariance estimates


		if (write_output) {
		System.out.print("Applying localization method according to Hamill\n");
		}
		// Get the localization correlation matrix for the ensemble
		IVector[] rho = this.ensemble[0].getObservedLocalization(obs.getObservationDescriptions(), this.distance);
		for(int i=0; i<rho.length; i++){
			int indexOutput=i;
			if (colIndexOutput!=null){indexOutput=colIndexOutput[i];}
			if (write_output) {
				resultWriteSubTree("KpreLoc_" + indexOutput, Kvecs[i], resultWriterSubTreevectorID);
				resultWriteSubTree("rho_" + indexOutput, rho[i], resultWriterSubTreevectorID);
			}
			Kvecs[i].pointwiseMultiply(rho[i]);
			rho[i].free();
			if (write_output) {
				resultWriteSubTree("KLoc_" + indexOutput, Kvecs[i], resultWriterSubTreevectorID);
			}
		}
	}

	protected void localizationZhang(IStochObserver obs, IVector[] Kvecs, EnsembleVectors ensemblePredictionsForecast, EnsembleVectors ensembleVectorsForecast) {
		localizationZhang(obs, Kvecs, ensemblePredictionsForecast, ensembleVectorsForecast, null, null, true);
	}

	protected void localizationZhang(IStochObserver obs, IVector[] Kvecs, EnsembleVectors ensemblePredictionsForecast, EnsembleVectors ensembleVectorsForecast, int colIndexOutput[], String resultWriterSubTreevectorID, boolean write_output){
		// Automatic localization algorithm as described in
		// Yanfen Zhang and Dean S. Oliver
		// Evaluation and error analysis: Kalman gain regularisation versus covariance
		// regularisation
		// Comput. Geosci (2011) 15:489-508
		//  DOI 10.1007/s10596-010-9218-y
		if (write_output) {
		System.out.print("Applying localization method according to Zhang\n");
		}
		// Get the localization correlation matrix for the ensemble
		AutoLocalizationZhang2011 localMethod = new AutoLocalizationZhang2011();
		IVector[] rho = localMethod.computeObservedLocalization(this, obs, ensemblePredictionsForecast, ensembleVectorsForecast, write_output);
        if (colIndexOutput!=null){
			if (rho.length!=colIndexOutput.length) {
				throw new RuntimeException("The length of the array with indices for output ("+colIndexOutput.length+
						                   ") does not correspond to the number of columns of the gain matrix ("+rho.length+
						                   ")\nThis is a programming error in OpenDA");
			}
		}
		for(int i=0; i<rho.length; i++){
			int indexOutput=i;
			if (colIndexOutput!=null){indexOutput=colIndexOutput[i];}

			if (write_output) {
				resultWriteSubTree("KpreLoc_" + indexOutput, Kvecs[i], resultWriterSubTreevectorID);
				resultWriteSubTree("rho_" + indexOutput, rho[i], resultWriterSubTreevectorID);
			}

            //Shur-product between localization mask and column of gain matrix
			Kvecs[i].pointwiseMultiply(rho[i]);

			if (write_output) {
				resultWriteSubTree("KLoc_" + indexOutput, Kvecs[i], resultWriterSubTreevectorID);
			}

			rho[i].free();
		}
	}

	// write a vector to resultwriter. When requested we only write a sub-tree (e.g. when used with augmented states)
	private void resultWriteSubTree(String name, IVector vector, String resultWriterSubTreevectorID){
		if (resultWriterSubTreevectorID!=null && vector instanceof ITreeVector) {
			ITreeVector treeVec = ((ITreeVector) vector).getSubTreeVector(resultWriterSubTreevectorID);
			Results.putValue(name, treeVec, treeVec.getSize(), "analysis step", IResultWriter.OutputLevel.All, IResultWriter.MessageType.Step);
		}
		else {
			Results.putValue(name, vector, vector.getSize(), "analysis step", IResultWriter.OutputLevel.All, IResultWriter.MessageType.Step);
		}
	}





	protected IVector[] computeGainMatrix(IStochObserver obs, EnsembleVectors ensemblePredictions, EnsembleVectors ensembleVectors){
		return computeGainMatrix(obs, ensemblePredictions, ensembleVectors, true, true);
	}

	protected IVector[] computeGainMatrix(IStochObserver obs, EnsembleVectors ensemblePredictions, EnsembleVectors ensembleVectors, boolean compute_pred_a_linear, boolean write_output){
		int m = obs.getCount(); // number of observations
		int n = ensembleVectors.mean.getSize(); // length of the state vector
		int q = this.ensembleSize; // number of ensemble members
		double sqrtQmin1 = Math.sqrt((double) q -1.0);


		// compute Kalman gain
		// D = HPH+R = (1/(q-1))PRED*PRED'+sqrtR*sqrtR' : covariance of
		// innovations
		timerLinalg.start();
		//H*A^f_k = predMat = prediction of the observed model values, after removing the mean.
		Matrix predMat = new Matrix(ensemblePredictions.ensemble);
		predMat.scale(Math.sqrt(1.0 / (q - 1.0)));
		// System.out.println("predMat="+predMat);
		Matrix D = new Matrix(m, m);
		D.multiply(1.0, predMat, predMat, 0.0, false, true);
		IMatrix sqrtR = obs.getSqrtCovariance().asMatrix();
		D.multiply(1.0, sqrtR, sqrtR, 1.0, false, true);
		timerLinalg.stop();
		timerResults.start();
		if (write_output){
			Results.putValue("sqrt_r", sqrtR, sqrtR.getNumberOfColumns() * sqrtR.getNumberOfRows() , "analysis step", IResultWriter.OutputLevel.All, IResultWriter.MessageType.Step);
        	Results.putValue("hpht_plus_r", D, D.getNumberOfColumns() * D.getNumberOfRows() , "analysis step", IResultWriter.OutputLevel.All, IResultWriter.MessageType.Step);
			Results.putProgression("length of state vector: " + n + ".");
			Results.putProgression("number of observations: " + m + ".");
		}
		timerResults.stop();
		timerLinalg.start();
		// K = XI*PRED'*inv(D) = XI * Xfac
		for(int i=0;i<q;i++){
			ensembleVectors.ensemble[i].scale(1.0/sqrtQmin1);
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
			Kvecs[i] = ensembleVectors.ensemble[0].clone(); //HERE !!!
			Kvecs[i].scale(0.0);
			for(int j=0;j<q;j++){
				Kvecs[i].axpy(E.getValue(j, i),ensembleVectors.ensemble[j]);
			}
			timerLinalg.stop();
			timerResults.start();
			if (write_output){
            	Results.putValue("k_"+i, Kvecs[i], Kvecs[i].getSize() , "analysis step", IResultWriter.OutputLevel.All, IResultWriter.MessageType.Step);
			}
			timerResults.stop();
		}

		for(int i=0;i<q;i++){
			ensembleVectors.ensemble[i].scale(sqrtQmin1);
		}

		if (compute_pred_a_linear){
		// Compute H*K for linear update of predictions, since for blackbox models the predictions
		// are not upadted until after the next forecast
		// H*K = PRED*PRED'*inv(D)
		timerLinalg.start();
		Matrix K_pred = new Matrix(m,m);
		K_pred.multiply(1.0, predMat, E, 0.0, false, false);
		// pred_a_linear = predAvg + K_pred*(obsVal-predAvg)
		IVector innovAvg = obs.getExpectations();
		innovAvg.axpy(-1, ensemblePredictions.mean);
		IVector pred_a_linear = ensemblePredictions.mean.clone();
		K_pred.rightMultiply(1.0, innovAvg, 1.0, pred_a_linear);
		innovAvg.free();
		timerLinalg.stop();
		timerResults.start();
		if (write_output){
        	Results.putValue("pred_a_linear", pred_a_linear, pred_a_linear.getSize() , "analysis step", IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Step);
		}
		timerResults.stop();

		pred_a_linear.free();
		}

		return Kvecs;
	}



	protected void storeGainMatrix(IStochObserver obs, ITime analysisTime, IVector[] Kvecs){

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
					for(int i=0;i<obs.getCount();i++){
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
	}

	protected void updateMainModel(EnsembleVectors ensembleVectors){
		IVector deltaMean = ensembleVectors.mean.clone();
		IVector xMain = mainModel.getState();

		deltaMean.axpy(-1.0,xMain);
		this.mainModel.axpyOnState(1.0, deltaMean);

		xMain.free();
		deltaMean.free();
	}

	protected void initAllTimers(){
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
	}


    
	public void analysis(IStochObserver obs, IVector obsValues, IVector predictions,
			IStochModelInstance mainModel, ITime analysisTime) {
			//CtaUtils.print_native_memory("Enkf start",1);
        System.gc();
		{
			// Initialize all timers (at first call)
			initAllTimers();

			timerTotal.start();

			// Forecast: Get ensemble and predictions from the model ensemble (and output to result writers)
			//collect model output before state update.
			EnsembleVectors ensembleVectorsForecast     = getEnsembleVectorsState(false);
			//ensemblePredictionsForecast represents the observed model values, i.e. what the observations would look like, if reality would be equal to the current stoch model state.
			EnsembleVectors ensemblePredictionsForecast = getEnsembleVectorsPrediction(obs.getObservationDescriptions(), false);

			// Set State of method
			this.setCurrentState(ensembleVectorsForecast.mean.clone());

			// Compute Kalman gain
			//the K matrix is stored in variable Kvecs, which is an array of Vectors. There is one Vector for each observation point. Each Vector has the same length as the state vector.
			//Important note: here for the predictions and predicted observations the numbers are stored in two parts (because this makes the computations easier):
			//the mean (in ensembleVectorsForecast.mean and ensemblePredictionsForecast.mean) and the deviations from the mean (in ensembleVectorsForecast.ensemble and ensemblePredictionsForecast.ensemble).
			IVector Kvecs[] = computeGainMatrix(obs,ensemblePredictionsForecast,ensembleVectorsForecast);

			// Apply localization to the gain matrix (shur-product)
			//localization is only applied if it is configured in the algorithm config.
			applyLocalizationToGain(obs, Kvecs, ensemblePredictionsForecast,ensembleVectorsForecast);

			// Apply smoothing on the gain matrix
			if (this.timeRegularisationPerDay>0.0){
				if (this.smoothedGainMatrix==null){this.smoothedGainMatrix=new SmoothedGainMatrix(this.timeRegularisationPerDay);}
				this.smoothedGainMatrix.SmoothGain(Kvecs, this.timeRegularisationPerDay, analysisTime);
			}

			// Store kalman gain for future use in this object
			storeGainMatrix(obs, analysisTime, Kvecs);

			// Multiply Kalman gain with innovations and update model states
			updateModelWithGain(obs, ensemblePredictionsForecast, ensembleVectorsForecast, Kvecs);

            // Free ensembles and Kalman gain;
			for (int j=0; j<Kvecs.length;j++){Kvecs[j].free();}
			ensembleVectorsForecast.free();
			ensemblePredictionsForecast.free();

			// Collect output for this analysis (and output to result writers)
			//collect model output after state update.
			EnsembleVectors ensembleVectorsAnalysis     = getEnsembleVectorsState(true);
			EnsembleVectors ensemblePredictionsAnalysis = getEnsembleVectorsPrediction(obs.getObservationDescriptions(),true);

			// Adjust mainModel (adjust mainModel to mean analyzed state)
			updateMainModel(ensembleVectorsAnalysis);

			// Free ensembles
			ensembleVectorsAnalysis.free();
	        ensemblePredictionsAnalysis.free();

			timerTotal.stop();
		}
	   // CtaUtils.print_native_memory("Enkf end",1);
        System.gc();
	}
}
