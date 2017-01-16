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
public class LocEnKF extends EnKF {

	protected EnsembleVectors getEnsembleVectorsState(int iDomain, boolean analysis){
		EnsembleVectors ensVectors =new EnsembleVectors();

		// Get StateVectors
		ensVectors.ensemble = new IVector[this.ensembleSize];
		timerGetStates.start();
		for (int i = 0; i < this.ensembleSize; i++) {
			// collect ensemble
			ensVectors.ensemble[i] = this.ensemble[i].getState(iDomain);
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

	protected EnsembleVectors getEnsembleVectorsPrediction(int iDomain, IObservationDescriptions observationDescriptions, boolean analysis){
		EnsembleVectors ensVectors =new EnsembleVectors();

		// Get StateVectors
		ensVectors.ensemble = new IVector[this.ensembleSize];
		timerGetObs.start();
		for (int i = 0; i < this.ensembleSize; i++) {
			// collect ensemble
			ensVectors.ensemble[i] = this.ensemble[i].getObservationOperator().getObservedValues(observationDescriptions, iDomain);
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

	public void analysis(IStochObserver obs, IVector obsValues, IVector predictions,
			IStochModelInstance mainModel, ITime analysisTime) {
			//CtaUtils.print_native_memory("Enkf start",1);
        System.gc();
		{
			// Initialize all timers (at first call)
			initAllTimers();

			timerTotal.start();

			// Loop over all domains
			ILocalizationDomains domains = mainModel.getLocalizationDomains();
			int nDomain = domains.getStateDomainCount();
			for (int iDomain=0; iDomain<nDomain; iDomain++){

				int[] selector = domains.getObservationSelector(obs.getObservationDescriptions(), iDomain);
				IStochObserver lobs = obs.createSelection(selector);

				// Forecast: Get ensemble and predictions from the model ensemble (and output to result writers)
				//collect model output before state update.
				EnsembleVectors ensembleVectorsForecast = getEnsembleVectorsState(iDomain, false);

				//ensemblePredictionsForecast represents the observed model values, i.e. what the observations would look like, if reality would be equal to the current stoch model state.
				EnsembleVectors ensemblePredictionsForecast = getEnsembleVectorsPrediction(iDomain, lobs.getObservationDescriptions(), false);

				// Set State of method
				this.setCurrentState(ensembleVectorsForecast.mean.clone());

				// Compute Kalman gain
				//the K matrix is stored in variable Kvecs, which is an array of Vectors. There is one Vector for each observation point. Each Vector has the same length as the state vector.
				//Important note: here for the predictions and predicted observations the numbers are stored in two parts (because this makes the computations easier):
				//the mean (in ensembleVectorsForecast.mean and ensemblePredictionsForecast.mean) and the deviations from the mean (in ensembleVectorsForecast.ensemble and ensemblePredictionsForecast.ensemble).
				IVector Kvecs[] = computeGainMatrix(lobs,ensemblePredictionsForecast,ensembleVectorsForecast);

				// Apply localization to the gain matrix (shur-product)
				//localization is only applied if it is configured in the algorithm config.
				applyLocalizationToGain(lobs, Kvecs, ensemblePredictionsForecast,ensembleVectorsForecast);

				// Apply smoothing on the gain matrix
				if (this.timeRegularisationPerDay>0.0){
					if (this.smoothedGainMatrix==null){this.smoothedGainMatrix=new SmoothedGainMatrix(this.timeRegularisationPerDay);}
					this.smoothedGainMatrix.SmoothGain(obs,Kvecs, this.timeRegularisationPerDay, analysisTime);
				}

				// Store kalman gain for future use in this object
				storeGainMatrix(lobs, analysisTime, Kvecs);

				// Multiply Kalman gain with innovations and update model states
				updateModelWithGain(lobs, ensemblePredictionsForecast, ensembleVectorsForecast, Kvecs);

				// Free ensembles and Kalman gain;
				for (int j=0; j<Kvecs.length;j++){Kvecs[j].free();}
				ensembleVectorsForecast.free();
				ensemblePredictionsForecast.free();

				// Collect output for this analysis (and output to result writers)
				//collect model output after state update.
				EnsembleVectors ensembleVectorsAnalysis     = getEnsembleVectorsState(iDomain, true);
				EnsembleVectors ensemblePredictionsAnalysis = getEnsembleVectorsPrediction(iDomain, lobs.getObservationDescriptions(),true);

				// Adjust mainModel (adjust mainModel to mean analyzed state)
				updateMainModel(ensembleVectorsAnalysis);

				// Free ensembles
				ensembleVectorsAnalysis.free();
				ensemblePredictionsAnalysis.free();
			}

			timerTotal.stop();
		}
	   // CtaUtils.print_native_memory("Enkf end",1);
        System.gc();
	}
}
