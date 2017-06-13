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
import org.openda.utils.Results;

/**
 * @author Nils van Velzen
 *         Deterministic Ensemble Kalman filter as introduced by
 *         Pavel Sakov and Peter R. Oke
 *         A deterministic formulation of the ensemble Kalman filter: an alternative to ensemble square root filters
 *         Tellus (2008), 60A, 361–371
 *
 *         This implementation supports localization using a a
 *         Schur-produkt for spatial truncation.
 */
//public class DEnKF extends AbstractSequentialEnsembleAlgorithm {
	public class DEnKF extends EnKF {


	private void updateModelWithGain(IStochObserver obs, EnsembleVectors ensemblePredictions, EnsembleVectors ensembleVectors, IVector[] Kvecs){
		System.out.println("Update DEnKF");
		timerGainMult.start();

		// Apply Kalman gain to mean
		IVector deltaMean=ensembleVectors.mean.clone();
		deltaMean.setConstant(0.0);

		IVector y_Hx=obs.getValues();
		y_Hx.axpy(-1.0, ensemblePredictions.mean);
		for(int j=0;j<Kvecs.length;j++){
			deltaMean.axpy(y_Hx.getValue(j), Kvecs[j]);
		}
		y_Hx.free();

		Results.putValue("deltaMean", deltaMean, deltaMean.getSize(), "analysis step", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Step);

		// correct each member but only using half the gain matrix
		for (int i = 0; i < this.ensembleSize; i++) {

            //Determine offset to make ensemble member equal to analysed mean
			IVector delta = deltaMean.clone();   //Update from x_mean_f to x_mean_a
            //add the update (use half the gain)
			for(int j=0;j<Kvecs.length;j++){
				delta.axpy(-0.5*ensemblePredictions.ensemble[i].getValue(j), Kvecs[j]);
			}
			Results.putValue("delta", delta, delta.getSize(), "analysis step", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Step);
			ensemble[i].axpyOnState(1.0, delta);
			delta.free();

		}

		deltaMean.free();

		timerGainMult.stop();
	}





	
	public void analysis(IStochObserver obs, IVector obsValues, IVector predictions,
			IStochModelInstance mainModel, ITime analysisTime) {
			//CtaUtils.print_native_memory("DEnkf start",1);
        System.gc();
		{
			// Initialize all timers (at first call)
			initAllTimers();

			timerTotal.start();

			// Forecast: Get ensemble and predictions from the model ensemble (and output to result writers)
			EnsembleVectors ensembleVectorsForecast     = getEnsembleVectorsState(false);
			//ensemblePredictionsForecast represents the observed model values, i.e. what the observations would look like, if reality would be equal to the current stoch model state.
			EnsembleVectors ensemblePredictionsForecast = getEnsembleVectorsPrediction(obs.getObservationDescriptions(), false);

			// Set State of method
			this.setCurrentState(ensembleVectorsForecast.mean.clone());

			// Compute Kalman gain
			IVector Kvecs[] = computeGainMatrix(obs,ensemblePredictionsForecast,ensembleVectorsForecast);

			// Apply localization to the gain matrix (shur-product)
			applyLocalizationToGain(obs, Kvecs, ensemblePredictionsForecast, ensembleVectorsForecast);

			// Store kalman gain for future use in this object
			storeGainMatrix(obs, analysisTime, Kvecs);

			// Multiply Kalman gain with innovations and update model states
			updateModelWithGain(obs, ensemblePredictionsForecast, ensembleVectorsForecast, Kvecs);

            // Free ensembles and Kalman gain;
			for (int j=0; j<Kvecs.length;j++){Kvecs[j].free();}
			ensembleVectorsForecast.free();
			ensemblePredictionsForecast.free();

			// Collect output for this analysis (and output to result writers)
			EnsembleVectors ensembleVectorsAnalysis     = getEnsembleVectorsState(true);
			EnsembleVectors ensemblePredictionsAnalysis = getEnsembleVectorsPrediction(obs.getObservationDescriptions(),true);

			// Adjust mainModel (adjust mainModel to mean analyzed state)
			updateMainModel(ensembleVectorsAnalysis);

			// Free ensembles
			ensembleVectorsAnalysis.free();
	        ensemblePredictionsAnalysis.free();

			timerTotal.stop();
		}
	   // CtaUtils.print_native_memory("DEnkf end",1);
        System.gc();
	}
}
