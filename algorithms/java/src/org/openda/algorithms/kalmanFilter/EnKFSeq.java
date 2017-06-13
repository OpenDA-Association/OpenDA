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
import org.openda.utils.StochVector;
import org.openda.utils.TreeVector;
import org.openda.utils.Vector;

import java.io.File;
import java.util.List;

/**
 * @author Nils van Velzen
 *         Traditional Ensemble Kalman filter as introduced by
 *         Evensen and Burgers. There is support for using a Schur-produkt
 *         for spatial truncation. This version assimilates the observations 1 at a time
 *         NOTE: The shur-product for localization is applied on the model state.
 *               There is no truncation on the observations which are augmented in this implementation
 */
public class EnKFSeq extends EnKF {

	/**
	 * Used to store a subset of the observations, i.e. one observation location.
	 */
	class MinimalObserver implements IStochObserver{
		private final IStochVector observation;
		private final MinimalObservationDescriptions minimalObservationDescriptions;

		public MinimalObserver(double expectation, double sqrtCovariance, double x, double y) {
			this.observation = new StochVector(1, expectation, sqrtCovariance);
			this.minimalObservationDescriptions = new MinimalObservationDescriptions(x, y);
		}

		public ISqrtCovariance getSqrtCovariance() {
			return observation.getSqrtCovariance();
		}

		public IVector getExpectations() {
				return observation.getExpectations();
		}

		public IVector getRealizations() {
			return observation.createRealization();
		}

		public IStochObserver createSelection(String selection) {
			return null;  //To change body of implemented methods use File | Settings | File Templates.
		}

		public IStochObserver createSelection(ITime selectionTimes) {
			return null;  //To change body of implemented methods use File | Settings | File Templates.
		}

		public IStochObserver createSelection(Type observationType) {
			return null;  //To change body of implemented methods use File | Settings | File Templates.
		}
		public ISelector createSelector(Type observationType) {
			return null;  //To change body of implemented methods use File | Settings | File Templates.
		}

		public int getCount() {
			return 1;  //To change body of implemented methods use File | Settings | File Templates.
		}

		public IVector getValues() {
			return null;  //To change body of implemented methods use File | Settings | File Templates.
		}


		public double evaluatePDF(IVector values) {
			return 0;  //To change body of implemented methods use File | Settings | File Templates.
		}

		public IVector evaluateMarginalPDFs(IVector values) {
			return null;  //To change body of implemented methods use File | Settings | File Templates.
		}

		public IVector getStandardDeviations() {
			return null;  //To change body of implemented methods use File | Settings | File Templates.
		}

		public ITime[] getTimes() {
			return new ITime[0];  //To change body of implemented methods use File | Settings | File Templates.
		}

		public void free() {
			//To change body of implemented methods use File | Settings | File Templates.
		}

		public IObservationDescriptions getObservationDescriptions() {
			return this.minimalObservationDescriptions;
		}

		public void setParent(IInstance parent) {
			//To change body of implemented methods use File | Settings | File Templates.
		}


		public void initialize(File workingDir, String[] arguments) {
			//To change body of implemented methods use File | Settings | File Templates.
		}

		public IInstance getParent() {
			return null;  //To change body of implemented methods use File | Settings | File Templates.
		}
	}

	/**
	 * Used to store a subset of the observations, i.e. one exchangeItem for one observation location.
	 */
	private class MinimalObservationDescriptions implements IObservationDescriptions {
		private final double x;
		private final double y;

		private MinimalObservationDescriptions(double x, double y) {
			this.x = x;
			this.y = y;
		}

		public List<IPrevExchangeItem> getExchangeItems() {
			throw new UnsupportedOperationException(getClass().getName() + ".getExchangeItems() not implemented.");
		}

		public IVector getValueProperties(String key) {
			if ("x".equalsIgnoreCase(key)) {
				return new Vector(new double[]{this.x});
			}
			if ("y".equalsIgnoreCase(key)) {
				return new Vector(new double[]{this.y});
			}

			throw new IllegalArgumentException("Unknown property key: " + key);
		}

		public String[] getStringProperties(String key) {
			throw new UnsupportedOperationException(getClass().getName() + ".getStringProperties() not implemented.");
		}

		public String[] getPropertyKeys() {
			return new String[]{"x", "y"};
		}

		public int getPropertyCount() {
			throw new UnsupportedOperationException(getClass().getName() + ".getPropertyCount() not implemented.");
		}

		public int getObservationCount() {
			return 1;
		}

		public ITime[] getTimes() {
			throw new UnsupportedOperationException(getClass().getName() + ".getTimes() not implemented.");
		}
	}

	protected void updateEnsembleWithGain(IStochObserver obs, EnsembleVectors ensemblePredictions, EnsembleVectors ensembleVectors, IVector[] Kvecs){
		timerGainMult.start();
		// correct each member
		double[] innovations = new double[ensembleVectors.ensemble.length];
		double innovationMean=0.0;
		for (int i = 0; i < this.ensembleSize; i++){
			IVector innovation = obs.getRealizations();
			innovation.axpy(-1.0, ensemblePredictions.ensemble[i]);
			innovation.axpy(-1.0, ensemblePredictions.mean);
			innovations[i] = innovation.getValue(0);
			innovationMean+=innovations[i];
		}
		innovationMean/=((double) this.ensembleSize);
		for (int i=0; i<innovations.length; i++){
			innovations[i]-=innovationMean;
		}

		for (int i = 0; i < this.ensembleSize; i++) {
			ensembleVectors.ensemble[i].axpy(innovations[i], Kvecs[0]);
		}
		ensembleVectors.mean.axpy(innovationMean, Kvecs[0]);
		timerGainMult.stop();
	}

	private void updateModel(EnsembleVectors ensembleVectors){
		timerGainMult.start();
		// correct each member
		for (int i = 0; i < this.ensembleSize; i++){
			IVector dxState = this.ensemble[i].getState();
			dxState.axpy(-1.0,ensembleVectors.mean.clone());
			dxState.axpy(-1.0,ensembleVectors.ensemble[i]);
			this.ensemble[i].axpyOnState(-1.0,dxState);
		}
		timerGainMult.stop();
	}


	private TreeVector augment(IVector vecModel, IVector vecObs){
		TreeVector treeVec= new TreeVector("augmented");
		treeVec.addChild(new TreeVector("model",vecModel));
		treeVec.addChild(new TreeVector("prediction",vecObs));
		return treeVec;
	}

	private EnsembleVectors augment(EnsembleVectors model, EnsembleVectors pred){
		EnsembleVectors combined = new EnsembleVectors();

		combined.ensemble = new IVector[this.ensemble.length];
		for (int iEns=0; iEns<ensemble.length; iEns++){
			combined.ensemble[iEns]=augment(model.ensemble[iEns],pred.ensemble[iEns]);
		}
		combined.mean=augment(model.mean, pred.mean);
		combined.standardDeviation=augment(model.standardDeviation, pred.standardDeviation);
		return combined;
	}

	private EnsembleVectors extractRow(EnsembleVectors ensVectors, int index){
		int q=ensVectors.ensemble.length;
		EnsembleVectors singleRowEnsemble = new EnsembleVectors();
		singleRowEnsemble.ensemble = new IVector[q];
		for (int iEns=0; iEns<q; iEns++){
			singleRowEnsemble.ensemble[iEns] = new Vector(1);
			singleRowEnsemble.ensemble[iEns].setValue(0,ensVectors.ensemble[iEns].getValue(index));
		}
		if (ensVectors.mean != null){
			singleRowEnsemble.mean = new Vector(1);
			singleRowEnsemble.mean.setValue(0,ensVectors.mean.getValue(index));
		}
		if (ensVectors.standardDeviation != null){
			singleRowEnsemble.standardDeviation = new Vector(1);
			singleRowEnsemble.standardDeviation.setValue(0,ensVectors.standardDeviation.getValue(index));
		}
		return singleRowEnsemble;
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
			EnsembleVectors ensembleVectorsForecast     = getEnsembleVectorsState(false);
			//ensemblePredictionsForecast represents the observed model values, i.e. what the observations would look like, if reality would be equal to the current stoch model state.
			EnsembleVectors ensemblePredictionsForecast = getEnsembleVectorsPrediction(obs.getObservationDescriptions(), false);

			EnsembleVectors ensembleVectorsForecastAugmented = augment(ensembleVectorsForecast,ensemblePredictionsForecast);

			// Set State of method
			this.setCurrentState(ensembleVectorsForecast.mean.clone());

			IVector expectations =obs.getExpectations();
			IVector standardDeviations  =obs.getStandardDeviations();

			//TODO the call to obs.getObservationDescriptions().getValueProperties will cause an Exception
			//if getValueProperties not implemented for the given obs.getObservationDescriptions(),
			//but I don't see how to implement this any other way. The only solution would be
			//to remove MinimalObserver and create the selection using a call to obs.createSelection(),
			//which should have been done in the first place.
			IVector xVector = obs.getObservationDescriptions().getValueProperties("x");
			IVector yVector = obs.getObservationDescriptions().getValueProperties("y");

			logLocalizationType();
			Results.putProgression("Assimilating " + obs.getCount() + " observations.");
			for (int iObs=0; iObs<obs.getCount(); iObs++){
				// Create observer for single observation
				double x = (xVector != null && xVector.getSize() > iObs) ? xVector.getValue(iObs) : Double.NaN;
				double y = (yVector != null && yVector.getSize() > iObs) ? yVector.getValue(iObs) : Double.NaN;
				MinimalObserver obs1 = new MinimalObserver(expectations.getValue(iObs), standardDeviations.getValue(iObs), x, y);

				// Create ensemble forecast for a single observation
				EnsembleVectors singleEnsemblePredictionForecast = extractRow(ensemblePredictionsForecast, iObs);

				// Compute Kalman gain
				IVector Kvecs[] = (IVector []) computeGainMatrix(obs1,singleEnsemblePredictionForecast,ensembleVectorsForecastAugmented, false, false);

				// Apply localization to the gain matrix (shur-product) only on model state
				applyLocalizationToGain(obs1, Kvecs, singleEnsemblePredictionForecast, ensembleVectorsForecastAugmented, iObs);

				// Multiply Kalman gain with innovations and update model states
				updateEnsembleWithGain(obs1, singleEnsemblePredictionForecast, ensembleVectorsForecastAugmented, Kvecs);

				for (int j=0; j<Kvecs.length;j++){Kvecs[j].free();}
			}

	 		// Update the model based on the computed values the ensemble (vectors)
			updateModel(ensembleVectorsForecast);


            // Free ensembles and Kalman gain;
			ensembleVectorsForecast.free();
			ensemblePredictionsForecast.free();

			// Collect output for this analysis (and output to result writers)
			EnsembleVectors ensembleVectorsAnalysis     = getEnsembleVectorsState(true);
			EnsembleVectors ensemblePredictionsAnalysis = getEnsembleVectorsPrediction(obs.getObservationDescriptions(),true);
			timerResults.start();
			Results.putValue("pred_a_linear", ensemblePredictionsAnalysis.mean, ensemblePredictionsAnalysis.mean.getSize() , "analysis step", IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Step);
			timerResults.stop();

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

	protected void applyLocalizationToGain(IStochObserver obs, IVector[] Kvecs, EnsembleVectors ensemblePredictionsForecast, EnsembleVectors ensembleVectorsForecast, int iObs){
		timerLocalization.start();
		int indexObs[]=new int[1];
		indexObs[0]=iObs;

		if (this.localizationMethod==LocalizationMethodType.hamill){
			// We can only get localization for the model state
			ITreeVector Kvecs_model[] = new ITreeVector[1];
			Kvecs_model[0] = ((ITreeVector) Kvecs[0]).getSubTreeVector("model");
			indexObs[0]=iObs;
			localizationHamill(obs, Kvecs_model, indexObs, null, false);
		}
		else if (this.localizationMethod==LocalizationMethodType.autoZhang){
			localizationZhang(obs, Kvecs, ensemblePredictionsForecast, ensembleVectorsForecast, indexObs, "model", false);
		}
		timerLocalization.stop();
	}

	private void logLocalizationType(){
		if (this.localizationMethod == LocalizationMethodType.hamill) {
			Results.putMessage(this.getClass().getSimpleName() + ": apply localization according to Hamill to state not augmented Hx!");

		} else if (this.localizationMethod == LocalizationMethodType.autoZhang) {
			Results.putMessage(this.getClass().getSimpleName() + ": apply localization according to Zhang.");

		} else {
			Results.putMessage(this.getClass().getSimpleName() + ": no localization.");
		}
	}
}
