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

import org.openda.algorithms.DudCoreOptimizer;
import org.openda.interfaces.*;
import org.openda.utils.*;

import java.io.File;

/**
 * 
 * @author Martin Verlaan Iterative Ensemble Kalman filter Research version of
 *         Martin Uses DUD like optimization for 3D-VAR cost with B estimated
 *         from ensemble
 */
public class DudEnKF extends AbstractSequentialEnsembleAlgorithm {

	public void analysis(IStochObserver obs, IVector obsValues, IVector predMainModel, 
			IStochModelInstance mainModel, ITime analysisTime) {
		int q = this.ensembleSize; // number of ensemble members
		IVector[] states = new IVector[q];
		IVector[] relStates = new IVector[q];
		IVector[] predictions = new IVector[q + 1];
		IVector[] perturbedObservations = new IVector[q];
		IVector initialTransformedStates[] = new IVector[q + 1];
		double initialCosts[] = new double[q + 1];
		IVector[] updatedStates = new IVector[q];
		IVector[] updatedPredictions = new IVector[q];

		// collect states and predictions and create perturbed obs
		for (int i = 0; i < q; i++) { // member index
			// collect ensemble states
			states[i] = this.ensemble[i].getState();
			relStates[i] = states[i].clone();
			// collect predictions
			predictions[i] = this.ensemble[i].getObservedValues(obs
					.getObservationDescriptions());
			// generate perturbed observations
			perturbedObservations[i] = obs.getRealizations(); // here noise is
																// added for
																// this instance
		}

		// compute ensemble average and subtract it from ensemble
		IVector xiAvg = ensembleAverage(states, q);
		IVector xiStd = ensembleStd(states);
        Results.putValue("x_f", xiAvg, xiAvg.getSize(), "analysis step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
        Results.putValue("std_x_f", xiStd, xiStd.getSize(), "analysis step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
		IVector predAvg = ensembleAverage(predictions, q);
        Results.putValue("pred_f", predAvg, predAvg.getSize(), "analysis step", IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Step);
		IVector obsVal = obs.getExpectations();
		// extra output for checking
		removeAverage(relStates, xiAvg);
		Matrix LfMat = new Matrix(relStates);
		LfMat.scale(Math.sqrt(1.0 / (q - 1.0)));
        Results.putValue("L_f", LfMat, LfMat.getNumberOfColumns() * LfMat.getNumberOfRows(), "analysis step", IResultWriter.OutputLevel.All, IResultWriter.MessageType.Step);

		// perturbed observations, thus we treat each modelInstance or Ensemble
		// member separately
		// setup and call dudCoreOptimizer for each member
		for (int i = 0; i < q; i++) { // member index
			IModelState savedState = this.ensemble[i].saveInternalState();
			AnalysisLeastSquaresCost f = new AnalysisLeastSquaresCost(
					this.ensemble[i], states, obs, perturbedObservations[i],
					savedState);
			f.factor = 0.5;

			// start with p[j] = sqrt(n-1) e_j - sqrt(n-1) e_i
			for (int j = 0; j < q; j++) {
				initialTransformedStates[j] = new Vector(q);
				if (i != j) {
					initialTransformedStates[j].setValue(j, Math.sqrt(q - 1.0));
					initialTransformedStates[j]
							.setValue(i, -Math.sqrt(q - 1.0));
				}
				// avoid some work bye computing initialCosts here outside
				// AnalysisLeastSquaresCost
				IVector residuals = perturbedObservations[i].clone();
				residuals.axpy(-1.0, predictions[j]);
				residuals.pointwiseDivide(obs.getStandardDeviations());
				initialCosts[j] = 0;
				if (i != j) {
					initialCosts[j] += 2.0 * 0.5 * (q - 1); // background
				}
				initialCosts[j] += 0.5 * Math.pow(residuals.norm2(), 2.0); // observations

				// above additional cost for re-evaluating the cost was avoided
				// by reuse
				// of existing data. Below this is checked with simple calls to
				// cost 'f'.
				// If you don't care about computational cost and want simple
				// code, you could
				// replace the code above with a call to f.
				/*
				 * double checkCost = f.evaluate(initialTransformedStates[j]);
				 * System.out.println("member i ="+i+" vector j="+j);
				 * System.out.println("===================================");
				 * System
				 * .out.println("p          = "+initialTransformedStates[j]);
				 * System.out.println("cost_local = "+initialCosts[j]);
				 * System.out.println("cost_f     = "+checkCost);
				 * System.out.println("pred_local = "+predictions[j]);
				 * System.out.println("pred_f     = "+f.getLastPredictions());
				 * System.out.println("obs_i      = "+perturbedObservations[i]);
				 * System.out.println("===================================");
				 */
			}
			// and p[n+1] = 0
			initialTransformedStates[q] = new Vector(q); // p=[0,...,0]
			initialTransformedStates[q].setValue(i, -Math.sqrt(q - 1.0));
			// TODO save and reuse
			initialCosts[q] = f.evaluate(initialTransformedStates[q],"analysis step");
			predictions[q] = f.getLastPredictions();

			DudCoreOptimizer dud = new DudCoreOptimizer(f);
			dud.initialize(initialTransformedStates, initialCosts, predictions);
			dud.maxit = 10;
			// run optimization
			dud.optimize();

			// get optimal state
			// set state of ensemble[i] to this value, detour to avoid
			// disturbing something
			IVector analysisIncrement = f.getOptimalState();
			analysisIncrement.axpy(-1.0, states[i]);
			this.ensemble[i].restoreInternalState(savedState);
			this.ensemble[i].axpyOnState(1.0, analysisIncrement);

			this.ensemble[i].releaseInternalState(savedState);

		} // loop i

		// collect output for this analysis
		for (int i = 0; i < q; i++) {
			updatedStates[i] = this.ensemble[i].getState();
			// collect predictions
			updatedPredictions[i] = this.ensemble[i].getObservedValues(obs
					.getObservationDescriptions());
		}
		xiAvg = ensembleAverage(updatedStates, q);
		// adjust mainModel (adjust mainModel to mean analyzed state)
		IVector deltaMain = xiAvg.clone();
		deltaMain.axpy(-1.0,mainModel.getState());
		mainModel.axpyOnState(1.0, deltaMain);

		xiStd = ensembleStd(updatedStates);
        Results.putValue("std_x_a", xiStd, xiStd.getSize(), "analysis step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
		predAvg = ensembleAverage(updatedPredictions, q);
		// extra output for checking
		removeAverage(updatedStates, xiAvg);
		Matrix LaMat = new Matrix(updatedStates);
		LaMat.scale(Math.sqrt(1.0 / (q - 1.0)));
        Results.putValue("L_a", LaMat, LaMat.getNumberOfColumns() * LaMat.getNumberOfRows(), "analysis step", IResultWriter.OutputLevel.All, IResultWriter.MessageType.Step);
	}

}
