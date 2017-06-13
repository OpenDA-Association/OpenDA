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
 * @author Martin Verlaan 
 *         Iterative Ensemble Square Root filter
 *         Research version of Martin
 *         Uses DUD like optimization for 3D-VAR cost with B estimated from ensemble
 */
public class DudENSR extends AbstractSequentialEnsembleAlgorithm {

	public void analysis(IStochObserver stochObs, IVector obsValues, IVector predMainModel, 
			IStochModelInstance mainModel, ITime analysisTime) {
		int q = this.ensembleSize; // number of ensemble members
		IVector[] states = new IVector[q];
		IVector[] relStates = new IVector[q];
		IVector[] predictions = new IVector[q+1];
		IVector initialTransformedStates[] = new IVector[q+1];
		double initialCosts[] = new double[q+1];
		IVector[] updatedStates = new IVector[q];
		IVector[] updatedPredictions = new IVector[q];

		// collect states and predictions and create perturbed obs
		for (int i = 0; i < q; i++) { //member index
			// collect ensemble states
			states[i] = this.ensemble[i].getState();
			relStates[i] = states[i].clone();
			// collect predictions
			predictions[i] = this.ensemble[i].getObservedValues(stochObs.getObservationDescriptions());
		}

		// compute ensemble average and subtract it from ensemble
		IVector xiAvg = ensembleAverage(states,q);
		removeAverage(relStates, xiAvg);
		IVector xiStd = ensembleStd(states);
        Results.putValue("x_f", xiAvg, xiAvg.getSize(), "analysis step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
        Results.putValue("std_x_f", xiStd, xiStd.getSize(), "analysis step", IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Step);
		IVector predAvg = ensembleAverage(predictions,q);
        Results.putValue("pred_f", predAvg, predAvg.getSize(), "analysis step", IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Step);
		IVector obsVal = stochObs.getExpectations();
		Results.putValue("std_obs", stochObs.getStandardDeviations(), stochObs.getStandardDeviations().getSize(), "analysis step", IResultWriter.OutputLevel.Normal, IResultWriter.MessageType.Step);

		// extra output for checking
		Matrix LfMat = new Matrix(relStates);
		LfMat.scale(Math.sqrt(1.0 / (q - 1.0)));
        Results.putValue("L_f", LfMat, LfMat.getNumberOfColumns() * LfMat.getNumberOfRows(), "analysis step", IResultWriter.OutputLevel.All, IResultWriter.MessageType.Step);

		// ENSR version, thus we make one Dud run for the average
		// setup and call dudCoreOptimizer for ensemble average (uses ensemble[0] for storage) 
		IVector deltaState = states[0].clone();
		deltaState.axpy(-1.0, xiAvg);
		IModelState savedMemberState = this.ensemble[0].saveInternalState(); //save original ensemble state
		this.ensemble[0].axpyOnState(-1.0, deltaState); // set member 0 to mean state
		IModelState savedAvgState = this.ensemble[0].saveInternalState(); //save average state

		AnalysisLeastSquaresCost f = new AnalysisLeastSquaresCost(this.ensemble[0], states, 
				stochObs, obsVal, savedAvgState);
		f.factor = 0.5;

		// start with p[j] = sqrt(n-1) e_j
		for(int j=0;j<q;j++){
			initialTransformedStates[j] = new Vector(q);
			initialTransformedStates[j].setValue(j, Math.sqrt(q-1.0));
			// avoid some work bye computing initialCosts here outside AnalysisLeastSquaresCost
			IVector residuals = obsVal.clone();
			residuals.axpy(-1.0, predictions[j]);
			IVector stdObs = stochObs.getStandardDeviations();
			residuals.pointwiseDivide(stdObs);
			initialCosts[j]  = f.evaluate(initialTransformedStates[j],"analysis step");
		}
		// and p[n+1] = 0
		initialTransformedStates[q] = new Vector(q); // p=[0,...,0]
		initialCosts[q] = f.evaluate(initialTransformedStates[q],"analysis step");
		predictions[q] = f.getLastPredictions();

		DudCoreOptimizer dud = new DudCoreOptimizer(f);
		dud.initialize(initialTransformedStates, initialCosts, predictions);
		dud.maxit = 10;
		// run optimization
		dud.optimize();

		// get optimal state
		// set state of ensemble[i] to this value, detour to avoid disturbing something
		IVector analysisIncrement = f.getOptimalState();
		analysisIncrement.axpy(-1.0, xiAvg); // update for average
		this.ensemble[0].restoreInternalState(savedMemberState); // restore member 0
		this.ensemble[0].releaseInternalState(savedMemberState);
		this.ensemble[0].releaseInternalState(savedAvgState);

		IMatrix transformL = dud.getSqrtCovariance(); // because initial transformed cov = eye
		for (int i = 0; i < q; i++) { // compute perturbations for each member
			//xi_a[i] = xi_f[i] + (xi_f_avg - xi_f[i]) + (xi_a_avg - xi_f_avg) + A*T[:,i]
			//                     > because we want to add to xi_f[i]
			//                                           > analysisIncrement
			//                                                                   > sqrt cov update
			deltaState = relStates[i].clone();
			deltaState.scale(-1.0);
			deltaState.axpy(1.0, analysisIncrement);
			for(int j=0;j<q;j++){
				deltaState.axpy(transformL.getValue(j, i), relStates[j]);
			}
			this.ensemble[i].axpyOnState(1.0, deltaState);
		}


		// collect output for this analysis
		for (int i = 0; i < q; i++) {
			updatedStates[i] = this.ensemble[i].getState();
			// collect predictions
			updatedPredictions[i] = this.ensemble[i].getObservedValues(
					stochObs.getObservationDescriptions());
		}
		xiAvg = ensembleAverage(updatedStates,q);
		// adjust mainModel (adjust mainModel to mean analyzed state)
		IVector deltaMain = xiAvg.clone();
		deltaMain.axpy(-1.0,mainModel.getState());
		mainModel.axpyOnState(1.0, deltaMain);

		xiStd = ensembleStd(updatedStates);
        Results.putValue("std_x_a", xiStd, xiStd.getSize(), "analysis step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
		predAvg = ensembleAverage(updatedPredictions,q);
		// extra output for checking
		removeAverage(updatedStates, xiAvg);
		Matrix LaMat = new Matrix(updatedStates);
		LaMat.scale(Math.sqrt(1.0 / (q - 1.0)));
        Results.putValue("L_a", LaMat, LaMat.getNumberOfColumns() * LaMat.getNumberOfRows(), "analysis step", IResultWriter.OutputLevel.All, IResultWriter.MessageType.Step);
	}
}
