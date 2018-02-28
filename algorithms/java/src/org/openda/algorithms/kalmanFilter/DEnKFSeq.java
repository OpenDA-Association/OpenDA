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

import org.openda.interfaces.IStochObserver;
import org.openda.interfaces.IVector;

/**
 * @author Nils van Velzen based in the initial OpenDA implementation by Martin Verlaan
 *         Traditional Ensemble Kalman filter as introduced by
 *         Evensen and Burgers. This is a classical implementation processing
 *         all observations at once. There is no Schur-produkt for spatial
 *         truncation.
 *         This version assimilates the observations one at a time and does not explicitly contruct
 *         the gain matrix like the DEnKF implementation
 */
public class DEnKFSeq extends EnKFSeq {


	// Update the Ensemble. Differences between DEnKF and EnKF:
	// a) no noise on observations
	// b) update covariance with 0.5 x Gain matrix
	protected void updateEnsembleWithGain(IStochObserver obs, EnsembleVectors ensemblePredictions, EnsembleVectors ensembleVectors, IVector[] Kvecs){
		timerGainMult.start();
		System.out.println("Update Ensemble DEnKF");
		// correct each member
		double[] innovations = new double[ensembleVectors.ensemble.length];
		double innovationMean=0.0;
		for (int i = 0; i < this.ensembleSize; i++){
			IVector innovation = obs.getExpectations();
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
			//* update spread with 0.5 times the gain !!
			ensembleVectors.ensemble[i].axpy(0.5*innovations[i], Kvecs[0]);
		}
		ensembleVectors.mean.axpy(innovationMean, Kvecs[0]);
		timerGainMult.stop();
	}
}
