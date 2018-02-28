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
import org.openda.utils.*;

import java.io.File;

/**
 * This filter implements an ensemble square-root filter EnSR as discussed by several authors 
 * (e.g. Evensen, Whitaker, Livings, Sakov) An important issue in the implementation is the 
 * freedom that seems to exist in the choice of the transformation matrix that is used. 
 * However, Sakov 2006 has shown that the symmtric solution is to be prefered since:
 * - it does not introduce an additional bias
 * - it does not modify the ensemble when inaccurate observations are  added
 * - it introduces the least additional sampling error. 
 * Therefore the implementation in this file is based on the paper by Sakov 2006,
 * "Only mean-preserving ensemble transformations should be used in Ensemble  Square Root Filters"
 * specifically eqns 10 and 13
 * (10)
 *      T = (I - (HL)'*inv(R)*(HL) )^{-1/2}
 * (13)
 *      T =  C * Gamma^{-1/2}*C'
 *  with the svd 
 *      C*Gamma*C' = (I - (HL)'*inv(R)*(HL) )
 *      
 * @author Martin Verlaan 
 * 
 * Remarks:
 * There is no Schur-product for spatial truncation yet
 */
public class EnSR extends AbstractSequentialEnsembleAlgorithm {

	public void analysis(IStochObserver obs, IVector obsValues, IVector predMainModel, 
			IStochModelInstance mainModel, ITime analysisTime) {
		int q = this.ensembleSize; // number of ensemble members
		IVector[] xi = new IVector[q];
		IVector[] pred = new IVector[q];
		for (int i = 0; i < q; i++) {
			// collect ensemble
			xi[i] = this.ensemble[i].getState();
			// collect predictions
			pred[i] = this.ensemble[i].getObservedValues(obs
					.getObservationDescriptions());
		}
		int m = obs.getCount(); // number of observations
		int n = xi[0].getSize(); // length of the state vector
		// compute ensemble average and subtract it from ensemble
		IVector xiAvg = ensembleAverage(xi);
		IVector xiStd = ensembleStd(xi);
        Results.putValue("x_f", xiAvg, xiAvg.getSize(), "analysis step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
        Results.putValue("std_x_f", xiStd, xiStd.getSize(), "analysis step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
		removeAverage(xi, xiAvg); // now xi = sqrt(q-1)*L
		IVector predAvg = ensembleAverage(pred);
        Results.putValue("pred_f", predAvg, predAvg.getSize(), "analysis step", IResultWriter.OutputLevel.Essential, IResultWriter.MessageType.Step);
		removeAverage(pred, predAvg); // now pred = sqrt(q-1)*H*L
		IVector obsVal = obs.getExpectations();
		//
		// compute Kalman gain
		// K = P*H'/(H*P*H'+R)
		Matrix predMat = new Matrix(pred);
		predMat.scale(Math.sqrt(1.0/(q-1))); // predMat = H*L
		// System.out.println("predMat="+predMat);
		// phi=(H*P*H'+R)
		Matrix phi = new Matrix(m, m);
		phi.multiply(1.0, predMat, predMat, 0.0, false, true); //phi=(HL)*(HL)'
		IMatrix sqrtR = obs.getSqrtCovariance().asMatrix();
        Results.putValue("sqrt_r", sqrtR, sqrtR.getNumberOfColumns() * sqrtR.getNumberOfRows() , "analysis step", IResultWriter.OutputLevel.All, IResultWriter.MessageType.Step);
		phi.multiply(1.0, sqrtR, sqrtR, 1.0, false, true); // phi+=R
        Results.putValue("hpht_plus_r", phi, phi.getNumberOfColumns() * phi.getNumberOfRows() , "analysis step", IResultWriter.OutputLevel.All, IResultWriter.MessageType.Step);
		// K = L*(HL)'*inv(phi) = XI * W
		Matrix W = new Matrix(q, m);
		Matrix inversePhi = phi.inverse();
		W.multiply(1.0, predMat,inversePhi, 0.0, true, false);
		//compute K and xa 
		Matrix LfMat = new Matrix(xi);
		LfMat.scale(Math.sqrt(1.0/(q-1)));
        Results.putValue("L_f", LfMat, LfMat.getNumberOfColumns() * LfMat.getNumberOfRows() , "analysis step", IResultWriter.OutputLevel.All, IResultWriter.MessageType.Step);
		Matrix K = new Matrix(n,m);
		K.multiply(1.0, LfMat,W, 0.0, false, false);
        Results.putValue("k", K, K.getNumberOfColumns() * K.getNumberOfRows() , "analysis step", IResultWriter.OutputLevel.All, IResultWriter.MessageType.Step);
		// compute updated mean state
		// xa = x + K*(y-Hx)
		IVector innovation = obsVal.clone(); //res=y-Hx
		innovation.axpy(-1.0, predAvg);
		IVector xiNewAvg=xiAvg.clone();
		K.rightMultiply(1.0, innovation, 1.0, xiNewAvg);
        Results.putValue("x_a_linear", xiNewAvg, xiNewAvg.getSize(), "analysis step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);

		/**
		* Compute analysis error covariance
		*
		* Pa = P - P*H'/(H*P*H'+R)*H*P
		* with P=L*L'
		* Pa = L*inv( I - (HL)'*inv(R)*(HL) )*L'
		* and with psi = inv(S)*H*L
		* Pa = L*inv( I - psi'*psi )*L'
		*
		* La = L * T
		* with
		* T = ( I - psi'*psi )^{-1/2}
		*
		*/
		// psi = this.ensembleSize
		ISqrtCovariance S = obs.getSqrtCovariance();
		IVector[] predNorm = new IVector[q];
		for(int i=0;i<q;i++){
			predNorm[i] = pred[i].clone();
			S.leftSolve(pred[i], predNorm[i]);
		}
		Matrix psi = new Matrix(predNorm);
		psi.scale(Math.sqrt(1.0/(q-1)));
		// chi = ( I + psi'*psi )
		Matrix chi = Matrix.eye(q);
		chi.multiply(1.0,psi, psi, 1.0,true,false);
		Matrix[] svdDecomposition = chi.svd();
		// U*D*V' = chi
		Matrix U = svdDecomposition[0];
		Matrix D = svdDecomposition[1];
		Matrix V = svdDecomposition[2];
		double[] d = D.diag().getValues();
		// temp is diagonal of D
		double[] temp = new double[q];
		for(int i=0;i<q;i++){
			temp[i] = Math.sqrt(1.0/d[i]);
		}
		IMatrix invSqrtD = new Matrix(temp);
		// symmetric transformation matrix
	    // T = ( I - psi'*psi )^{-1/2}
        IMatrix Ts = U.mult(invSqrtD).mult(V.transpose());

		//
		// update the ensemble
		//
		// L_a = x_a +L_a
        //
        // TODO only simple implementation with matrices is used now
        // should be optimized later to use state-vectors directly
        IVector[] L_a = Matrix.mult(LfMat,Ts).asVectorArray();
        Matrix mL_a = new Matrix(L_a);
        Results.putValue("L_a", mL_a , mL_a.getNumberOfColumns() * mL_a.getNumberOfRows(), "analysis step", IResultWriter.OutputLevel.All, IResultWriter.MessageType.Step);
        for(int i=0;i<q;i++){
        	// compute change to i-th' ensemble state
    		IVector xChange    = xiNewAvg.clone();
    		IVector xi_i_f     = ensemble[i].getState();
    		xChange.axpy(-1.0,xi_i_f);
    		xChange.axpy(Math.sqrt(q-1),L_a[i]);
    		// apply to the model state
    		this.ensemble[i].axpyOnState(1.0, xChange);
        }
		//
		// collect output for this analysis
		//
		for (int i = 0; i < this.ensembleSize; i++) {
			xi[i] = this.ensemble[i].getState();
			// collect predictions
			pred[i] = this.ensemble[i].getObservedValues(obs
					.getObservationDescriptions());
		}
		xiAvg = ensembleAverage(xi);
		// adjust mainModel (adjust mainModel to mean analyzed state)
		IVector deltaMain = xiAvg.clone();
		deltaMain.axpy(-1.0,mainModel.getState());
		mainModel.axpyOnState(1.0, deltaMain);
		
		xiStd = ensembleStd(xi);
        Results.putValue("std_x_a", xiStd , xiStd.getSize(), "analysis step", IResultWriter.OutputLevel.Verbose, IResultWriter.MessageType.Step);
		predAvg = ensembleAverage(pred);
	}
}

