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

import org.openda.interfaces.IStochObserver;
import org.openda.interfaces.IVector;
import org.openda.utils.DistributedCounter;

import java.util.Random;

/**
 * @author Nils van Velzen
 *         Automatic localization algorithm as described in
 *         Yanfen Zhang and Dean S. Oliver
 *         Evaluation and error analysis: Kalman gain regularisation versus covariance
 *         regularisation
 *         Comput. Geosci (2011) 15:489-508
 *         DOI 10.1007/s10596-010-9218-y
 *
 *         Note: This is an initial straight forward implementation which is NOT memory
 *               efficient. At some point we will need a version that computes the localization
 *               weights in an element wise way such that we do not need to hold an ensemble of
 *               gain matrices.
 */
public class AutoLocalizationZhang2011 {
	private double sigmaAlpha2=0.36;  //Optimal value (see paper)
	private int nBootstrap=50;

	//Use the DistributedCounter to make sure we have different seeds in a parallel run
	//(probably not needed but is does not hurt ;-))
	private static DistributedCounter SeedWithOffset = new DistributedCounter(20100816);
	private static Random generator = new Random(SeedWithOffset.val());

	public IVector[] computeObservedLocalization(EnKF algorithmEnkF, IStochObserver obs, EnsembleVectors ensemblePredictions, EnsembleVectors ensembleVectors, boolean write_output){

		if (write_output) {
		System.out.println("Debug: We doen lokalizatie volgens Zhang +nbootstrap="+this.nBootstrap);
		}
		//Create an ensemble of gain matrices
		IVector [][] gainMatrices = createBootstrapGainsMatrices(algorithmEnkF, obs, ensemblePredictions, ensembleVectors, write_output);
		//for (int i=0; i<gainMatrices.length; i++){
		//System.out.println("gain("+i+")="+gainMatrices[i][0]);
		//}



		//Get work matrices
		IVector [] work1 = createZeroWorkMatrix( gainMatrices[0]);
		IVector [] work2 = createZeroWorkMatrix( gainMatrices[0]);


		//Compute main of matrix (note work1 is returned and should be 0 on entry
		IVector[] gainMean = computeGainMean(gainMatrices, work1);
		work1=null;
		//System.out.println("gainMean=" + gainMean[0]);

		//Remove the mean from the ensemble of gain matrices and square the elements
		IVector[][] gainMatricesMinMeanSquared = removeMeanAndSquare(gainMatrices, gainMean);
		gainMatrices=null;

		//Square the mean
		IVector[] gainMeanSquared = squareGainMean(gainMean);
		//System.out.println("gainMeanSquared="+gainMeanSquared[0]);
		gainMean=null;

		//Compute sigma2 = mean(gainMatricesMinMeanSquared)
		IVector[] sigma2 = computeSigma2(gainMatricesMinMeanSquared, work2);
		//System.out.println("sigma2="+sigma2[0]);
		work2=null;

		//Divide sigma2 by the mean
		IVector[] C2 =  divideByMeanSquare(gainMeanSquared, sigma2);
		sigma2=null;
		gainMeanSquared=null;
		//System.out.println("C2="+C2[0]);


		//Compute the localization weights alpha_i,j=1/(1+(1/sigmaAlpha2)*C2  )
		IVector[] rho = computeRho(C2);
		C2=null;
		//System.out.println("rho="+rho[0]);

		return rho;
	};


	private IVector[] computeSigma2(IVector[][] gainMatricesMinMeanSquared, IVector[] work){
		double alpha=1.0/(double) nBootstrap;
		int nObs= work.length;
		for (int iBootstrap=0; iBootstrap<this.nBootstrap; iBootstrap++){
			for (int iObs=0; iObs<nObs; iObs++){
				work[iObs].axpy(alpha,gainMatricesMinMeanSquared[iBootstrap][iObs]);
			}
		}
		return work;
	}

	private IVector[] computeRho(IVector[] C2){
		int nObs= C2.length;
		IVector ones = C2[0].clone();
		ones.setConstant(1.0);
		double beta=1.0+1.0/sigmaAlpha2;
		for (int iObs=0; iObs<nObs; iObs++){
			C2[iObs].scale(beta);
			C2[iObs].axpy(1.0,ones);
			//System.out.println("C2 voor resiproce: "+C2[iObs]);

			ones.pointwiseDivide(C2[iObs]);
			//System.out.println("C2 (=ones) na resiproce: "+ones);

			IVector swap=C2[iObs];
			C2[iObs]=ones;
			ones=swap;
			ones.setConstant(1.0);
		}
		return C2;
	}


	private IVector[] divideByMeanSquare(IVector[] meanSquare, IVector[] sigma2){
		int nObs= meanSquare.length;
		IVector wrk=meanSquare[0].clone();
		wrk.setConstant(1.0e-16);
		for (int iObs=0; iObs<nObs; iObs++){
			meanSquare[iObs].axpy(1.0,wrk);
			sigma2[iObs].pointwiseDivide(meanSquare[iObs]);
		}

		for (int iObs=0; iObs<nObs; iObs++){
			double [] values = sigma2[iObs].getValues();
			for (int i=0; i<values.length; i++){
				if (values[i]<0.0){
					throw new RuntimeException("Strange value "+i+" of for observation "+iObs+" is negative :"+values[i]);
				}
			}
		}


		return  sigma2;
	}

	private IVector[] squareGainMean(IVector[] gainMean){
		int nObs= gainMean.length;
		for (int iObs=0; iObs<nObs; iObs++){
			gainMean[iObs].pointwiseMultiply(gainMean[iObs]);
		}
		return gainMean;
	}


	private IVector[][] removeMeanAndSquare(IVector[][] gainMatrices, IVector[] gainMean){
		// Remove mean and square
		int nObs= gainMean.length;
		for (int iBootstrap=0; iBootstrap<this.nBootstrap; iBootstrap++){
			for (int iObs=0; iObs<nObs; iObs++){
				gainMatrices[iBootstrap][iObs].axpy(-1.0, gainMean[iObs]);
				gainMatrices[iBootstrap][iObs].pointwiseMultiply(gainMatrices[iBootstrap][iObs]);
			}
		}
		return  gainMatrices;
	}



	private IVector[] createZeroWorkMatrix(IVector[] gain){
		int nObs=gain.length;
		IVector [] work = new IVector[nObs];

		for (int iObs=0; iObs<nObs; iObs++){
			work[iObs]=gain[0].clone();
			work[iObs].setConstant(0.0);
		}
	    return work;

	}

	private IVector[] computeGainMean(IVector[][]gainMatrices, IVector[] wrkReturn){
		double alpha=1.0/(double) nBootstrap;
		int nObs= gainMatrices[0].length;
		for (int iBootstrap=0; iBootstrap<this.nBootstrap; iBootstrap++){
			for (int iObs=0; iObs<nObs; iObs++){
				wrkReturn[iObs].axpy(alpha,gainMatrices[iBootstrap][iObs]);
			}
		}
		return  wrkReturn;
	}



	private IVector[][] createBootstrapGainsMatrices(EnKF algorithmEnkF, IStochObserver obs, EnsembleVectors ensemblePredictions, EnsembleVectors ensembleVectors, boolean write_output){

		//Create an ensemble of gain matrices
		IVector [][] gainMatrices = new IVector[this.nBootstrap][];
		int n=ensembleVectors.getEnsembleSize();
		IVector [] bootstrapEnsembleVectors = new IVector[n];
		IVector [] bootstrapEnsemblePredictions = new IVector[n];
		//for (int i=0; i<n; i++){
		//	bootstrapEnsembleVectors[i]=ensembleVectors.ensemble[0].clone();
		//	bootstrapEnsembleVectors[i].axpy(1.0,ensembleVectors.mean);
		//	bootstrapEnsemblePredictions[i]=ensemblePredictions.ensemble[0].clone();
		//	bootstrapEnsemblePredictions[i].axpy(1.0, ensemblePredictions.mean);
		//}

		for (int iBootstrap=0; iBootstrap<this.nBootstrap; iBootstrap++){
			if (write_output) {
			System.out.println("Bootstrap "+iBootstrap+" of "+ this.nBootstrap);
			}
			for (int i=0; i<n; i++){
				int iRand = generator.nextInt(n);
				bootstrapEnsembleVectors[i]=ensembleVectors.ensemble[iRand].clone();
				bootstrapEnsemblePredictions[i]=ensemblePredictions.ensemble[iRand].clone();
				bootstrapEnsembleVectors[i].axpy(1.0,ensembleVectors.ensemble[iRand]);
				bootstrapEnsemblePredictions[i].axpy(1.0,ensemblePredictions.ensemble[iRand]);
			}
			// Create a bootstrap ensemble
			EnsembleVectors bootstrapEnsemble = new EnsembleVectors(bootstrapEnsembleVectors);
			EnsembleVectors bootstrapPredictions = new EnsembleVectors(bootstrapEnsemblePredictions);

			// Greate gain matrix
			gainMatrices[iBootstrap] = algorithmEnkF.computeGainMatrix(obs, bootstrapPredictions, bootstrapEnsemble, false, write_output);
		}
		return gainMatrices;
	}








}
