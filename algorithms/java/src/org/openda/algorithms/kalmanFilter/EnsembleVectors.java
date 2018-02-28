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


import org.openda.interfaces.IVector;

/**
 * @author Nils van Velzen
 *         Class for holding general ensembles of vectors/treevectors
 *         Data container for the implementation of Ensemble based methods
 */
public class EnsembleVectors {
	//Important note: ensemble only stores the deviations from the mean.
	//one vector for each ensemble member. Each vector has length of state vector.
	public IVector[] ensemble;           // ensemble (mean removed)
	public IVector   mean;               // mean
	public IVector   standardDeviation;  // Standard deviation


	public EnsembleVectors(){
	}

	public EnsembleVectors(IVector[] ensemble){
		this.ensemble = ensemble;
		computeAndSetEnsembleMean();
		removeMeanFromEnsemble();
		computeAndSetstandardDeviation(true);
	}


	public void computeAndSetEnsembleMean(){
		if (this.mean!=null) this.mean.free();
		int n=ensemble.length;
		double alpha = 1.0/((double) n);
		this.mean=ensemble[0].clone();
		for (int i=1; i<n; i++){
			this.mean.axpy(1.0, this.ensemble[i]);
		}
		this.mean.scale(alpha);
	}

	public void removeMeanFromEnsemble(){
		int n=ensemble.length;
		for (int i=1; i<n; i++){
			this.ensemble[i].axpy(-1.0, this.mean);
		}
	}

    public void addMeanToEnsemble(){
		int n=ensemble.length;
		for (int i=1; i<n; i++){
			this.ensemble[i].axpy(1.0, this.mean);
		}
	}

	public void computeAndSetstandardDeviation(boolean meanAlreadyRemoved){
		int n=ensemble.length;
		if (this.standardDeviation!=null) this.mean.free();

		IVector X2=this.ensemble[0].clone();
		this.standardDeviation=this.ensemble[0].clone();
		this.standardDeviation.setConstant(0.0);
		double alpha=1.0/((double) n-1.0);
		if (meanAlreadyRemoved){
			for (int i=1; i<n; i++){
				X2.setConstant(0.0);
				X2.axpy(1.0,this.ensemble[i]);
				X2.pointwiseMultiply(this.ensemble[i]);
				this.standardDeviation.axpy(alpha, X2);
			}
			this.standardDeviation.sqrt();
		}
		else {
			throw new RuntimeException("Option meanAlreadyRemoved=false not yet implemented");
		}
	}

	public int getEnsembleSize(){
		return this.ensemble.length;
	}

	public void free(){
		if (ensemble != null){
			for (int iEns=0; iEns<ensemble.length; iEns++){
				if (ensemble[iEns]!=null) {
					ensemble[iEns].free();
				}
			}
		}
		if (mean!=null){
			mean.free();
		}
		if (standardDeviation!=null){
			standardDeviation.free();
		}
	}
}
