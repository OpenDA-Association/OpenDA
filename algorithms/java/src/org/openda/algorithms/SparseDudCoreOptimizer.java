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
package org.openda.algorithms;

import org.openda.interfaces.*;
import org.openda.utils.Matrix;
import org.openda.utils.Results;
import org.openda.utils.Vector;

public class SparseDudCoreOptimizer extends BaseDudCoreOptimizer {


	// SparseDud Only stuff (i.e. not DuD):
	private SparsenessPattern sparsenessPattern;
	private int rand_counter;
	public int idebug = 1; 

	/**
	 * Constructor for Sparse Dud minimization - initializes from parameterUncertainty
	 * @param f      : cost function to be minimized
	 * @param pInit  : initial parameters
	 */
	public SparseDudCoreOptimizer(LeastSquaresCostFunction f, IVector pInit,int[][] nonZeros) {
		rand_counter = 0;
		sparsenessPattern = new SparsenessPattern(nonZeros);

		this.f = f;
		int m = numberOfSearchDirections();
		number_of_stored_runs = Math.max(m+1,6);
		number_of_evaluations = sparsenessPattern.max_nnz_per_row;
		initialize(pInit);
	}
	
	/**
	 * Constructor for Sparse Dud minimization - initializes from parameterUncertainty
	 * @param f        cost function to be minimized
	 * @param nonZeros matrix containing the sparseness pattern
	 */
	public SparseDudCoreOptimizer(LeastSquaresCostFunction f, int[][] nonZeros) {
		rand_counter = 0;
		sparsenessPattern = new SparsenessPattern(nonZeros);

		this.f = f;
		int m = numberOfSearchDirections();
		number_of_stored_runs = Math.max(m+1,6);
		number_of_evaluations = sparsenessPattern.max_nnz_per_row;
	}

	
	protected double InitialSearchStep(int i, int j) {
		return nep_rand();
	}
	private double nep_rand() {
		rand_counter++;
		return (0.6 * Math.sin(5 + rand_counter * 3) + 0.4
				* Math.sin(2 + rand_counter / 3) + 0.2 * Math
				.sin(1 + rand_counter));
	}


	
	protected Matrix CalculateGradSimu() {
		// create the matrix which describes the gradient of predictions with respect to parameters

		// get data needed by the algorithm
		IVector[] pars = pCurrent;
		IVector[] preds = predCurrent;

		int ndirs = number_of_evaluations; // number of search directions (so far)
		int npars = pars[0].getSize();     // number of elements for a parametervector
		int nobs = preds[0].getSize();

		IVector rhs_obs = obs.clone(); 
		rhs_obs.axpy(-1.0, preds[0]);
		rhs_obs.pointwiseDivide(sigmaObs);
		Results.putMessage("RHS norm: "+rhs_obs.norm2());

		double[] scaleDirs = new double[ndirs];
		for (int i = 0; i < ndirs; i++) { 
			IVector resNorm_i = obs.clone();
			resNorm_i.axpy(-1.0, preds[i+1]);
			resNorm_i.pointwiseDivide(sigmaObs);
			scaleDirs[i] = rhs_obs.norm2() / resNorm_i.norm2();
		}

		IVector[] predsNorm = new IVector[ndirs];
		for (int i = 0; i < ndirs; i++) { 
			predsNorm[i] = preds[i+1].clone();
			predsNorm[i].axpy(-1.0, preds[0]);
			predsNorm[i].pointwiseDivide(sigmaObs);
		}

		IVector[] parsRel   = new IVector[ndirs];
		for (int i = 0; i < ndirs; i++) { 
			parsRel[i] = pars[i+1].clone();
			parsRel[i].axpy(-1.0, pars[0]);
		}
		
		Matrix A_obs = new Matrix(npars, nobs, 0.0);

		/* loop over number of patterns; typically the number of timeseries (=number of stations). 
		 *   For example, suppose 8 measurement stations, 10s with 0.1s intervals. 
		 *   Then 808 (nobs) observations,
		 *   but only 8 patterns. */
		// Each loop sweep, part of the gradsimu matrix will be filled.
		for (int ipat = 0; ipat < sparsenessPattern.columnPatterns.length; ipat++) {
			int[] columnPattern = sparsenessPattern.columnPatterns[ipat]; // column number (between 0 and Npar-1) with nonzeros

			/* Look up the observations with pattern ipat.
			 * (typically all observations of the same timeseries) */
			int[] irows = sparsenessPattern.patterns2Rows[ipat]; 
			int Nobs_ipat = irows.length;

			if (idebug>=3){ 
				printarray_i("columnpattern", columnPattern);
				Results.putProgression("number of rows with pattern " + ipat + ":" + irows.length);
			}
			// --------------------------------------------------------------------------
			// construct matrix dP^T of dimension N x Npar_nnz where N = ndirs
			Matrix dP_matrix;
			{
				Vector[] dP_vectors = new Vector[columnPattern.length]; // size:  length(nnz(ipat)) x ndirs
				int npat =  columnPattern.length;
				for (int i = 0; i < npat; i++) {
					int ipar = columnPattern[i];
					double dPi[] = new double[ndirs]; // size: ndirs=N
	
					for (int idir = 0; idir < ndirs; idir++) {
						// parsRel = [par1-par0 ... parN-par0]. Size: N x Npar.
						// size(parsRel[0]) = Npar
						double[] t1 = parsRel[idir].getValues(); // size(t1) = nPar
						dPi[idir] = t1[ipar] * scaleDirs[idir];
					}
					dP_vectors[i] = new Vector(dPi);
				}
				dP_matrix = new Matrix(dP_vectors); // matrixsize:  ndirs x length(nnz(ipat))
			}

			// -------------------------------------------------------------
			// construct matrix dS_matrix^T of dimension N x Nobs_ipat
			Matrix dS_matrix;
			{
				Vector[] dS_vectors = new Vector[Nobs_ipat];
				for (int iobs = 0; iobs < Nobs_ipat; iobs++) {
					// nb: predsNorm[jj].length = nobs; jj<N.
	
					int irow = irows[iobs];
					double dSi[] = new double[ndirs]; // size: ndirs=N
	
					for (int idir = 0; idir < ndirs; idir++) {
						double[] t2 = predsNorm[idir].getValues(); // size t2: nobs
						dSi[idir] = t2[irow] * scaleDirs[idir];
					}
					// idum = printarray_d("dSi",dSi);
					dS_vectors[iobs] = new Vector(dSi);
				}
				dS_matrix = new Matrix(dS_vectors); // matrixsize:  ndirs x nobs_ipat
			}

			// ------------------------------------------------------------------------
			// Now compute part of A_obs as a least squares solution:
			//        dP * x = dS --> x = (dP^t dP)^-1 dP^t dS
			Matrix xvals = new Matrix(columnPattern.length, Nobs_ipat);
			
			// Using rightSolve causes error 'Matrix cannot be cast to Jama.Matrix'
			boolean CanDoLSsystems = false;
			if (CanDoLSsystems) {
				dP_matrix.rightSolve(dS_matrix, xvals);
			} else {
				Matrix AtB = dP_matrix.simpletranspose().mult(dS_matrix); // A^t B
				Matrix AtA = dP_matrix.simpletranspose().mult(dP_matrix); // A^t A 
				xvals = AtA.inverse().mult(AtB);
				if (idebug>=6) { 	
					PrintMatrix("P^T*dS",AtB);
					PrintMatrix("dP^T*dP",AtA); 
				}
			}
			
			if (idebug>=4) { 	
				PrintMatrix("dP_Matrix",dP_matrix);		
				PrintMatrix("dS^T",dS_matrix.simpletranspose());
			}
			if (idebug>=3) { 	
				PrintMatrix("xvals^T = dS^T/(dP^t*dP)",xvals.simpletranspose());
			}
		
			// now fill a part of the A_obs matrix in a double loop
			for (int ii = 0; ii < Nobs_ipat; ii++) {
				for (int jj = 0; jj < columnPattern.length; jj++) {
					double vald = xvals.getValue(jj, ii);
					A_obs.setValue(columnPattern[jj], irows[ii], vald);
				}
			}

		} // end: for ipat
		
		if (idebug>=2)   PrintMatrix("A_obs^T",A_obs.simpletranspose());
		return A_obs.simpletranspose();
	}

	
	protected Matrix CalculateParsMatrix() {
		return IdentityMatrix(this.pCurrent[0]);
	}
	private Matrix IdentityMatrix(IVector vec) {
		int dim         = vec.getSize();
		IVector[] vecs   = new IVector[dim];
		for (int i = 0; i < dim; i++) { 
			vecs[i] = vec.clone();
			vecs[i].setConstant(0.0);
			vecs[i].setValue(i, 1.0);
		}		
		return new Matrix(vecs);
	}

	
	protected Matrix BackgroundMatrix() {
		IVector[] pars = this.pCurrent;
		int npars      = pars[0].getSize();
		IVector[] vecs = new IVector[npars];
		for (int i=0; i<npars; i++) {
			vecs[i] = pars[0].clone();
			vecs[i].setConstant(0.0);
			vecs[i].setValue(i, 1.0);
			LPar.rightSolve(vecs[i],vecs[i]);
		}
		return new Matrix(vecs);
	}

	private int printarray_i(String str, int[] iar) {
		Results.putProgression("array " + str + " (length=" + iar.length + ") : [" );
		String line = "   ";
		for (int i = 0; i < iar.length; i++) {
			line += iar[i] + "  ";
		}
		Results.putProgression(line + "]");
		return 0;
	}
	
	private void PrintMatrix(String Name, Matrix M) {
		int ncols = M.getNumberOfColumns();
		int nrows = M.getNumberOfRows();
		Results.putProgression("Matrix " + Name + " (" + nrows + " x " + ncols + "):");
		for (int irow = 0; irow < nrows; irow++) {
			Results.putProgression("     " + M.getMatrixSelection(irow, irow, 0, ncols-1).toString());
		}
	}
}
