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

public class DudCoreOptimizer extends BaseDudCoreOptimizer {


	/**
	 * Constructor for Dud minimization - initializes from parameterUncertainty
	 * @param f      : costfunction to be minimized
	 */
	public DudCoreOptimizer(LeastSquaresCostFunction f) {
		this.f = f;
		int m = numberOfSearchDirections(); 
		number_of_stored_runs = m+1;
		number_of_evaluations = m;
	}
	public DudCoreOptimizer(LeastSquaresCostFunction f, IVector pInit) {
		this.f = f;
		int m = numberOfSearchDirections(); 
		number_of_stored_runs = m+1;
		number_of_evaluations = m;
		initialize(pInit);
	}

	@Override
	protected double InitialSearchStep(int i, int j) {
		return (i==j ? 1 : 0);
	}


	/* 
	 * The best iterand we have until now is pars[0] (with predictions preds[0] = f.evaluate(pars[0]))
	 * 
	 * A linear system will be set up for pStep, which from which the next iterand pars can be 
	 * calulated as
	 * 
	 *    pars = pars[0] + [pars[1]-pars[0], ... , pars[n]-pars[0]] * pStep,
	 * 
	 * or, when under-relaxation is needed due to the nonlinear nature of the equations:
	 * 
	 *    pars = pars[0] + [pars[1]-pars[0], ... , pars[n]-pars[0]] * pStep.
	 * 
	 * The matrix P_matrix is now calculated, so that this iterand pars can be written as
	 *    
	 *    pars = pars[0] + P_matrix * pStep,      
	 * or
	 *    pars = pars[0] + innerScaleFac * P_matrix * pStep.
	 */
	@Override
	protected Matrix CalculateParsMatrix() {
		int ndirs        = number_of_evaluations;
		IVector[] pars   = this.pCurrent;
		IVector[] vecs   = new IVector[ndirs];
		for (int i = 0; i < ndirs; i++) { 
			vecs[i] = pars[i+1].clone();
			vecs[i].axpy(-1.0, pars[0]);
		}		
		return new Matrix(vecs);
	}

	@Override
	protected Matrix CalculateGradSimu() {
		int ndirs       = number_of_evaluations;
		IVector[] preds = this.predCurrent;
		IVector[] vecs  = new IVector[ndirs];
		for (int i = 0; i < ndirs; i++) {
			vecs[i] = preds[i+1].clone();
			vecs[i].axpy(-1.0, preds[0]);
			vecs[i].pointwiseDivide(sigmaObs);
		}
		return new Matrix(vecs);
	}

	@Override
	protected Matrix BackgroundMatrix() {
		int ndirs        = number_of_evaluations;
		IVector[] pars   = this.pCurrent;
		IVector[] vecs = new IVector[ndirs];
		for (int i=0; i<ndirs; i++) {
			vecs[i] = pars[i+1].clone();
			vecs[i].axpy(-1.0, pars[0]);
			LPar.rightSolve(vecs[i],vecs[i]);
		}
		return new Matrix(vecs);
	}
}
