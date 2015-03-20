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

package org.openda.interfaces;

/**
 * Abstract representation of a square-root covariance matrix. If P is the covariance of a
 * StochVector then any "matrix" L satisfying P=L*transpose(L) is called a
 * Square-root-covariance. A SqrtCovariance can in theory always be represented by a matrix
 * but if the dimensions are large then it may be much more efficient to work with the
 * operators provided here instead.
 */
public interface ISqrtCovariance {

    /**
     * Gets the number of rows of the SqrtCovariance, which equals the number of elements of
     * any realization from the underlying stochTreeVector
     * @return number of rows of L
     */
    int getNumberOfRows();

    /**
     * Gets the number of columns of the SqrtCovariance, which equals the rank of the matrix
     * (if less than length). If this number is much smaller than L.getLength()
     * than a square-root approach reduces storage and computations.
     * @return number of columns of L
     */
    int getNumberOfColumns();

    /**
     * Perform matrix-vector multiplication x=alpha*x+beta*L*v, but then implicitly and often more efficiently
     * @param beta scaling parameter for matrix multiplication
     * @param v vector used for multiplication (in most cases a "small" vector number of columns of L)
     * @param alpha scaling parameter for vector x
     * @param x updated vector in multiplication (in most cases a "large" vector of size of model state)
     */
     void rightMultiply(double beta, IVector v, double alpha, IVector x);

    /**
     * Perform matrix-vector multiplication v=alpha*v+beta*(x'*L)', but then implicitly and often more efficiently
     * @param beta scaling parameter for matrix multiplication
     * @param x vector used for multiplication (in most cases a "large" vector of size of model state)
     * @param alpha scaling parameter for vector v
     * @param v updated vector in multiplication (in most cases a "small" vector number of columns of L)
     */
     void leftMultiply(double beta, IVector x, double alpha, IVector v);

    /**
     * Perform matrix-vector solve x=inv(L)*v, but then implicitly and often more efficiently.
     * This is the (pseudo-)inverse of leftMultiply.
     * @param v vector used for linear solver
     * @param x result of linear solve
     */
     void leftSolve(IVector v, IVector x);

    /**
     * Perform matrix-vector solve for v in v=L*x, but then implicitly and often more
     * efficiently. This is the (pseudo-)inverse of rightMultiply.
     * @param x vector used for right solve
     * @param v result of linear solver
     */
     void rightSolve(IVector x, IVector v);

    /**
     * Return SqrtCovariance as Matrix
     * @return matrix representation of L
     */
     IMatrix asMatrix();

    /**
     * Return part of the SqrtCovariance as Matrix
     * @param rowmin start row of selection
     * @param rowmax end row of selection
     * @param colmin start column of selection
     * @param colmax end column of selection
     * @return matrix representation of L
     */
     IMatrix getMatrixSelection(int rowmin, int rowmax, int colmin, int colmax);

    /**
     * Return SqrtCovariance as an array of Vectors, where each tv represents one column of L
     * @return tv[] representation of L
     */
     IVector[] asVectorArray();

}
