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


using System;

namespace OpenDA.DotNet.Interfaces
{
    /**
     * Abstract representation of a square-root covariance matrix. If P is the covariance of a
     * StochVector then any "matrix" L satisfying P=L*transpose(L) is called a
     * Squar-root-covariance. A SqrtCovariance can in theory always be represented by a matrix
     * but if the dimensions are large then it may be much more efficient to work with the
     * operators provided here instead.
     */

    public interface ISqrtCovariance
    {
        /// <summary>
        /// Gets the number of rows of the SqrtCovariance, which equals the number of elements of
        /// any realization from the underlying stochTreeVector
        /// </summary>
        /// <returns>number of rows of L</returns>
        int getNumberOfRows { get; }

        /// <summary>
        ///  * Gets the number of columns of the SqrtCovariance, which equals the rank of the matrix
        /// (if less than length). If this number is much smaller than L.getLength()
        /// than a square-root approach reduces storage and computations.
        /// </summary>
        /// <returns>number of columns of L</returns>
        int getNumberOfColumns { get; }

        /// <summary>
        /// Perform matrix-vector multiplication x=alpha*x+beta*L*v, but then implicitly and often more efficiently
        /// </summary>
        /// <param name="beta">scaling parameter for matrix multiplication</param>
        /// <param name="v">vector used for multiplication (in most cases a "small" vector number of columns of L)</param>
        /// <param name="alpha">scaling parameter for vector x</param>
        /// <param name="x">updated vector in multiplication (in most cases a "large" vector of size of model state)</param>
        void rightMultiply(double beta, IVector v, double alpha, IVector x);

        /// <summary>
        /// Perform matrix-vector multiplication v=alpha*v+beta*(x'*L)', but then implicitly and often more efficiently
        /// </summary>
        /// <param name="beta">scaling parameter for matrix multiplication</param>
        /// <param name="x">vector used for multiplication (in most cases a "large" vector of size of model state)</param>
        /// <param name="alpha">scaling parameter for vector v</param>
        /// <param name="v">updated vector in multiplication (in most cases a "small" vector number of columns of L)</param>
        void leftMultiply(double beta, IVector x, double alpha, IVector v);

        /// <summary>
        /// Perform matrix-vector solve x=inv(L)*v, but then implicitly and often more efficiently.
        /// This is the (pseudo-)inverse of leftMultiply.
        /// </summary>
        /// <param name="v">vector used for linear solver</param>
        /// <param name="x">result of linear solve</param>
        void leftSolve(IVector v, IVector x);

        /// <summary>
        /// Perform matrix-vector solve for v in v=L*x, but then implicitly and often more
        /// efficiently. This is the (pseudo-)inverse of rightMultiply.
        /// </summary>
        /// <param name="x">vector used for right solve</param>
        /// <param name="v">result of linear solver</param>
        void rightSolve(IVector x, IVector v);

        /// <summary>
        /// Return SqrtCovariance as Matrix
        /// </summary>
        /// <returns>matrix representation of L</returns>
        IMatrix asMatrix();

        /// <summary>
        /// Return part of the SqrtCovariance as Matrix
        /// </summary>
        /// <param name="rowmin">start row of selection</param>
        /// <param name="rowmax">end row of selection</param>
        /// <param name="colmin">start column of selection</param>
        /// <param name="colmax">end column of selection</param>
        /// <returns>matrix representation of L</returns>
        IMatrix getMatrixSelection(int rowmin, int rowmax, int colmin, int colmax);

        /// <summary>
        /// Return SqrtCovariance as an array of Vectors, where each tv represents one column of L
        /// </summary>
        /// <returns>tv[] representation of L</returns>
        IVector[] asVectorArray();
    }
}
