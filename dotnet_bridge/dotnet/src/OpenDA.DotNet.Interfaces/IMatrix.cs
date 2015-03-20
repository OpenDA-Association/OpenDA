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


using System.Collections.Generic;


namespace OpenDA.DotNet.Interfaces
{
    /**
    * Matrix
    */
    public interface IMatrix
    {
        /// <summary>
        /// Gets the number of rows of the Matrix.
        /// </summary>
        /// <returns>number of rows of matrix</returns>
        int NumberOfRows { get; }

        /// <summary>
        /// Gets the number of columns of the Matrix.
        /// </summary>
        /// <returns>number of columns of L</returns>
        int NumberOfColumns { get; }

        /// <summary>
        /// Perform matrix-vector multiplication x=alpha*x+beta*L*v, but then implicitly and often more efficiently
        /// </summary>
        /// <param name="beta">scaling parameter for matrix multiplication</param>
        /// <param name="v">vector used for multiplication (in most cases a "small" vector number of columns of L)</param>
        /// <param name="alpha">scaling parameter for vector x</param>
        /// <param name="x">updated vector in multiplication (in most cases a "large" vector of size of model state)</param>
        void RightMultiply(double beta, IVector v, double alpha, IVector x);

        /// <summary>
        /// Perform matrix-vector multiplication v=alpha*v+beta*(x'*L)', but then implicitly and often more efficiently
        /// </summary>
        /// <param name="beta">scaling parameter for matrix multiplication</param>
        /// <param name="x">vector used for multiplication (in most cases a "large" vector of size of model state)</param>
        /// <param name="alpha">scaling parameter for vector vupdated vector in multiplication (in most cases a "small" vector number of columns of L)</param>
        /// <param name="v"></param>
        void LeftMultiply(double beta, IVector x, double alpha, IVector v);

        /// <summary>
        /// Perform matrix-vector solve x=inv(L)*v, but then implicitly and often more efficiently.
        /// This is the (pseudo-)inverse of leftMultiply.
        /// </summary>
        /// <param name="v">vector used for linear solver</param>
        /// <param name="x">result of linear solve</param>
        void LeftSolve(IVector v, IVector x);

        /// <summary>
        /// Perform matrix-vector solve for v in v=L*x, but then implicitly and often more
        /// efficiently. This is the (pseudo-)inverse of rightMultiply.
        /// </summary>
        /// <param name="x">vector used for right solve</param>
        /// <param name="v">result of linear solver</param>
        void RightSolve(IVector x, IVector v);

        /// <summary>
        /// Return part of the SqrtCovariance as Matrix
        /// </summary>
        /// <param name="rowmin">start row of selection</param>
        /// <param name="rowmax">end row of selection</param>
        /// <param name="colmin">start column of selection</param>
        /// <param name="colmax">end column of selection</param>
        /// <returns>matrix representation of L</returns>
        IMatrix GetMatrixSelection(int rowmin, int rowmax, int colmin, int colmax);

    	/// <summary>
    	/// Return SqrtCovariance as an array of Vectors, where each tv represents one column of L
    	/// </summary>
    	/// <returns>tv[] representation of L</returns>
    	List<IVector> AsVectorArray();

        /// <summary>
        /// Get/Set single value of the Matrix.
        /// </summary>
        /// <param name="row"></param>
        /// <param name="column"></param>
        double GetValue(int row, int column);

        void SetValue(int row, int column, double value);

    	/// <summary>
    	/// Matrix as a 2d array of doubles
    	/// </summary>
    	/// <returns>double[][] representation of matrix</returns>
    	double[,] AsArray();

        /// <summary>
        /// Clone (i.e. duplicate) a matrix.
        /// <p/>
        /// Note: Duplication means that a new vector is created that is identical to
        /// the current vector. All data in the current vector is also copied.
        /// </summary>
        /// <returns>A copy of the present vector.</returns>
        IMatrix Clone();
    }
}
