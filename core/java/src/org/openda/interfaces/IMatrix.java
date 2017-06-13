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

package org.openda.interfaces;

/**
 * General interface to a matrix.
 */
public interface IMatrix extends Cloneable{
    /**
     * Gets the number of rows of the Matrix.
     * @return number of rows of matrix
     */
    public int getNumberOfRows();

    /**
     * Gets the number of columns of the Matrix.
     * @return number of columns of L
     */
    public int getNumberOfColumns();

    /**
     * Perform matrix-vector multiplication x=alpha*x+beta*L*v, but then implicitly and often more efficiently
     * @param beta scaling parameter for matrix multiplication
     * @param v vector used for multiplication (in most cases a "small" vector number of columns of L)
     * @param alpha scaling parameter for vector x
     * @param x updated vector in multiplication (in most cases a "large" vector of size of model state)
     */
    public  void rightMultiply(double beta, IVector v, double alpha, IVector x);

    /**
     * Perform matrix-vector multiplication v=alpha*v+beta*(x'*L)', but then implicitly and often more efficiently
     * @param beta scaling parameter for matrix multiplication
     * @param x vector used for multiplication (in most cases a "large" vector of size of model state)
     * @param alpha scaling parameter for vector v
     * @param v updated vector in multiplication (in most cases a "small" vector number of columns of L)
     */
    public  void leftMultiply(double beta, IVector x, double alpha, IVector v);

    /** 
    * General Matrix-matrix multiplication as BLAS-GEMM: C=(alpha)AB+(beta)C
    * i.e. for matrices A,B (both optionally transposed) and C=this, scalars alpha and beta
    * @param facAB : factor for A*B
    * @param A : left Matrix
    * @param B : right Matrix
    * @param facThis : factor for this (C)
    * @param transa : transpose A before multiplication
    * @param transb : transpose B before multiplication
    */
   public void multiply(double facAB, IMatrix A, IMatrix B, double facThis,boolean transa, boolean transb);
    
    /**
     * Perform matrix-vector solve x=inv(L')*v, but then implicitly and often more efficiently.
     * This is the (pseudo-)inverse of leftMultiply.
     * @param v vector used for linear solver
     * @param x result of linear solve
     */
    public  void leftSolve(IVector v, IVector x);

    /**
     * Perform matrix-vector solve for v in v=L*x, (ie x = inv(L)*v) but then implicitly and often more
     * efficiently. This is the (pseudo-)inverse of rightMultiply.
     * @param x vector used for right solve
     * @param v result of linear solver
     */
    public  void rightSolve(IVector v, IVector x);

    /**
     * Return part of the Matrix as Matrix
     * @param rowmin start row of selection
     * @param rowmax end row of selection
     * @param colmin start column of selection
     * @param colmax end column of selection
     * @return selected part of the matrix
     */
    public IMatrix getMatrixSelection(int rowmin, int rowmax, int colmin, int colmax);

    /**
     * Return Matrix as an array of Vectors, where each vector represents one column of the matrix
     * @return v[] vector representation of L
     */
    public  IVector[] asVectorArray();

    /**
     * Get single value of the Matrix.
     *
     * @param row         index in value in matrix
     * @param column      index in value in matrix
     * @return return the value at some position
     */
    double getValue(int row,int column);
 
    /**
     * Set single value of the Matrix.
     *
     * @param row         index in value in matrix
     * @param column      index in value in matrix
     * @param value       value in matrix
     */
    void setValue(int row,int column,double value);
 
    /**
     * Return Matrix as a 2d array of doubles
     * @return double[][] representation of matrix
     */
    public  double[][] asArray();

    /**
     * Clone (i.e. duplicate) a matrix.
     * <p/>
     * Note: Duplication means that a new matrix is created that is identical to
     * the current matrix. All data in the current matrix is also copied.
     *
     * @return A copy of the present matrix.
     */
    IMatrix clone();
}
