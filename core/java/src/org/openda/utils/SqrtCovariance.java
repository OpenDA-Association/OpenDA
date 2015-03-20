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
package org.openda.utils;

import org.openda.interfaces.*;


/**
 * Abstract representation of a square-root covariance matrix. If P is the covariance of a
 * StochVector then any "matrix" L satisfying P=L*transpose(L) is called a
 * Square-root-covariance. A SqrtCovariance can in theory always be represented by a matrix
 * but if the dimensions are large then it may be much more efficient to work with the
 * operators provided here instead.
 */
public class SqrtCovariance implements ISqrtCovariance {
    boolean isDiagonal = true;
    IVector std        = null; //used if isDiagonal=true
    IMatrix L          = null; //used if isDiagonal=false

    /**
     * Create a SqrtCovariance from a vector with standard-deviations
     * @param std standard-deviation as Vector
     */
    public SqrtCovariance(IVector std){
       this.std        = new Vector(std);
       this.L          = null; //not used
       this.isDiagonal = true;
    }

	/**
	 * Create a SqrtCovariance from an array of standard-deviations
	 *
	 * @param std standard-deviation as Vector
	 */
	public SqrtCovariance(double[] std) {
		this.std = new Vector(std);
		this.L = null; //not used
		this.isDiagonal = true;
	}

	/**
     * Create a SqrtCovariance from a matrix with square-root of the covariance
     * @param L standard-deviation as Vector
     */
    public SqrtCovariance(IMatrix L){
    	this.isDiagonal = false;
    	this.std        = null; //not used
    	this.L          = new Matrix(L.asArray());
    }

    /**
     * Gets the number of rows of the SqrtCovariance, which equals the number of elements of
     * any realization from the underlying stochTreeVector
     * @return number of rows of L
     */
    public int getNumberOfRows(){
       int temp=0;
       if(this.isDiagonal){
          temp = this.std.getSize();
       }else{
          temp = this.L.getNumberOfRows();
       }
      return temp;
    }

    /**
     * Gets the number of columns of the SqrtCovariance, which equals the rank of the matrix
     * (if less than length). If this number is much smaller than L.getLength()
     * than a square-root approach reduces storage and computations.
     * @return number of columns of L
     */
    public int getNumberOfColumns(){
       int temp=0;
       if(this.isDiagonal){
          temp = this.std.getSize();
       }else{
          temp = this.L.getNumberOfColumns();
       }
      return temp;
    }

    /**
     * Perform matrix-vector multiplication x=alpha*x+beta*L*v, but then implicitly and often more efficiently
     * @param beta scaling parameter for matrix multiplication
     * @param v vector used for multiplication (in most cases a "small" vector number of columns of L)
     * @param alpha scaling parameter for vector x
     * @param x updated vector in multiplication (in most cases a "large" vector of size of model state)
     */
    public  void rightMultiply(double beta, IVector v, double alpha, IVector x){
        if(this.isDiagonal){
            for(int i=0;i<this.std.getSize();i++){
            	x.setValue(i, alpha*x.getValue(i)+beta*this.std.getValue(i)*v.getValue(i));
            }
         }else{
            this.L.rightMultiply(beta, v, alpha, x);            
         }
    }

    /**
     * Perform matrix-vector multiplication v=alpha*v+beta*(x'*L)', but then implicitly and often more efficiently
     * @param beta scaling parameter for matrix multiplication
     * @param x vector used for multiplication (in most cases a "large" vector of size of model state)
     * @param alpha scaling parameter for vector v
     * @param v updated vector in multiplication (in most cases a "small" vector number of columns of L)
     */
    public  void leftMultiply(double beta, IVector x, double alpha, IVector v){
        if(this.isDiagonal){
            for(int i=0;i<this.std.getSize();i++){
            	v.setValue(i, alpha*v.getValue(i)+beta*this.std.getValue(i)*x.getValue(i));
            }
         }else{
            this.L.leftMultiply(beta, x, alpha, v);
         }
    }

    /**
     * Perform matrix-vector solve x=inv(L')*v, but then implicitly and often more efficiently.
     * This is the (pseudo-)inverse of leftMultiply.
     * @param v vector used for linear solver
     * @param x result of linear solve
     */
    public  void leftSolve(IVector v, IVector x){
        if(this.isDiagonal){
            for(int i=0;i<this.std.getSize();i++){
            	x.setValue(i, v.getValue(i)/this.std.getValue(i));
            }
         }else{
            this.L.leftSolve(v,x);
         }    	
    }

    /**
     * Perform matrix-vector solve for x in v=L*x, (ie x=inv(L)*v ) but then implicitly and often more
     * efficiently. This is the (pseudo-)inverse of rightMultiply.
     * @param x vector used for right solve
     * @param v result of linear solver
     */
    public  void rightSolve(IVector v, IVector x){
        if(this.isDiagonal){
            for(int i=0;i<this.std.getSize();i++){
            	x.setValue(i, v.getValue(i)/this.std.getValue(i));
            }
         }else{
            this.L.rightSolve(v,x);
         }    	
    }

    /**
     * Return SqrtCovariance as Matrix
     * @return matrix representation of L
     */
    public IMatrix asMatrix(){
       IMatrix mat = null;
       if(this.isDiagonal){
           mat = Matrix.diag(this.std);
        }else{
           mat = new Matrix(this.L);
        }    	
       return mat;
    }

    /**
     * Return part of the SqrtCovariance as Matrix
     * @param rowmin start row of selection
     * @param rowmax end row of selection
     * @param colmin start column of selection
     * @param colmax end column of selection
     * @return matrix representation of L
     */
    public IMatrix getMatrixSelection(int rowmin, int rowmax, int colmin, int colmax){
        IMatrix mat = null;
        if(this.isDiagonal){
        	IMatrix temp = Matrix.diag(this.std); //first make full
            mat = temp.getMatrixSelection(rowmin, rowmax, colmin, colmax); //TODO make more efficient
         }else{
            mat = this.L.getMatrixSelection(rowmin, rowmax, colmin, colmax);
         }    	
        return mat;
    }

    /**
     * Return SqrtCovariance as an array of Vectors, where each tv represents one column of L
     * @return tv[] representation of L
     */
    public  IVector[] asVectorArray(){
    	IVector v[] = null;
    	if (this.isDiagonal){
    		IVector std = this.std;
    		int n = std.getSize();
    		v = new Vector[n];
    		for(int i=0;i<n;i++){
    			v[i] = new Vector(n);
                v[i].setValue(i, std.getValue(i));
    		}
    	}else{
    		v = this.L.asVectorArray();
    	}
    	return v;
    }

    /**
     * Write SqrtCovariance to string
     * <p/>
     */
    public String toString(){
       String temp = "";
       if(this.isDiagonal){
          temp = "diag("+this.std+")";
       }else{
          temp = "full("+this.L.toString()+")";
       }
       return temp;
    }

}
