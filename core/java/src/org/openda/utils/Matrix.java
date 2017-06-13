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

package org.openda.utils;

import org.openda.interfaces.IMatrix;
import org.openda.interfaces.IVector;

import java.io.PrintStream;
import java.text.DecimalFormatSymbols;
import java.text.DecimalFormat;
//import Jama.*; // Do not import name clashes with org.openda.interfaces.Matrix

/**
 * Matrix implements OpenDA Matrix interface.
 * This class is a direct java implementation and is not optimized for speed.
 */
public class Matrix implements IMatrix {
    /* data for full matrix implementation */
   double values[][]=null;
   int m=0;
   int n=0;

    /**
     * Create a Matrix with specified number of rows and columns
     * @param rows number of rows
     * @param columns number of columns
     */
    public Matrix(int rows,int columns){
        this.values = new double[rows][columns];
        this.m      = rows;
        this.n      = columns; 
    }

    /**
     * Create a Matrix with specified number of rows and columns
     * @param rows number of rows
     * @param columns number of columns
     * @param value as double
     */
    public Matrix(int rows,int columns,double value){
        this.values = new double[rows][columns];
        for(int i=0;i<rows;i++){
        	for(int j=0;j<columns;j++){
        		this.values[i][j] = value;
        	}
        }
        this.m      = rows;
        this.n      = columns; 
    }

    /**
     * Create a Matrix from a double array
     * @param values as double-array double[][]
     */
    public Matrix(double[][] values){
        this.m      = values.length;
        if(this.m>0){
           this.n = values[0].length;
        }else{
        	this.n = 0;
        }
    	this.values = new double[this.m][this.n];
        for(int i=0;i<this.m;i++){
            System.arraycopy(values[i], 0, this.values[i], 0, this.n);
        }
    }

    /**
     * Create a Matrix from a resized Vector. Memory order is along rows value[row][col] 
     * @param vec as Vector
     * @param m number of rows
     * @param n number of columns
     */
    public Matrix(Vector vec,int m,int n){
        this.m=m;
        this.n=n;
        if(m*n!=vec.getSize()){
           throw new RuntimeException("Matrix: size of vector ("+vec.getSize()
        		   +") does not match requested matrix size ("+m+","+n+")");
        }
    	this.values = new double[this.m][this.n];
    	double[] vals=vec.getValues();
        for(int i=0;i<this.m;i++){
            System.arraycopy(vals, i*n, this.values[i], 0, this.n);
        }
    }

    /**
     * Create a DIAGONAL Matrix from a double array
     * @param values as double-array double[]
     */
    public Matrix(double[] values){
        this.m      = values.length;
        this.n      = values.length;
    	this.values = new double[this.m][this.n];
        for(int i=0;i<this.m;i++){
        	for(int j=0;j<this.n;j++){
        		if(i==j){
        			this.values[i][j] = values[i];
        		}else{
        			this.values[i][j] = 0.0;
        		}
        	}
        }
    }
    
    /**
     * Create a Matrix copy of a Matrix
     * @param M Source matrix
     */
    public Matrix(IMatrix M){
        this.m      = M.getNumberOfRows();
        this.n      = M.getNumberOfColumns();
    	this.values = new double[this.m][this.n];
        for(int i=0;i<this.m;i++){
        	for(int j=0;j<this.n;j++){
        		this.values[i][j] = M.getValue(i,j);
        	}
        }
    }

    /**
     * Create a Matrix copy of an array of Vectors
     * @param vectors Source vector[]
     */
    public Matrix(IVector[] vectors){
        this.m      = vectors[0].getSize();
        this.n      = vectors.length;
    	this.values = new double[this.m][this.n];
    	for(int j=0;j<this.n;j++){
    		for(int i=0;i<this.m;i++){
    			this.values[i][j] = vectors[j].getValue(i);
    		}
    	}
    }

    /**
     * Create a Matrix from a string representation, eg. "[1.0,2.0;3.0,4.0]"
     * @param matstring String with matrix values
     */
    public Matrix(String matstring){
       int ifirst = matstring.indexOf("[")+1;
       int ilast  = matstring.indexOf("]");
       String buffer = matstring.substring(ifirst,ilast);
       String[] rowstrings = buffer.split(";");
       String[] oneRowAsStrings = rowstrings[0].split(",");
       this.n = oneRowAsStrings.length;
       this.m = rowstrings.length;
       this.values = new double[this.m][this.n];
       for(int i=0;i<this.m;i++){
          oneRowAsStrings = rowstrings[i].split(",");
          for(int j=0;j<this.n;j++){
             this.values[i][j] = Double.parseDouble(oneRowAsStrings[j]);
          }
       }
    }

    /**
     * Create a outer product Matrix from two Vectors
     * @param v,w Source vectors
	 * @return The outer product matrix
     */
    public static Matrix outer(IVector v, IVector w){
    	int m = v.getSize();
    	int n = w.getSize(); 
    	double values[][] = new double[m][n];
    	for(int j=0;j<n;j++){
    		for(int i=0;i<m;i++){
	 			values[i][j] = v.getValue(i)*w.getValue(j);
 			}
 		}
		return new Matrix(values);
    }

    /**
     * Create a diagonal Matrix from a Vector
     * @param v Source vector
     * @return The diagonal matrix
     */
    public static Matrix diag(IVector v){
    	int nn = v.getSize();
        Matrix result = new Matrix(nn,nn);
        for(int i=0;i<nn;i++){
        		result.setValue(i, i, v.getValue(i));
        }
        return result;
    }

    /**
     * Extract the diagonal Matrix from a Vector
     * @return Vector
     */
    public IVector diag(){
    	int n = Math.min(this.m, this.n);
        IVector result = new Vector(n);
        for(int i=0;i<n;i++){
        		result.setValue(i,this.values[i][i]);
        }
        return result;
    }


    /**
     * Gets the number of rows of the Matrix.
     * @return number of rows of L
     */
    public int getNumberOfRows(){
       return this.m;
    }

    /**
     * Gets the number of columns of the Matrix.
     * @return number of columns of L
     */
    public int getNumberOfColumns(){
       return this.n;
    }

    /**
     * Perform matrix-vector multiplication x=alpha*x+beta*L*v, but then implicitly and often more efficiently
     * @param beta scaling parameter for matrix multiplication
     * @param v vector used for multiplication
     * @param alpha scaling parameter for vector x
     * @param x updated vector in multiplication. No update is performed for incorrect dimensions.
     */
    public  void rightMultiply(double beta, IVector v, double alpha, IVector x){
        //check dimensions
        if (x.getSize()==this.m && v.getSize()==this.n) {
            for(int i=0;i<this.m;i++){
                double temp = alpha * x.getValue(i);
                for(int j=0;j<this.n;j++){
                   temp+=beta*this.values[i][j]*v.getValue(j);
                }
                x.setValue(i,temp);
            }
        }
    }

    /**
     * Perform matrix-vector multiplication v=alpha*v+beta*(x'*L)', but then implicitly and often more efficiently
     * @param beta scaling parameter for matrix multiplication
     * @param x vector used for multiplication
     * @param alpha scaling parameter for vector v
     * @param v updated vector in multiplication. No update is performed for incorrect dimensions.
     */
    public  void leftMultiply(double beta, IVector x, double alpha, IVector v){
		// check dimensions
		if (v.getSize()==this.n && x.getSize()==this.m) {
			v.scale(alpha);
       		for(int j=0;j<this.n;j++){
        		double temp = v.getValue(j);
          		for(int i=0;i<this.m;i++){
            		temp+=beta*this.values[i][j]*x.getValue(i);
          		}
          		v.setValue(j,temp);
       		}
		}
    }

    /**
     * Perform matrix-vector solve x'*L=v', (ie x=inv(L')*v) but then implicitly and often more efficiently.
     * This is the (pseudo-)inverse of leftMultiply.
     * @param v vector used for linear solver
     * @param x result of linear solve
     */
    public  void leftSolve(IVector v, IVector x){
       if(this.m<this.n) throw(new java.lang.RuntimeException("JAMA bug!"));
       System.out.println("y (in)= "+v);
       //init
       double eps=1e-7;
       java.text.DecimalFormat fmt = new java.text.DecimalFormat("0.0000E00");
       fmt.setDecimalFormatSymbols(new java.text.DecimalFormatSymbols(java.util.Locale.US));
       //translate args 
       Jama.Matrix MatL = new Jama.Matrix(this.values);
       //System.out.print("L="); MatL.print(fmt,10);
       Jama.Matrix MatY = new Jama.Matrix(v.getValues(),1);
       //System.out.print("y="); MatY.print(fmt,10);
       //Solve x'*L=y with SVD-decomposition L=U*S*V'
       Jama.SingularValueDecomposition SvdA = MatL.svd(); // U*D*V' = A
       //System.out.print("U=");
       Jama.Matrix MatU = SvdA.getU();
       //MatU.print(fmt,10);
       //System.out.print("S=");
       Jama.Matrix MatS = SvdA.getS();
       // Compute inv(S)
       double svals[] = SvdA.getSingularValues();
       for(int i=0;i<svals.length;i++){ 
    	   if(Math.abs(svals[i])<eps){
    		   MatS.set(i,i,0.0);
    	   }else{
    		   MatS.set(i,i,1.0/svals[i]);
    	   }
       }
       //MatS.print(fmt,10);
       //System.out.print("V=");
       Jama.Matrix MatV = SvdA.getV();
       //MatV.print(fmt,10);
       // we now have x' = y*V*inv(S)*U';
       Jama.Matrix MatX = MatY.times(MatV).times(MatS).times(MatU.transpose());
       //System.out.print("x=");
       //MatX.print(fmt,10);
       //convert to vector
       x.setValues(MatX.getColumnPackedCopy());
       System.out.println("x(out)= "+x);
    }

    /**
     * Perform matrix-vector solve for x in v=L*x, (ie x=inv(L)*v) but then implicitly and often more
     * efficiently. This is the (pseudo-)inverse of rightMultiply.
     * @param v vector used for right solve
     * @param x result of linear solver
     */
    public  void rightSolve(IVector v, IVector x){
    	if(this.m<this.n) throw(new java.lang.RuntimeException("JAMA bug!"));
        //init
        double eps=1e-7;
        java.text.DecimalFormat fmt = new java.text.DecimalFormat("0.0000E00");
        fmt.setDecimalFormatSymbols(new java.text.DecimalFormatSymbols(java.util.Locale.US));
        //translate args 
        Jama.Matrix MatL = new Jama.Matrix(this.values);
        //System.out.print("L="); MatL.print(fmt,10);
        Jama.Matrix MatY = new Jama.Matrix(v.getValues(),1);
        //System.out.print("x="); MatX.print(fmt,10);
        //Solve L*y=x with SVD-decomposition L=U*S*V'
        Jama.SingularValueDecomposition SvdA = MatL.svd(); // U*D*V' = A
        //System.out.print("U=");
        Jama.Matrix MatU = SvdA.getU();
        //MatU.print(fmt,10);
        //System.out.print("S=");
        Jama.Matrix MatS = SvdA.getS();
        // Compute inv(S)
        double svals[] = SvdA.getSingularValues();
        for(int i=0;i<svals.length;i++){ 
     	   if(Math.abs(svals[i])<eps){
     		   MatS.set(i,i,0.0);
     	   }else{
     		   MatS.set(i,i,1.0/svals[i]);
     	   }
        }
        //MatS.print(fmt,10);
        //System.out.print("V=");
        Jama.Matrix MatV = SvdA.getV();
        //MatV.print(fmt,10);
        // we now have x' = y'*U*inv(S)*V';
        Jama.Matrix MatX = MatY.times(MatU).times(MatS).times(MatV.transpose());
        //System.out.print("y=");
        //MatY.print(fmt,10);
        //convert to vector
        x.setValues(MatX.getColumnPackedCopy());
        //
        // System.out.println("x= "+x);
    }
    /**
     * Perform matrix-vector solve for x in v=L*x, (ie x=inv(L)*v) but then implicitly and often more
     * efficiently. This is the (pseudo-)inverse of rightMultiply.
     * @param v vector used for right solve
     * @param x result of linear solver
     */
    public  void rightSolve(IMatrix v, IMatrix x){
    	if(this.m<this.n) throw(new java.lang.RuntimeException("JAMA bug!"));
        //init
        double eps=1e-7;
        java.text.DecimalFormat fmt = new java.text.DecimalFormat("0.0000E00");
        fmt.setDecimalFormatSymbols(new java.text.DecimalFormatSymbols(java.util.Locale.US));
        //translate args 
        Jama.Matrix MatL = new Jama.Matrix(this.values);
        //System.out.print("L="); MatL.print(fmt,10);
        Jama.Matrix MatY = (Jama.Matrix) v;
        //System.out.print("x="); MatX.print(fmt,10);
        //Solve L*y=x with SVD-decomposition L=U*S*V'
        Jama.SingularValueDecomposition SvdA = MatL.svd(); // U*D*V' = A
        //System.out.print("U=");
        Jama.Matrix MatU = SvdA.getU();
        //MatU.print(fmt,10);
        //System.out.print("S=");
        Jama.Matrix MatS = SvdA.getS();
        // Compute inv(S)
        double svals[] = SvdA.getSingularValues();
        for(int i=0;i<svals.length;i++){ 
     	   if(Math.abs(svals[i])<eps){
     		   MatS.set(i,i,0.0);
     	   }else{
     		   MatS.set(i,i,1.0/svals[i]);
     	   }
        }
        //MatS.print(fmt,10);
        //System.out.print("V=");
        Jama.Matrix MatV = SvdA.getV();
        //MatV.print(fmt,10);
        // we now have x' = y'*U*inv(S)*V';
        Jama.Matrix MatX = MatY.times(MatU).times(MatS).times(MatV.transpose());
        //System.out.print("y=");
        //MatY.print(fmt,10);
        //convert to vector
        x = (IMatrix) MatX;
        //
        // System.out.println("x= "+x);
    }

    /**
     * Return part of the Matrix as Matrix
     * @param rowmin start row of selection
     * @param rowmax end row of selection
     * @param colmin start column of selection
     * @param colmax end column of selection
     * @return matrix representation of L
     */
    public IMatrix getMatrixSelection(int rowmin, int rowmax, int colmin, int colmax){
       IMatrix temp = new Matrix(rowmax-rowmin+1,colmax-colmin+1);
       for(int i=rowmin;i<=rowmax;i++){
          for(int j=colmin;j<=colmax;j++){
             temp.setValue(i-rowmin,j-colmin,this.values[i][j]);
		  }
	   }
       return temp;
    }

    /**
     * Return Matrix as an array of Vectors, where each vector represents one column of L
     * @return Vector[] representation of L
     */
    public  IVector[] asVectorArray(){
    	IVector[] result = new Vector[this.n];
    	for(int j=0;j<this.n;j++){
    		result[j] = new Vector(this.m);
    		for(int i=0;i<this.m;i++){
    			result[j].setValue(i,this.values[i][j]);
    		}
    	}
    	return result;
    }

    /**
     * Return Matrix as a Vector, with the rows of the matrix concatenated
     * @return Vector representation of L
     */
    public  Vector asVector(){
    	double vals[]=new double[this.m*this.n];
        for(int i=0;i<this.m;i++){
            System.arraycopy(this.values[i], 0, vals, i*this.n, this.n);
        }
    	return new Vector(vals);
    }

    /**
     * Get single value of the Matrix.
     *
     * @param row         index in value in matrix; [0..(m-1)]
     * @param column      index in value in matrix; [0..(n-1)]
     * @return return the value at some position
     */
    public double getValue(int row,int column){
       return this.values[row][column];
    }

    /**
     * Set single value of the Matrix.
     *
     * @param row         index in value in matrix
     * @param column      index in value in matrix
     * @param value       value in vector
     */
    public void setValue(int row,int column,double value){
       this.values[row][column]=value;
    }

    /**
     * Return Matrix as a two dimensional array
     * @return array[][] 2d array of doubles
     */
    public  double[][] asArray(){
       double temp[][] = new double[this.m][this.n];
       for(int i=0;i<this.m;i++){
           System.arraycopy(this.values[i], 0, temp[i], 0, this.n);
       }
       return temp;
    }
    
    /**
     * Axpy operation on matrices y=alpa*x+y
     * @param alpha : factor applied to X before X is added
     * @param X : another Matrix
     */
    public void axpy(double alpha, IMatrix X){
    	for(int i=0;i<this.m;i++){
    		for(int j=0;j<this.n;j++){
    			this.values[i][j]+=alpha*X.getValue(i,j);
    		}
    	}
    }

    /**
     * Return string representation of a matrix, eg. "[1.0,2.0;3.0,4.0]"
     * @return string representation of matrix
     */


    public String toString() {
        String temp = "[";
        for (int i = 0; i < this.m; i++) {
            if (i > 0) temp += ";";
            for (int j = 0; j < this.n; j++) {
                if (j > 0) temp += ",";
                temp += this.values[i][j];
            }
        }
        temp += "]";
        return temp;
    }

    public String printString() {
        String temp = "[";
        
        for (int i = 0; i <  this.m; i++) {
            if (i > 0) temp += ";";
            for (int j = 0; j < this.n; j++) {
                if (j > 0) temp += ",";
                temp += printNumber(this.values[i][j]);
            }
        }
        temp += "]";
        return temp;
    }
    public String printfreeString() {
         String temp = "";
         for (int i = 0; i < this.m; i++) {
             if (i > 0) temp += " ; ";
             for (int j = 0; j < this.n; j++) {
                 if (j > 0) temp += ",";
                 temp += printNumber(this.values[i][j]);
             }
         }
         temp += "";
         return temp;
     }

    /**
     * Compute square root of a symmetric matrix P=L*L' (also P=L*L)
     * @return sqrtMatrix
     */
    public Matrix sqrt(){
    	if(this.m!=this.n) throw(new java.lang.RuntimeException("Matrix should be square"));
    	//init
    	double eps=1e-7;
    	//translate args 
    	Jama.Matrix MatP = new Jama.Matrix(this.values);
    	//Compute SVD-decomposition L=U*S*V'
    	Jama.SingularValueDecomposition SvdA = MatP.svd(); // U*D*V' = A
    	Jama.Matrix MatU = SvdA.getU();
    	Jama.Matrix MatS = SvdA.getS();
    	double svals[] = SvdA.getSingularValues();
    	for(int i=0;i<svals.length;i++){ 
    		if(Math.abs(svals[i])<-eps){
    			MatS.set(i,i,-Math.sqrt(-svals[i]));
    		}else if(Math.abs(svals[i])<eps){
    			MatS.set(i,i,0.0);
    		}else{
    			MatS.set(i,i,Math.sqrt(svals[i]));
    		}
    	}
        Jama.Matrix MatV = SvdA.getV();
        // we now have L = U*sqrt(S)*V';
        Jama.Matrix MatY = MatU.times(MatS).times(MatV.transpose());
    	return new Matrix(MatY.getArrayCopy());
    }

    /**
     * Clone a matrix
     * @return copy of the matrix
     */
    
    @SuppressWarnings({"CloneDoesntCallSuperClone", "CloneDoesntDeclareCloneNotSupportedException"})
    public Matrix clone(){
        return new Matrix(this.values);
     }

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
   public void multiply(double facAB, IMatrix A, IMatrix B, double facThis,boolean transa, boolean transb){
	   int A_m = A.getNumberOfRows();
	   int A_n = A.getNumberOfColumns();
	   int B_m = B.getNumberOfRows();
	   int B_n = B.getNumberOfColumns();
	   // 4 cases
	   if(!transa & !transb){
		   if (A_m!=this.m){ throw new RuntimeException("rows(A) does not match rows(this)");}
		   if (A_n!=B_m){ throw new RuntimeException("cols(A) does not match rows(B)");}
		   if (B_n!=this.n){ throw new RuntimeException("colss(B) does not match colss(this)");}
		   for(int i=0;i<A_m;i++) {
			   for (int j = 0; j < B_n; j++) {
				   this.values[i][j] *= facThis;
				   double sum = 0.0;
				   for (int k = 0; k < A_n; k++) {
					   sum += A.getValue(i, k) * B.getValue(k, j);
				   }
				   this.values[i][j] += facAB * sum;
			   }
		   }
	   }else if(transa & !transb){
		   if (A_n!=this.m){ throw new RuntimeException("cols(A) does not match rows(this)");}
		   if (A_m!=B_m){ throw new RuntimeException("rows(A) does not match rows(B)");}
		   if (B_n!=this.n){ throw new RuntimeException("colss(B) does not match colss(this)");}
		   for(int i=0;i<A_n;i++){
			   for(int j=0;j<B_n;j++){
				   this.values[i][j] *= facThis;
				   double sum=0.0;
				   for(int k=0;k<A_m;k++){
					   sum += A.getValue(k, i)*B.getValue(k, j);
				   }
				   this.values[i][j] += facAB * sum;
			   }
		   }

	   }else if(!transa){ // (! transa & transb)
		   if (A_m!=this.m){ throw new RuntimeException("rows(A) does not match rows(this)");}
		   if (A_n!=B_n){ throw new RuntimeException("cols(A) does not match cols(B)");}
		   if (B_m!=this.n){ throw new RuntimeException("rows(B) does not match cols(this)");}
		   for(int i=0;i<A_m;i++){
			   for(int j=0;j<B_m;j++){
				   this.values[i][j] *= facThis;
				   double sum=0.0;
				   for(int k=0;k<A_n;k++){
					   sum += A.getValue(i, k)*B.getValue(j,k);
				   }
				   this.values[i][j] += facAB * sum;
			   }
		   }
	   }else{ // (transa & transb)
		   if (A_n!=this.m){ throw new RuntimeException("cols(A) does not match rows(this)");}
		   if (A_m!=B_n){ throw new RuntimeException("rows(A) does not match cols(B)");}
		   if (B_m!=this.n){ throw new RuntimeException("rows(B) does not match cols(this)");}
		   for(int i=0;i<A_n;i++){
			   for(int j=0;j<B_m;j++){
				   this.values[i][j] *= facThis;
				   double sum=0.0;
				   for(int k=0;k<A_m;k++){
					   sum += A.getValue(k,i)*B.getValue(j,k);
				   }
				   this.values[i][j] += facAB * sum;
			   }
		   }		   
	   }
   }

   /**
    * Transposematrix, i.e. if B=transpose(A) then A(i,j)=B(j,i)
    * @return Matrix
    */
   public IMatrix transpose(){
	   IMatrix result = new Matrix(this.n,this.m);
	   for(int i=0;i<this.m;i++){
		   for(int j=0;j<this.n;j++){
			   result.setValue(j,i,this.values[i][j]);
		   }
	   }
	   return result;
   }

   
   /**
    * TransposeSimplematrix, i.e. if B=transpose(A) then A(i,j)=B(j,i)
    * @return SimpleMatrix
    */
   public Matrix simpletranspose(){
	   Matrix result = new Matrix(this.n,this.m);
	   for(int i=0;i<this.m;i++){
		   for(int j=0;j<this.n;j++){
			   result.setValue(j,i,this.values[i][j]);
		   }
	   }
	   return result;
   }

 
   
   
   /**
    * Simplified version of multiply C=A*B
    * @param A left Matrix
    * @param B right Matrix
    * @return A*B Matrix
    */
   public static Matrix mult(IMatrix A, IMatrix B){
	   return Matrix.mult(A, B, false, false);
   }

   /**
    * Simplified version of multiply C=A*B
    * @param A left Matrix
    * @param B right Matrix
    * @param transposeA should A be transposed before multiplication
    * @param transposeB should B be transposed before multiplication
    * @return A*B Matrix (or A'*B or A*B' or A'*B' depending on transpose settings)
    */
   public static Matrix mult(IMatrix A, IMatrix B, boolean transposeA, boolean transposeB ){
	   int AbRows = A.getNumberOfRows();
	   if(transposeA){
		   AbRows = A.getNumberOfColumns();
	   }
	   int AbCols = B.getNumberOfColumns();
	   if(transposeB){
		   AbCols = B.getNumberOfRows();
	   }
	   Matrix result = new Matrix(AbRows,AbCols);
	   result.multiply(1.0, A, B, 1.0, transposeA, transposeB);
	   return result;
   }

   
   /**
    * Simplified version of multiply C=A*B A=this
    * @param B right Matrix
    * @return A*B Matrix
    */
   public Matrix mult(IMatrix B){
	   Matrix result = new Matrix(this.m,B.getNumberOfColumns());
	   result.multiply(1.0, this, B, 1.0, false, false);
	   return result;
   }

   
   /**
    * Scale a Matrix C:=factor*C
    * @param factor for scaling (double) 
    */
   public void scale(double factor){
	   for(int i=0;i<this.m;i++){
		   for(int j=0;j<this.n;j++){
			   this.values[i][j] *= factor;
		   }
	   }	   
   }
   
   /**
    * Compute 2-norm for matrix
    * @return norm
    */
   public double norm(){
	   Jama.Matrix MatP = new Jama.Matrix(this.values);
	   //Compute SVD-decomposition L=U*S*V'
	   Jama.SingularValueDecomposition SvdA = MatP.svd(); // U*D*V' = A
	   double svals[] = SvdA.getSingularValues();
	   return svals[0];
   }

	/**
	 * Create nxn submatrix
     * @param n The size of the submatrix
     * @return The nxn submatrix
     */
   public static Matrix eye(int n){
       Matrix result = new Matrix(n,n);
       for(int i=0;i<n;i++){
    	   result.setValue(i,i,1.0);
       }
       return result;
   }
   /**
   * Compute the inverse of a matrix if invA = A.inverse(), then A*invA = eye(n) 
   * @return Matrix inverse
   */
   public Matrix inverse(){
       	double eps=1e-7;
    	//translate args 
    	Jama.Matrix MatA = new Jama.Matrix(this.values);
    	//Compute SVD-decomposition L=U*S*V'
    	Jama.SingularValueDecomposition SvdA = MatA.svd(); // U*D*V' = A
    	Jama.Matrix MatU = SvdA.getU();
    	Jama.Matrix MatD = SvdA.getS();
    	Jama.Matrix MatV = SvdA.getV();
    	double dvals[] = SvdA.getSingularValues();
    	for(int i=0;i<dvals.length;i++){ 
    		if(Math.abs(dvals[i])>eps){
    			MatD.set(i,i,1.0/dvals[i]);
    		}else{
    			MatD.set(i,i,0.0);
    		}
    	}
        // we now have invA = V*inv(D)*U';
        Jama.Matrix invA = MatV.times(MatD).times(MatU.transpose());
    	return new Matrix(invA.getArrayCopy());
  	}
   
   
   /**
    * Compute singular value decomposition of a matrix, i.e three 
    * matrices (U,D,V) with A=U*D*V' with U and V unitary and D is diagonal
    * @return svd matrix triplet
    */
   public Matrix[] svd(){
  		Matrix[] result = new Matrix[3];
     	//translate args 
    	Jama.Matrix MatA = new Jama.Matrix(this.values);
    	boolean isWide = MatA.getColumnDimension()>MatA.getRowDimension();
    	if(isWide){
    		MatA = MatA.transpose();
    	}
    	//Compute SVD-decomposition L=U*S*V'
    	Jama.SingularValueDecomposition SvdA = MatA.svd(); // U*D*V' = A
    	Jama.Matrix MatU = SvdA.getU();
    	Jama.Matrix MatD = SvdA.getS();
    	Jama.Matrix MatV = SvdA.getV();
    	if(!isWide){
    		result[0] = new Matrix(MatU.getArrayCopy());
    		result[1] = new Matrix(MatD.getArrayCopy());
    		result[2] = new Matrix(MatV.getArrayCopy());
    	}else{
    		result[0] = new Matrix(MatV.getArrayCopy());
    		result[1] = new Matrix(MatD.getArrayCopy());
    		result[2] = new Matrix(MatU.getArrayCopy());
    	}
    	return result;
   }

    public void serialize(PrintStream outputStream) {
        outputStream.print("[");
        for(int i=0;i< Math.min(this.m,40);i++){
           if(i>0) outputStream.print(";");
           for(int j=0;j<Math.min(this.n,40);j++){
              if(j>0) {
                  outputStream.print(",");
              }
              outputStream.print(this.values[i][j]);
           }
        }
        outputStream.print("]");
    }

	/**
	 * Return vertical concatenation of two matrices, mat1 an n x m matrix, mat2 a k x l matrix, as one (n+k) x MAX(m,l) Matrix
	 * @param mat1 upper Matrix
	 * @param mat2 lower Matrix
	 * @return resulting Matrix
	 */
	public static Matrix concatVertical(IMatrix mat1, IMatrix mat2){
    	int mat1M = mat1.getNumberOfRows();
    	int mat1N = mat1.getNumberOfColumns();
    	int mat2M = mat2.getNumberOfRows();
    	int mat2N = mat2.getNumberOfColumns();
    	int matN  = Math.max(mat1N,mat2N);
    	double[][] array1 = mat1.asArray();
    	double[][] array2 = mat2.asArray();
    	// create array of appropriate size
    	double[][] vertCat = new double[mat1M+mat2M][matN];
    	// copy first matrix
        for(int i=0;i<mat1M;i++){
            System.arraycopy(array1[i], 0, vertCat[i], 0, mat1N);
        }
    	// copy second matrix
        for(int i=0;i<mat2M;i++){
            System.arraycopy(array2[i], 0, vertCat[i+mat1M], 0, mat2N);
        }
    	return new Matrix(vertCat);
    }
    
    /**
     * Return horizontal combination of two matrices, mat1 an n x m matrix, mat2 a k x l matrix, as one MAX(n,k) x m+l Matrix
	 * @param mat1 upper Matrix
	 * @param mat2 lower Matrix
	 * @return resulting Matrix
     */
    public static Matrix concatHorizontal(Matrix mat1, Matrix mat2){
    	Matrix mat1_transpose = mat1.simpletranspose();
    	Matrix mat2_transpose = mat2.simpletranspose();
    	Matrix output = concatVertical(mat1_transpose, mat2_transpose);
    	return output.simpletranspose();
    }

    // Note: copied from Vector - should be refactored (AM)
    private String printNumber(double value) {
        DecimalFormatSymbols symbols = new DecimalFormatSymbols();
        symbols.setDecimalSeparator('.');
        DecimalFormat formatFloat = new DecimalFormat("0.###", symbols);
        DecimalFormat formatExponent = new DecimalFormat("0.###E0", symbols);

        if(Double.isNaN(value)){
        	return "NaN";
        } else if (Math.abs(value) > 0.01 && Math.abs(value) < 1000.0 || value == 0.0) {
            return formatFloat.format(value);
        } else {
            return formatExponent.format(value);
        }
    }

    /**
     * Compute determinant of a square matrix 
     * with the use of a Singular Value Decomposition.
     * The value is always positive, even if the product of the eigenvalues is negative.
     * So, perhaps absDeterminant would be a more appropriate name.
     * @return determinant
     */
    public double determinant(){
    	double result=1.0;
    	if(this.m!=this.n) throw(new java.lang.RuntimeException("Matrix should be square"));
    	Jama.Matrix MatP = new Jama.Matrix(this.values);
    	//Compute SVD-decomposition L=U*S*V'
    	Jama.SingularValueDecomposition SvdA = MatP.svd(); // U*D*V' = A
    	double svals[] = SvdA.getSingularValues();
		for (double sval : svals) {
			result *= sval;
		}
    	return result;
    }

	/**
	 * Removes one row from a Matrix
	 * @param rowindex  index of row to be removed
	 * @return resulting Matrix; the original Matrix is returned in case of an incorrect rowindex.
	 */
    public Matrix remove_row(int rowindex){
    	
    	int rowmax = this.getNumberOfRows();
    	int colmax = this.getNumberOfColumns();

		if (rowindex < 0 || rowindex > rowmax) return this;
        if (rowindex == rowmax) return new Matrix(this.getMatrixSelection(0, rowmax-2, 0, colmax-1));

		Matrix temp1 = new Matrix(this.getMatrixSelection(0, rowindex-1, 0, colmax-1));
		Matrix temp2 = new Matrix(this.getMatrixSelection(rowindex+1,rowmax-1 , 0, colmax-1));

    	return concatVertical(temp1,temp2);
    } 
}
