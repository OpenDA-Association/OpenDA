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

/**
 * Main class for testing Matrix class
 */

package org.openda.utils;

import org.openda.interfaces.*;

import junit.framework.TestCase;

public class MatrixTest extends TestCase{

   public static void testMatrix_1() {
	   IMatrix Mat1= new Matrix(3,2); //initializes with 0
	   assertEquals("Mat1.toString()","[0.0,0.0;0.0,0.0;0.0,0.0]",Mat1.toString());
	   //    public int Matrix(String matstring)
	   IMatrix Mat2= new Matrix("[1.0,2.0;3.0,4.0]");
	   assertEquals("Mat2.toString()","[1.0,2.0;3.0,4.0]",Mat2.toString());
	   //    public int getNumberOfRows()
	   assertEquals("Mat1.getNumberOfRows()",3,Mat1.getNumberOfRows());
	   //    public int getNumberOfColumns()
	   assertEquals("Mat1.getNumberOfColumns()",2,Mat1.getNumberOfColumns());
	   //    public  Matrix getMatrixSelection(int rowmin, int rowmax, int colmin, int colmax)
	   IMatrix Mat3= new Matrix("[1.1,1.2,1.3,1.4;2.1,2.2,2.3,2.4;3.1,3.2,3.3,3.4]");
	   IMatrix Mat4= Mat3.getMatrixSelection(1,2,1,2);
	   assertEquals("Mat3(1:2,1:2)","[2.2,2.3;3.2,3.3]",Mat4.toString());
	   //    double getValue(int row,int column)
	   double val = Mat3.getValue(1,1);
	   assertEquals("Mat3.getValue(1,1)",2.2,val);
	   //    void setValue(int row,int column,double value)
	   Mat3.setValue(1,1,999.99);
	   val = Mat3.getValue(1,1);
	   assertEquals("Mat3.setValue(1,1,999.99)",999.99,val);
	   //    public  double[][] asArray()
	   double arr[][] = Mat3.asArray();
	   assertEquals("Mat3.asArray()",999.99,arr[1][1]);
	   Mat3 = new Matrix("[1.1,1.2,1.3,1.4;2.1,2.2,2.3,2.4;3.1,3.2,3.3,3.4]");
       IVector Varr[] = Mat3.asVectorArray();
	   assertEquals("Mat3.asVectorArray()[:,0]","[1.1,2.1,3.1]",Varr[0].toString());

   }

   public static void testMatrix_2() {
	   //    public  void rightMultiply(double beta, Vector v, double alpha, Vector x)
	   IMatrix Mat5 = new Matrix("[1.0,1.0,1.0;1.0,1.0,1.0]");
	   IVector v    = new Vector("[1.0,2.0,3.0]");
	   IVector x    = new Vector("[0.0,0.0]");
	   Mat5.rightMultiply(1.0/3.0,v,0.0,x); // x=Mat4*v * (1/3) is average for each element
	   assertEquals("Mat5.rightMultiply(1.0/3.0,v,0.0,x)","[2.0,2.0]",x.toString());
	   //    public  void leftMultiply(double beta, Vector x, double alpha, Vector v)
	   //Matrix Mat5 = new Matrix("[1.0,1.0,1.0;1.0,1.0,1.0]");
	   x    = new Vector("[1.0,3.0]");
	   v    = new Vector("[0.0,0.0,0.0]");
	   Mat5.leftMultiply(1.0/2.0,x,0.0,v); // v=Mat4'*v' * (1/2) is average for each element
	   assertEquals("Mat5.leftMultiply(1.0/2.0,x,0.0,v)","[2.0,2.0,2.0]",v.toString());
	   //    public  void leftSolve(Vector v, Vector x)
	   IMatrix Mat6 = new Matrix("[10.0,10.0;1.0,-1.0]");
	   IVector y6    = new Vector("[1.0,2.0]");
	   IVector x6    = new Vector("[0.0,0.0]");
	   Mat6.leftSolve(y6, x6); // x6'*Mat6=y6
	   assertEquals("Mat6.leftSolve(y6, x6)","[0.15000000000000005,-0.5]",x6.toString());
	   //    public  void rightSolve(Vector x, Vector v)
	   IMatrix Mat7 = new Matrix("[10.0,1.0;10.0,-1.0]");
	   IVector x7    = new Vector("[1.0,2.0]");
	   IVector y7    = new Vector("[0.0,0.0]");
	   Mat7.rightSolve(x7, y7); // Mat7*y7=x7
	   assertEquals("Mat7.rightSolve(x7, y7)","[0.15,-0.5000000000000001]",y7.toString());
	   //    public  void rightSolve(Vector x, Vector v)
	   // Test with non-square matrix
	   IMatrix Mat8 = new Matrix("[10.0,1.0;10.0,-1.0;0.0,0.0]");
	   IVector x8    = new Vector("[1.0,2.0,0.0]");
	   IVector y8    = new Vector("[0.0,0.0]");
	   Mat8.rightSolve(x8, y8); // Mat8*y8=x8
	   assertEquals("Mat8.rightSolve(x8, y8)","[0.15,-0.5000000000000001]",y8.toString());
   }

   public static void testSimpleMatrix_1() {
	   Matrix Mat5= new Matrix("[1.1,1.2,1.3,1.4;2.1,2.2,2.3,2.4;3.1,3.2,3.3,3.4]");
	   Mat5.axpy(-1.0, Mat5); //subtract from itself returns zeroes
	   assertEquals("Mat5.axpy(-1.0, Mat5)","[0.0,0.0,0.0,0.0;0.0,0.0,0.0,0.0;0.0,0.0,0.0,0.0]",Mat5.toString());
	   Matrix Mat6= new Matrix(2,3,1.0); //matrix with constant value
	   assertEquals("Mat6","[1.0,1.0,1.0;1.0,1.0,1.0]",Mat6.toString());
	   double[][] arr7 = {{1.1,1.2},{2.1,2.2}};
	   Matrix Mat7 = new Matrix(arr7);
	   assertEquals("Mat7","[1.1,1.2;2.1,2.2]",Mat7.toString());
	   Matrix Mat8 = new Matrix("[4.0,0.0;0.0,4.0]");
	   Matrix L8 = Mat8.sqrt();
	   assertEquals("Mat8.sqrt()","[2.0,0.0;0.0,2.0]",L8.toString());
	   IVector v9 = new Vector("[1.0,2.0]");
	   Matrix Mat9 = Matrix.diag(v9);
	   assertEquals("diag(v9)","[1.0,0.0;0.0,2.0]",Mat9.toString());
	   IVector d9 = Mat9.diag();
	   assertEquals("d9","[1.0,2.0]",d9.toString());
	   Matrix Mat10 = new Matrix("[4.0,0.0;0.0,4.0]");
	   Matrix Mat11 = Mat10.clone();
	   assertEquals("Mat10.clone()","[4.0,0.0;0.0,4.0]",Mat11.toString());
	   Matrix Mat12 = new Matrix(Mat10);
	   assertEquals("new Matrix(Mat10)","[4.0,0.0;0.0,4.0]",Mat12.toString());
	   //public void multiply(double facAB,Matrix A, Matrix B, double facThis,boolean transa, boolean transb){
       IMatrix A = new Matrix("[1.0, 2.0]");
       IMatrix AT = new Matrix("[1.0; 2.0]");
       Matrix C = new Matrix(1,1);
       C.multiply(1.0, A, AT, 0.0, false, false);
	   assertEquals("A*A'","[5.0]",C.toString());
       C.multiply(1.0, AT, AT, 0.0, true, false);
	   assertEquals("A*A'","[5.0]",C.toString());
       C.multiply(1.0, A, A, 0.0, false, true);
	   assertEquals("A*A'","[5.0]",C.toString());
       C.multiply(1.0, AT, A, 0.0, true, true);
	   assertEquals("A*A'","[5.0]",C.toString());
	   //public static Matrix mult(Matrix A, Matrix B){
       IMatrix AB = Matrix.mult(A,AT);
	   assertEquals("A*A'","[5.0]",AB.toString());
	   //public Matrix transpose(){
	   Matrix Mat13 = new Matrix("[1.0,2.0,3.0]");
	   IMatrix Mat13_trans = Mat13.transpose();
	   assertEquals("Mat13.transpose()","[1.0;2.0;3.0]",Mat13_trans.toString());
	   //public void scale(double factor){
	   Matrix Mat14 = new Matrix("[1.0,2.0,3.0]");
	   Mat14.scale(10.0);
	   assertEquals("Mat14.scale()","[10.0,20.0,30.0]",Mat14.toString());
	   //public double norm(){
	   Matrix Mat15 = new Matrix("[0.0,0.0,3.0]");
	   double Mat15_norm = Mat15.norm();
	   assertEquals("Mat15.norm()",3.0,Mat15_norm);
       //public static Matrix eye(int n){
	   IMatrix Mat16 = Matrix.eye(2);
	   assertEquals("eye(2)","[1.0,0.0;0.0,1.0]",Mat16.toString());
       //public static Matrix eye(int n){
	   Matrix Mat17 = new Matrix("[1.0,0.0;1.0,1.0]");
	   Matrix invMat17 = Mat17.inverse();
	   Matrix Mat17b = Matrix.mult(Mat17,invMat17);
	   Mat17b.axpy(-1.0, Matrix.eye(2));
	   double normMat17b = Mat17b.norm();
	   assertEquals("invMat17 : norm(A*inv(A))",0.0,normMat17b,0.000001);
   }

   public static void testSimpleMatrix_2() {
	   Matrix Mat1= new Matrix("[1.0,0.0;0.0,2.0;0.0,0.0]");
	   Matrix[] svd = Mat1.svd();
	   IMatrix Mat1b = svd[0].mult(svd[1]).mult(svd[2].transpose());
	   assertEquals("udv","[1.0,0.0;0.0,2.0;0.0,0.0]",Mat1b.toString());

	   assertEquals("Mat1.svd()","[2.0,0.0;0.0,1.0]",svd[1].toString());
	   Matrix Mat2= new Matrix("[0.0;4.0;3.0]");
	   svd = Mat2.svd();
	   assertEquals("Mat2.svd()","[5.0]",svd[1].toString());

	   Matrix Mat3= new Matrix("[1.0,0.0,0.0;0.0,2.0,0.0]");
	   svd = Mat3.svd();
	   assertEquals("Mat3.svd()","[2.0,0.0;0.0,1.0]",svd[1].toString());
	   IVector diagD = svd[1].diag();
	   assertEquals("d.diag()","[2.0,1.0]",diagD.toString());
	   IMatrix Mat3b = svd[0].mult(svd[1]).mult(svd[2].transpose());
	   assertEquals("udv",Mat3b.toString(),"[1.0,0.0,0.0;0.0,2.0,0.0]");

	   //vertical concatenation
	   Matrix Mat4= new Matrix("[1.1,1.2;2.1,2.2;3.1,3.2]");
	   Matrix Mat5= new Matrix("[4.1,4.2;5.1,5.2]");
       Matrix Mat45 = Matrix.concatVertical(Mat4, Mat5);
	   assertEquals("concatVertical(Mat4, Mat5)","[1.1,1.2;2.1,2.2;3.1,3.2;4.1,4.2;5.1,5.2]",Mat45.toString());

	   //horizontal concatenation
	   Matrix Mat6= new Matrix("[1.0,2.0;5.0,6.0;9.0,10.0;13.0,14.0]");
	   Matrix Mat7= new Matrix("[3.0,4.0;7.0,8.0;11.0,12.0]");
	   Matrix Mat67 = Matrix.concatHorizontal(Mat6, Mat7);
	   assertEquals("concatHorizontal(Mat6, Mat7)","[1.0,2.0,3.0,4.0;5.0,6.0,7.0,8.0;9.0,10.0,11.0,12.0;13.0,14.0,0.0,0.0]",Mat67.toString());

	   //remove first row
	   Matrix Mat8 = Mat67.remove_row(0);
	   assertEquals("first row removed: (Mat67)","[5.0,6.0,7.0,8.0;9.0,10.0,11.0,12.0;13.0,14.0,0.0,0.0]",Mat8.toString());

	   //remove row index 1
	   Mat8 = Mat67.remove_row(1);
	   assertEquals("second row removed: (Mat67)","[1.0,2.0,3.0,4.0;9.0,10.0,11.0,12.0;13.0,14.0,0.0,0.0]",Mat8.toString());

	   //remove last row
	   Mat8 = Mat67.remove_row(Mat67.getNumberOfRows());
	   assertEquals("last row removed: (Mat67)","[1.0,2.0,3.0,4.0;5.0,6.0,7.0,8.0;9.0,10.0,11.0,12.0]",Mat8.toString());

	   //try to remove a non-existing row
	   Matrix Mat9 = Mat8.remove_row(Mat8.getNumberOfRows()+1);
	   assertEquals("Remove non-existing row: (Mat9)",Mat8,Mat9);
	   Mat9 = Mat8.remove_row(-1);
	   assertEquals("Remove non-existing row: (Mat9)",Mat8,Mat9);

   }

   public static void testSimpleMatrix_3() {
	   // computation of determinant
	   Matrix Mat1= new Matrix("[1.0,0.0;1.0,2.0]");
	   double det1 = Mat1.determinant();
	   assertEquals("Mat1.determinant()",2.0,det1,0.000001);
   }   

	public static void testOuterProduct(){
		
		System.out.println("==============================================================");
		System.out.println("Outer product test");
		System.out.println("==============================================================");
		IVector v1 = new Vector("[17.0,-5.0,11.0]");
		IVector v2 = new Vector("[13.0,3.0]");
		IVector v3 = new Vector("[7.0,2.0]");	// same size as v2 
		IVector v4 = v1.clone();				// same size as v1
		
	   	System.out.println("v1 = "+v1+"\nv2 = "+v2+"\nv3 = "+v3);
	   	
	   	// outer product
		Matrix v1v2 = Matrix.outer(v1,v2);
    	v1v2.rightMultiply(1, v3, 0, v4);
    	System.out.println("(v1 v2^T) v3 = "+v4);
    	
    	// different order of multiplication, no outer product needed
    	double v2v3 = v2.dotProduct(v3);
    	v1.scale(v2v3);
    	System.out.println("v1 (v2^T v3) = "+v1);
    	
		assertEquals("First variable ",v1.getValue(0),v4.getValue(0));
		assertEquals("Second variable ",v1.getValue(1),v4.getValue(1));
		
	}

	   public static void testMatrixVectorConversion() {
		   Vector v=new Vector("[1,2,3,4,5,6]");
		   Matrix mat= new Matrix(v,3,2);
		   double val11 = mat.getValue(1, 1); //zero based indices
		   assertEquals("mat(1,1)=4",4.0,val11,0.000001);
		   Vector v2=mat.asVector();
		   double v2_val4=v2.getValue(4);
		   assertEquals("v(4)=5",5.0,v2_val4,0.000001);
	   }   

}
