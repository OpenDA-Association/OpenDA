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

/**
 * TODO: class description
 * Main class for testing SqrtCovariance class
 */


package org.openda.utils;

import junit.framework.TestCase;

import org.openda.interfaces.*;
import org.openda.utils.Matrix;
import org.openda.utils.SqrtCovariance;
import org.openda.utils.StochVector;
import org.openda.utils.Vector;


/**
 * Main class
 */
public class SqrtCovarianceTest extends TestCase{

	public static void testSqrtCovariance_1() {
		System.out.println("==============================================================================");
		System.out.println("Tests for diagonal SqrtCovariance basic output");
		System.out.println("==============================================================================");

		IVector mean1 = new Vector("[0.0,0.0]");
		IVector std1  = new Vector("[0.1,0.1]");
		IStochVector sv1= new StochVector(mean1,std1);
		System.out.println("sv1 = "+sv1.toString());  //explicit call to toString() is not needed
		System.out.println("should be sv1 = {[0.0,0.0],[0.1,0.1]}");
		ISqrtCovariance l1 = sv1.getSqrtCovariance();

		// toString
		System.out.println("l1="+l1.toString());
		System.out.println("Should be l1=diag([0.1,0.1])");
		assertEquals("ls.toString",l1.toString(),"diag([0.1,0.1])");
		//    public int getNumberOfColumns(){
		int noCols = l1.getNumberOfColumns();
		assertEquals("l1.getNumberOfColumns()",noCols,2);
		//    public int getNumberOfColumns(){
		int noRows= l1.getNumberOfRows();
		assertEquals("l1.getNumberOfRows()",noRows,2);
		//    public  Vector[] asVectorArray(){
		IVector[] Lvec = l1.asVectorArray();
		int n = Lvec.length;
		IVector v=null;
		for(int i=0;i<n;i++){
			v = new Vector(2); // here [1.0,0.0] and [0.0,1.0]
			v.setValue(i, 0.1);
			assertEquals("l1.asVectorArray()",Lvec[i].toString(),v.toString());
			System.out.println("Lvec[:,"+i+"]="+Lvec[i].toString());
			System.out.println("Should be Lvec[:,"+i+"]="+v.toString());
		}

	}

	public static void testSqrtCovariance_2() {
		System.out.println("==============================================================================");
		System.out.println("Tests for diagonal SqrtCovariance Matrix output");
		System.out.println("==============================================================================");

        ISqrtCovariance L = new SqrtCovariance(new Matrix("[2.0,0.0,0.0;0.0,4.0,0.0]"));
        IVector v = new Vector("[1.0,1.0,1.0]");
        IVector x = new Vector("[1.0,1.0]");
		//    public  void rightMultiply(double beta, Vector v, double alpha, Vector x){
        // x=alpha*x+beta*L*v
        L.rightMultiply(1.0, v, 0.0, x);
		System.out.println("L.rightMultiply(0.0, v, 1.0, x)="+x.toString());
		System.out.println("Should be L.rightMultiply(0.0, v, 1.0, x)=[2.0,4.0]");
		assertEquals("L.rightMultiply(0.0, v, 1.0, x)",x.toString(),"[2.0,4.0]");        
		//    public  void leftMultiply(double beta, Vector x, double alpha, Vector v){
		//    public  void leftSolve(Vector v, Vector x){
		//    public  void rightSolve(Vector x, Vector v){
		//    public  Matrix asMatrix(){
		//    public  Matrix getMatrixSelection(int rowmin, int rowmax, int colmin, int colmax){
		//TODO
	}
	
	public static void testSqrtCovariance_3() {
		System.out.println("==============================================================================");
		System.out.println("Tests for fullMatrix SqrtCovariance basics output");
		System.out.println("==============================================================================");
		ISqrtCovariance L3 = new SqrtCovariance(new Matrix("[1.0,0.0;0.0,1.0]"));
		System.out.println("L3="+L3.toString());
		System.out.println("Should be L3=full([1.0,0.0;0.0,1.0])");
		assertEquals("L3.toString",L3.toString(),"full([1.0,0.0;0.0,1.0])");
		System.out.println("==============================================================================");
		IMatrix M3 = L3.asMatrix();
		System.out.println("L3.asMatrix="+M3.toString());
		System.out.println("Should be L3.asMatrix=[1.0,0.0;0.0,1.0]");
		assertEquals("L3.asMatrix",M3.toString(),"[1.0,0.0;0.0,1.0]");
		//    public int getNumberOfColumns(){
		int noCols = L3.getNumberOfColumns();
		assertEquals("L3.getNumberOfColumns()",noCols,2);
		//    public int getNumberOfColumns(){
		int noRows= L3.getNumberOfRows();
		assertEquals("L3.getNumberOfRows()",noRows,2);
		//    public  Vector[] asVectorArray(){
		IVector[] Lvec = L3.asVectorArray();
		int n = Lvec.length;
		IVector v=null;
		for(int i=0;i<n;i++){
			v = new Vector(2); // here [1.0,0.0] and [0.0,1.0]
			v.setValue(i, 1.0);
			assertEquals("l1.asVectorArray()",Lvec[i].toString(),v.toString());
			System.out.println("Lvec[:,"+i+"]="+Lvec[i].toString());
			System.out.println("Should be Lvec[:,"+i+"]="+v.toString());
		}
	}

	public static void testSqrtCovariance_4() {
		System.out.println("==============================================================================");
		System.out.println("Tests for fulMatrix SqrtCovariance computations");
		System.out.println("==============================================================================");

		//TODO assertTrue("test4",false);
	}

	public static void testSqrtCovariance_5() {
		System.out.println("==============================================================================");
		System.out.println("Tests for fulMatrix SqrtCovariance connected to StochVector");
		System.out.println("==============================================================================");

		//TODO assertTrue("test5",false);
	}



}//end class
