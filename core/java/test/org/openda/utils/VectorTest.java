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
 * Main class for testing vector class
 */

package org.openda.utils;

import junit.framework.TestCase;

/**
 * Test for simple vector
 */
public class VectorTest extends TestCase {

    public static void testVector_1() {
    	System.out.println("========================================================");

        //    Vector(int n)

        Vector v1 = new Vector(3); //initializes with 0
        assertEquals("v1 a", "[0.0,0.0,0.0]", v1.toString());

        //    Vector(double[] values)

        double[] v2_values = {1.0, 2.0, 3.0};
        Vector v2 = new Vector(v2_values); //initializes with doubles
        assertEquals("v2", "[1.0,2.0,3.0]", v2.toString());
        Vector v3 = new Vector("[10.0,20.0,30.0]"); //initializes with string
        assertEquals("v3", "[10.0,20.0,30.0]", v3.toString());

        //    void setConstant(double value)

        v1.setConstant(1.0);
        assertEquals("v1 b", "[1.0,1.0,1.0]", v1.toString());

        //    void scale(double alpha)

        v1.scale(2.0);
        assertEquals("v1 c", "[2.0,2.0,2.0]", v1.toString());

        //    void setValues(double[] values)

        double[] v1_values = {3.0, 2.0, 1.0};
        v1.setValues(v1_values);
        assertEquals("v1 d", "[3.0,2.0,1.0]", v1.toString());

        //    void setValues(Vector source)

        v1.setValues(v2.getValues());
        assertEquals("v1 e", "[1.0,2.0,3.0]", v1.toString());

        //    double[] getValues()

        v1_values = v1.getValues();
        assertEquals("v1_values[0]", 1.0, v1_values[0],0.0001);
        assertEquals("v1_values[1]", 2.0, v1_values[1],0.0001);
        assertEquals("v1_values[2]", 3.0, v1_values[2],0.0001);

        //    void setValue(int index, double value)

        v1.setValue(0, 4.0);
        assertEquals("v1 f", "[4.0,2.0,3.0]", v1.toString());

        //    double getValue(int index)

        double value = v1.getValue(0);
        assertEquals("value v1[0]", 4.0, value,0.0001);

        //    int getSize()

        int n = v1.getSize();
        assertEquals("v1.getSize", 3, n);

        //    void axpy(double alpha, Vector x)

        v1.axpy(1.0, v2); // [4,2,3]+1*[1,2,3]=[5,4,6]
        assertEquals("v1 g", "[5.0,4.0,6.0]", v1.toString());

        //    double dotProduct(Vector otherVector)

        double dotVal = v1.dotProduct(v2); // [5,4,6]*[1,2,3]=31
        assertEquals("dot(v1,v2)", 31.0, dotVal,0.0001);

        //    double norm2()

        v2 = new Vector("[3.0,4.0]");
        double normVal = v2.norm2(); // |(3,4)|=sqrt(3*3+4*4) = 5
        assertEquals("norm2(v2)", 5.0, normVal,0.0001);

        //    public void pointwiseDivide(Vector otherVector)

        v1.pointwiseDivide(v1); // [5,4,6]./[5,4,6]=[1,1,1]
        assertEquals("v1 h", "[1.0,1.0,1.0]", v1.toString());

        //    public void pointwiseMultiply(Vector otherVector)

        v1 = new Vector("[1.0,2.0,3.0]");
        v1.pointwiseMultiply(v1); // [1,2,3].*[1,2,3]=[1,4,9]
        assertEquals("v1 i", "[1.0,4.0,9.0]", v1.toString());

		//   public remove_entry
		v3 = new Vector("[1.0,2.0,3.0,4.0]");
 		// remove first entry
		Vector result = v3.remove_entry(0);
		assertEquals("first index removed: ","[2.0,3.0,4.0]",result.toString());
		//remove some entry
		result = v3.remove_entry(2);
		assertEquals("middle index removed: ","[1.0,2.0,4.0]",result.toString());
		// remove last entry
		result = v3.remove_entry(v3.getSize()-1);
		assertEquals("last index removed: ","[1.0,2.0,3.0]",result.toString());
		// remove non-existiing entry
		result = v3.remove_entry(-1);
		assertEquals("return original vector: ",v3,result);

		//   public remove_entry
		v3 = new Vector("[1.0,2.0,3.0,4.0,5.0]");
		// get subvector
		result = v3.get_selection(1,3);
		assertEquals("sub1: ","[2.0,3.0,4.0]",result.toString());
		result = v3.get_selection(1,1);
		assertEquals("sub1: ","[2.0]",result.toString());
		// test out of bounds situations
		result = v3.get_selection(-1,1);
		assertEquals("i1<0","[1.0,2.0]",result.toString());
		result = v3.get_selection(3,2);
		assertNull("i1>i2: ",result);
		result = v3.get_selection(1,v3.getSize()+2);
		assertEquals("return original vector: ","[2.0,3.0,4.0,5.0]",result.toString());
	}

    public static void testVector_2() {
    	System.out.println("========================================================");
    	Vector v2 = (Vector) Vector.range(0.0, 5.0, 0.5);
    	System.out.println("range(0.0,5.0,0.5) = "+v2);
    	System.out.println("Should be range(0.0,5.0,0.5) = [0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0]");
    	assertEquals("Vector.range()",v2.toString(),"[0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0]");
    	
    	Vector va = new Vector("[1.0,2.0,3.0]");
    	Vector vb = new Vector("[4.0,5.0,6.0]");
    	Vector vab = Vector.concatenate(va, vb);
    	System.out.println("Vector.concatenate(va, vb) = "+vab);
    	System.out.println("Should be Vector.concatenate(va, vb) = [1.0,2.0,3.0,4.0,5.0,6.0]");
    	assertEquals("Vector.concatenate(va, vb)",vab.toString(),"[1.0,2.0,3.0,4.0,5.0,6.0]");
    }
}


