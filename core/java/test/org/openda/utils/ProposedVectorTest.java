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
 * Main class for testing ProposedVector class
 */


package org.openda.utils;

import junit.framework.TestCase;
import org.openda.interfaces.ProposedIVector;
import org.openda.utils.Vector;

/**
 * Test for simple vector
 */
public class ProposedVectorTest extends TestCase {

    public static void testVector_1() {
    	System.out.println("========================================================");

        //    ProposedVector(int n)

        ProposedIVector v1 = new ProposedVector(3); //initializes with 0
        assertEquals("v1 a", "[0.0,0.0,0.0]", v1.toString());

        //    ProposedVector(double[] values)

        double[] v2_values = {1.0, 2.0, 3.0};
        ProposedIVector v2 = new ProposedVector(v2_values); //initializes with doubles
        assertEquals("v2", "[1.0,2.0,3.0]", v2.toString());
        ProposedIVector v3 = new ProposedVector("[10.0,20.0,30.0]"); //initializes with string
        assertEquals("v3", "[10.0,20.0,30.0]", v3.toString());

        //    void setConstant(double value)

        v1.setConstant(1.0);
        assertEquals("v1 b", "[1.0,1.0,1.0]", v1.toString());

        //    void scale(double alpha)

        v1.scale(2.0);
        assertEquals("v1 c", "[2.0,2.0,2.0]", v1.toString());

        //    void setValues(double[] values)

        double[] v1_values = {3.0, 2.0, 1.0};
        v1.setValuesAsDoubles(v1_values);
        assertEquals("v1 d", "[3.0,2.0,1.0]", v1.toString());

        //    void setValues(ProposedVector source)

        v1.setValuesAsDoubles(v2.getValuesAsDoubles());
        assertEquals("v1 e", "[1.0,2.0,3.0]", v1.toString());

        //    double[] getValues()

        v1_values = v1.getValuesAsDoubles();
        assertEquals("v1_values[0]", 1.0, v1_values[0],0.0001);
        assertEquals("v1_values[1]", 2.0, v1_values[1],0.0001);
        assertEquals("v1_values[2]", 3.0, v1_values[2],0.0001);

        //    void setValue(int index, double value)

        v1.setValueAsDouble(0, 4.0);
        assertEquals("v1 f", "[4.0,2.0,3.0]", v1.toString());

        //    double getValue(int index)

        double value = v1.getValueAsDouble(0);
        assertEquals("value v1[0]", 4.0, value,0.0001);

        //    int getSize()

        int n = v1.length();
        assertEquals("v1.getSize", 3, n);

        //    void axpy(double alpha, ProposedVector x)

        v1.axpy(1.0, v2); // [4,2,3]+1*[1,2,3]=[5,4,6]
        assertEquals("v1 g", "[5.0,4.0,6.0]", v1.toString());

        //    double dotProduct(ProposedVector otherVector)

        double dotVal = v1.dotProduct(v2); // [5,4,6]*[1,2,3]=31
        assertEquals("dot(v1,v2)", 31.0, dotVal,0.0001);

        //    double norm2()

        v2 = new ProposedVector("[3.0,4.0]");
        double normVal = v2.norm2(); // |(3,4)|=sqrt(3*3+4*4) = 5
        assertEquals("norm2(v2)", 5.0, normVal,0.0001);

        //    public void pointwiseDivide(ProposedVector otherVector)

        v1.pointwiseDivide(v1); // [5,4,6]./[5,4,6]=[1,1,1]
        assertEquals("v1 h", "[1.0,1.0,1.0]", v1.toString());

        //    public void pointwiseMultiply(ProposedVector otherVector)

        v1 = new ProposedVector("[1.0,2.0,3.0]");
        v1.pointwiseMultiply(v1); // [1,2,3].*[1,2,3]=[1,4,9]
        assertEquals("v1 i", "[1.0,4.0,9.0]", v1.toString());

    }//end test

    public static void testVector_2() {
    	System.out.println("========================================================");
    	ProposedIVector v2 = ProposedVector.range(0.0, 5.0, 0.5);
    	System.out.println("range(0.0,5.0,0.5) = "+v2);
    	System.out.println("Should be range(0.0,5.0,0.5) = [0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0]");
    	assertEquals("Vector.range()",v2.toString(),"[0.0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0]");
    	
    	ProposedVector va = new ProposedVector("[1.0,2.0,3.0]");
    	ProposedVector vb = new ProposedVector("[4.0,5.0,6.0]");
    	ProposedVector vab = ProposedVector.concatenate(va, vb);
    	System.out.println("Vector.concatenate(va, vb) = "+vab);
    	System.out.println("Should be Vector.concatenate(va, vb) = [1.0,2.0,3.0,4.0,5.0,6.0]");
    	assertEquals("Vector.concatenate(va, vb)",vab.toString(),"[1.0,2.0,3.0,4.0,5.0,6.0]");
    }

}//end class


