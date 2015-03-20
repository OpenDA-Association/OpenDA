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
import junit.framework.TestCase;
import org.openda.interfaces.*;

public class ArrayTest extends TestCase{

    public static void testArray_1() {
    	System.out.println("========================================================");
    	System.out.println(" Array Constructors and data access");
    	System.out.println("========================================================");
    	
    	double[] values = {1.0,2.0,3.0,4.0,5.0,6.0};
    	int[] dimensions={3,2};
    	boolean copyValues=false;
    	Array array1 = new Array(values, dimensions, copyValues);
    	System.out.println("array1="+array1);
    	System.out.println("Should be array1={{1.0,2.0},{3.0,4.0},{5.0,6.0}}");
    	assertEquals("{{1.0,2.0},{3.0,4.0},{5.0,6.0}}", array1.toString());

    	//Array(IArray sourceArray)
    	Array array2 = new Array(array1);
    	System.out.println("array2="+array2);
    	System.out.println("Should be array2={{1.0,2.0},{3.0,4.0},{5.0,6.0}}");
    	assertEquals("{{1.0,2.0},{3.0,4.0},{5.0,6.0}}", array2.toString());

    	//Array(String source)
    	Array array3 = new Array("{{1.0,2.0},{3.0,4.0},{5.0,6.0}}");
    	System.out.println("array3="+array3);
    	System.out.println("Should be array3={{1.0,2.0},{3.0,4.0},{5.0,6.0}}");
    	assertEquals("{{1.0,2.0},{3.0,4.0},{5.0,6.0}}", array3.toString());
   
    	//Array(String source)
    	Array array4 = new Array("{{{1.0,2.0},{3.0,4.0},{5.0,6.0}},{{10.0,20.0},{30.0,40.0},{50.0,60.0}}}");
    	System.out.println("array4="+array4);
    	System.out.println("Should be array4={{{1.0,2.0},{3.0,4.0},{5.0,6.0}},{{10.0,20.0},{30.0,40.0},{50.0,60.0}}}");
    	assertEquals("{{{1.0,2.0},{3.0,4.0},{5.0,6.0}},{{10.0,20.0},{30.0,40.0},{50.0,60.0}}}", array4.toString());
    }
    
    public static void testArray_2() {
    	System.out.println("========================================================");
    	System.out.println(" Array meta info");
    	System.out.println("========================================================");

    	//Array(String source)
    	Array array = new Array("{{1.0,2.0},{3.0,4.0},{5.0,6.0}}");
    	System.out.println("array="+array);

    	int k = array.getNumberOfDimensions();
    	assertEquals(2, k);
    	
    	int[] dims = array.getDimensions();
    	assertEquals(2,dims.length);
    	assertEquals(3,dims[0]);
    	assertEquals(2,dims[1]);
    	
    	int n = array.length();
    	assertEquals(6,n);
    }
    
    public static void testArray_3() {
    	System.out.println("========================================================");
    	System.out.println(" Array values");
    	System.out.println("========================================================");

    	//Array(String source)
    	Array array = new Array("{{1.0,2.0},{3.0,4.0},{5.0,6.0}}");
    	System.out.println("array="+array);

    	double[] values = array.getValuesAsDoubles(false);
    	assertEquals(6,values.length);
    	double eps=1e-12;
    	assertEquals(1.0,values[0],eps);
    	assertEquals(6.0,values[5],eps);
    	
    	double[] someValues = array.getValuesAsDoubles(2, 3);
    	assertEquals(2,someValues.length);
    	assertEquals(3.0,someValues[0],eps);
    	assertEquals(4.0,someValues[1],eps);

    	array.setValuesAsDoubles(2, 3, new double[]{30.,40.});
    	someValues = array.getValuesAsDoubles(2, 3);
    	assertEquals(2,someValues.length);
    	assertEquals(30.0,someValues[0],eps);
    	assertEquals(40.0,someValues[1],eps);

    	double value=array.getValueAsDouble(new int[]{2,1}); //counting from 0
    	assertEquals(6.0,value,eps);

    	array.setValueAsDouble(new int[]{2,1}, 60.);
    	value=array.getValueAsDouble(new int[]{2,1}); //counting from 0
    	assertEquals(60.0,value,eps);

    	//int valueIndex(int[] indices)
    	int index = array.valueIndex(new int[]{2,0});
    	assertEquals(4,index);
    	
    	int index2 = array.index2(2, 0);
    	assertEquals(4,index2);

    	value=array.getValueAsDouble(5); //counting from 0
    	assertEquals(60.0,value,eps);
    	array.setValueAsDouble(5, 600.);
    	value=array.getValueAsDouble(5); //counting from 0
    	assertEquals(600.0,value,eps);
    	
    	Array array2 = new Array(3);
    	array2.setConstant(10.);
    	assertEquals(10.0,array2.getValueAsDouble(0),eps);
    	assertEquals(10.0,array2.getValueAsDouble(2),eps);
    	
    }

    public static void testArray_4() {
    	System.out.println("========================================================");
    	System.out.println(" Array slices");
    	System.out.println("========================================================");

    	//Array(String source)
    	Array array = new Array("{{1.0,2.0},{3.0,4.0},{5.0,6.0}}");
    	System.out.println("array="+array);
    	
    	//IArray getSlice(int dimension, int index)
    	IArray arraySlice1 = array.getSlice(0, 1); // {3.0,4.0}
    	System.out.println("arraySlice1="+arraySlice1);
    	System.out.println("Should be arraySlice1={3.0,4.0}");
    	assertEquals("{3.0,4.0}", arraySlice1.toString());

    	IArray arraySlice2 = array.getSlice(1, 1); // {2.0,4.0,6.0}
    	System.out.println("arraySlice2="+arraySlice2);
    	System.out.println("Should be arraySlice2={2.0,4.0,6.0}");
    	assertEquals("{2.0,4.0,6.0}", arraySlice2.toString());

    	//IArray getSlice(int dimension, int minIndex, int maxIndex)
    	//{{1.0,2.0},
    	// {3.0,4.0},
    	// {5.0,6.0}}
    	// array(1:2,:) base=0
    	IArray arraySlice3 = array.getSlice(0, 1, 2); // {{3.0,4.0},{5.0,6.0}}
    	System.out.println("arraySlice3="+arraySlice3);
    	System.out.println("Should be arraySlice3={{3.0,4.0},{5.0,6.0}}");
    	assertEquals("{{3.0,4.0},{5.0,6.0}}", arraySlice3.toString());

    	//double[] getSliceAsDoubles(int dimension, int minIndex, int maxIndex)
    	double[] slice1 = array.getSliceAsDoubles(0, 1, 2); // {{3.0,4.0},{5.0,6.0}}
    	assertEquals(4,slice1.length);
    	double eps=1e-12;
    	assertEquals(3.0,slice1[0],eps);
    	assertEquals(4.0,slice1[1],eps);
    	assertEquals(5.0,slice1[2],eps);
    	assertEquals(6.0,slice1[3],eps);
    	
    	//void setSlice(IArray slice, int dimension, int index)
    	IArray newSlice1 = new Array("{20.0,40.0,60.0}");
    	IArray arrayCopy1 = new Array(array);
    	arrayCopy1.setSlice(newSlice1, 1, 1);
    	System.out.println("arrayCopy1="+arrayCopy1);
    	System.out.println("Should be arrayCopy1={{1.0,20.0},{3.0,40.0},{5.0,60.0}}");
    	assertEquals("{{1.0,20.0},{3.0,40.0},{5.0,60.0}}", arrayCopy1.toString());
    	
    	IArray newSlice2 = new Array("{30.0,40.0}");
    	IArray arrayCopy2 = new Array(array);
    	arrayCopy2.setSlice(newSlice2, 0, 1);
    	System.out.println("arrayCopy2="+arrayCopy2);
    	System.out.println("Should be arrayCopy2={{1.0,2.0},{30.0,40.0},{5.0,6.0}}");
    	assertEquals("{{1.0,2.0},{30.0,40.0},{5.0,6.0}}", arrayCopy2.toString());

    	//void setSlice(IArray slice, int dimension, int minIndex, int maxIndex)
    	IArray newSlice3 = new Array("{{30.0,40.0},{50.0,60.0}}");
    	IArray arrayCopy3 = new Array(array);
    	arrayCopy3.setSlice(newSlice3, 0, 1, 2);
    	System.out.println("arrayCopy3="+arrayCopy3);
    	System.out.println("Should be arrayCopy3={{1.0,2.0},{30.0,40.0},{50.0,60.0}}");
    	assertEquals("{{1.0,2.0},{30.0,40.0},{50.0,60.0}}", arrayCopy3.toString());
    	
    	//void setSlice(double[] slice, int dimension, int minIndex, int maxIndex)
    	double[] newSlice4 = new double[]{30.0,40.0,50.0,60.0};
    	IArray arrayCopy4 = new Array(array);
    	arrayCopy4.setSlice(newSlice4, 0, 1, 2);
    	System.out.println("arrayCopy4="+arrayCopy4);
    	System.out.println("Should be arrayCopy4={{1.0,2.0},{30.0,40.0},{50.0,60.0}}");
    	assertEquals("{{1.0,2.0},{30.0,40.0},{50.0,60.0}}", arrayCopy4.toString());
    	
    }

    public static void testArray_5() {
    	System.out.println("========================================================");
    	System.out.println(" Array utilities");
    	System.out.println("========================================================");

    	double start=100.;
    	int n=5;
    	double delta=0.1;
    	Array range=Array.Range1(start,n,delta);
    	assertEquals("length",5,range.length());
    	assertEquals("number of dimenions",1,range.getNumberOfDimensions());
    	assertEquals("range[0]",100.,range.getValueAsDouble(0),1e-6);
    	assertEquals("range[1]",100.1,range.getValueAsDouble(1),1e-6);
    	assertEquals("range[4]",100.4,range.getValueAsDouble(4),1e-6);

    	
    }
}
