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

import junit.framework.TestCase;
import org.openda.interfaces.ITreeVector;
import org.openda.utils.TreeVector;
import org.openda.utils.Vector;

/**
 * Test for simple vector
 */
public class TreeVectorTest extends TestCase {

    public static void testTreeVector_1() {
    	System.out.println("==============================================================================");
    	System.out.println("TreeVector basics : constructor no content ...");
    	System.out.println("==============================================================================");
    	//public TreeVector(String id) {
        TreeVector treeVector_1 = new TreeVector("tv1");
        //public TreeVector(String id, String caption) {
        ITreeVector subTreeVector_a = new TreeVector("tv1.sub_a");
        ITreeVector subTreeVector_b = new TreeVector("tv1.sub_b");
        treeVector_1.addChild(subTreeVector_a);
        treeVector_1.addChild(subTreeVector_b);
        String treeVector_1_toString = treeVector_1.toString();
        System.out.println("tv1="+treeVector_1_toString);
        assertTrue(treeVector_1_toString.contains("tv1.sub_a"));
        assertTrue(treeVector_1_toString.contains("tv1.sub_b"));
    }

    public static void testTreeVector_2() {
    	System.out.println("==============================================================================");
    	System.out.println("TreeVector basics : constructor with content");
    	System.out.println("==============================================================================");
    	TreeVector treeVector_2 = new TreeVector("tv2");
    	//public TreeVector(String id, String caption, Vector vector) {
        ITreeVector subTreeVector_a = new TreeVector("tv2.sub_a","Caption tv2.sub_a",
        		                                          new Vector("[0.0,1.0,2.0]"));
        //public TreeVector(String Id, Vector vector) {
        ITreeVector subTreeVector_b = new TreeVector("tv2.sub_b",new Vector("[3.0,4.0]"));
        treeVector_2.addChild(subTreeVector_a);
        treeVector_2.addChild(subTreeVector_b);
        String treeVector_2_toString = treeVector_2.toString();
        System.out.println("tv2="+treeVector_2_toString);
        assertTrue(treeVector_2_toString.contains("tv2.sub_a"));
        assertTrue(treeVector_2_toString.contains("tv2.sub_b"));
        assertTrue(treeVector_2_toString.contains("[0.0,1.0,2.0]"));
    }

    public static void testTreeVector_3() {
    	System.out.println("==============================================================================");
    	System.out.println("TreeVector basics : get and set");
    	System.out.println("==============================================================================");
    	TreeVector treeVector_3 = new TreeVector("tv3");
    	//public TreeVector(String id, String caption, Vector vector) {
        ITreeVector subTreeVector_a = new TreeVector("tv3.sub_a","Caption tv3.sub_a",
        		                                          new Vector("[0.0,1.0,2.0]"));
        //public TreeVector(String Id, Vector vector) {
        ITreeVector subTreeVector_b = new TreeVector("tv3.sub_b",new Vector("[3.0,4.0]"));
        treeVector_3.addChild(subTreeVector_a);
        treeVector_3.addChild(subTreeVector_b);
        //public TreeVector(String id, String[] parameterIds, double[] parameterValues) {
        String ids[]    = {"par1","par2","par3","par4","par5"};
        double[] values = {1.0,2.0,3.0,4.0,5.0};
        TreeVector treeVector_3b = new TreeVector("tv3b",ids,values);
        System.out.println("tv3b="+treeVector_3b.toString());
        
        // get and set
        double p1_init = treeVector_3b.getValue(0);
        double p5_init = treeVector_3b.getValue(4);
        System.out.println("tv3b[0]="+p1_init);
        System.out.println("Should be tv3b[0]=1.0");
        System.out.println("tv3b[4]="+p5_init);
        System.out.println("Should be tv3b[4]=5.0");
        assertEquals("treeVector_3b[0]",1.0, p1_init, 1e-6);
        assertEquals("treeVector_3b[4]",5.0, p5_init, 1e-6);
        System.out.println("Set tv3b[0]=1.1");
        treeVector_3b.setValue(0,1.1);
        System.out.println("Set tv3b[4]=5.1");
        treeVector_3b.setValue(4,5.1);
        System.out.println("tv3="+treeVector_3b.toString());
        double p1_modified = treeVector_3b.getValue(0);
        double p5_modified = treeVector_3b.getValue(4);
        System.out.println("tv3b[0]="+p1_init);
        System.out.println("Should be tv3b[0]=1.1");
        System.out.println("tv3b[4]="+p5_init);
        System.out.println("Should be tv3b[4]=5.1");
        assertEquals("treeVector_3b[0]",1.1, p1_modified, 1e-6);
        assertEquals("treeVector_3b[4]",5.1, p5_modified, 1e-6);
        
        
    }

	public static void testTreeVector_4() {
		System.out.println("==============================================================================");
		System.out.println("TreeVector basics : getvalues");
		System.out.println("==============================================================================");
		TreeVector treeVector_4 = new TreeVector("tv4");
		//public TreeVector(String id, String caption, Vector vector) {
		ITreeVector subTreeVector_a = new TreeVector("tv4.sub_a","Caption tv4.sub_a",
				new Vector("[0.0,1.0,2.0]"));
		//public TreeVector(String Id, Vector vector) {
		ITreeVector subTreeVector_b = new TreeVector("tv4.sub_b",new Vector("[3.0,4.0]"));
		treeVector_4.addChild(subTreeVector_a);
		treeVector_4.addChild(subTreeVector_b);

		double[] allvals = treeVector_4.getValues();

		((TreeVector)subTreeVector_b).setExcludeFromVector(true);

		TreeVector treeVector_5 = new TreeVector("tv5");
		treeVector_5.addChild(subTreeVector_a);
		treeVector_5.addChild(subTreeVector_b);
		subTreeVector_b.excludeFromVector();
		double[] allvals_with_exclude = treeVector_5.getValues();

	}


    public static void testTreeVector_5() {
        System.out.println("==============================================================================");
        System.out.println("TreeVector advanced; old errors captured");
        System.out.println("==============================================================================");

        TreeVector x;
        TreeVector y;
        double[] values;
        double value;

        //Construct or vectors for the tests
        Vector x1 = new Vector("[1.0, 2.0]");
        Vector x2 = new Vector("[3.0, 4.0]");
        TreeVector xt1= new TreeVector("top",x1);
        TreeVector xt2= new TreeVector("top");
        TreeVector xt3= new TreeVector("top",x2);
        xt2.addChild(xt3);


        //AXPY
        x=xt1.clone();
        y=xt2.clone();
        x.axpy(1.0, y);
        values = x.getValues();
        assertEquals(4.0, values[0],1e-06);
        assertEquals(6.0, values[1],1e-06);

        x=xt1.clone();
        y=xt2.clone();
        y.axpy(1.0, x);
        values = y.getValues();
        assertEquals(4.0, values[0],1e-06);
        assertEquals(6.0, values[1],1e-06);


        x=xt1.clone();
        y=xt2.clone();
        x.axpy(0.5, y);
        values = x.getValues();
        assertEquals(2.5, values[0],1e-06);
        assertEquals(4.0, values[1],1e-06);

        x=xt1.clone();
        y=xt2.clone();
        y.axpy(0.5,x);
        values = y.getValues();
        assertEquals(3.5, values[0],1e-06);
        assertEquals(5.0, values[1],1e-06);

        //dot product
        x=xt1.clone();
        y=xt2.clone();
        value=x.dotProduct(y);
        assertEquals(11.0, value,1e-06);

        x=xt1.clone();
        y=xt2.clone();
        value=y.dotProduct(x);
        assertEquals(11.0, value,1e-06);

        //point wise divide
        x=xt1.clone();
        y=xt2.clone();
        x.pointwiseDivide(y);
        values=x.getValues();
        assertEquals(1.0/3.0, values[0],1e-06);
        assertEquals(0.5, values[1],1e-06);

        x=xt1.clone();
        y=xt2.clone();
        y.pointwiseDivide(x);
        values=y.getValues();
        assertEquals(3.0, values[0],1e-06);
        assertEquals(2.0, values[1],1e-06);

        //point wise multiply
        x=xt1.clone();
        y=xt2.clone();
        x.pointwiseMultiply(y);
        values=x.getValues();
        assertEquals(3.0, values[0],1e-06);
        assertEquals(8.0, values[1],1e-06);

        x=xt1.clone();
        y=xt2.clone();
        y.pointwiseMultiply(x);
        values=y.getValues();
        assertEquals(3.0, values[0],1e-06);
        assertEquals(8.0, values[1],1e-06);







    }





}

    //public TreeVector(String Id, Vector vector, int iSize, int jSize) {
    //public TreeVector(String Id, Vector vector, int iSize, int jSize, int kSize) {
    //public TreeVector(String Id, Vector vector, DimensionIndex[] dimensionIndices) {
    //public void setId(String id) {
    //public String getId() {
    //public void setCaption(String caption) {
    //public String getCaption() {
    //public void setDescription(String description) {
    //public String getDescription() {
    //public ArrayList<String> getSubTreeVectorIds() {
    //public void addChild(TreeVector subTreeVector) {
    //public TreeVector getSubTreeVector(String id) {
    //public DimensionIndex[] getDimensionIndices() {
    //public void setExcludeFromVector(boolean excludeFromVector) {
    //public boolean excludeFromVector() {
    //public void setConstant(double value) {
    //public void scale(double alpha) {
    //public void setValues(double[] values) {
    //public void setValues(org.openda.interfaces.Vector source){
    //public double[] getValues(){
    //public void setValue(int index, double value){
    //public double getValue(int index){
    //public int getSize() {
    //public void axpy(double alpha, Vector x) {
    //public double dotProduct(Vector otherVector) {
    //public double norm2() {
    //public void pointwiseDivide(Vector otherVector) {
    //public void pointwiseMultiply(Vector otherVector) {
    //public void sqrt()
    //public TreeVector clone(){
    //public void free() {
    //public String toString() {
