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

import org.openda.exchange.ArrayExchangeItem;
import org.openda.exchange.QuantityInfo;
import org.openda.exchange.TimeInfo;
import org.openda.interfaces.IArray;
import org.openda.interfaces.IArrayExchangeItem;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.IPrevExchangeItem.Role;
import org.openda.interfaces.IArrayTimeInfo;
import org.openda.interfaces.ProposedITreeVector;
import org.openda.utils.ProposedTreeVector;
import org.openda.utils.ProposedVector;

/**
 * Test for simple ProposedVector
 */
public class ProposedTreeVectorTest extends TestCase {

    public static void testProposedTreeVector_1() {
    	System.out.println("==============================================================================");
    	System.out.println("ProposedTreeVector basics : constructor no content ...");
    	System.out.println("==============================================================================");
    	//public ProposedTreeVector(String id) {
        ProposedTreeVector ProposedTreeVector_1 = new ProposedTreeVector("tv1");
        //public ProposedTreeVector(String id, String caption) {
        ProposedITreeVector subProposedTreeVector_a = new ProposedTreeVector("tv1.sub_a");
        ProposedITreeVector subProposedTreeVector_b = new ProposedTreeVector("tv1.sub_b");
        ProposedTreeVector_1.addChild(subProposedTreeVector_a);
        ProposedTreeVector_1.addChild(subProposedTreeVector_b);
        String ProposedTreeVector_1_toString = ProposedTreeVector_1.toString();
        System.out.println("tv1="+ProposedTreeVector_1_toString);
        assertTrue(ProposedTreeVector_1_toString.contains("tv1.sub_a"));
        assertTrue(ProposedTreeVector_1_toString.contains("tv1.sub_b"));
    }

    public static void testProposedTreeVector_2() {
    	System.out.println("==============================================================================");
    	System.out.println("ProposedTreeVector basics : constructor with content");
    	System.out.println("==============================================================================");
    	ProposedTreeVector ProposedTreeVector_2 = new ProposedTreeVector("tv2");
    	//public ProposedTreeVector(String id, String caption, ProposedVector ProposedVector) {
        ProposedITreeVector subProposedTreeVector_a = new ProposedTreeVector("tv2.sub_a","Caption tv2.sub_a",
        		                                          new ProposedVector("[0.0,1.0,2.0]"));
        //public ProposedTreeVector(String Id, ProposedVector ProposedVector) {
        ProposedITreeVector subProposedTreeVector_b = new ProposedTreeVector("tv2.sub_b",new ProposedVector("[3.0,4.0]"));
        ProposedTreeVector_2.addChild(subProposedTreeVector_a);
        ProposedTreeVector_2.addChild(subProposedTreeVector_b);
        String ProposedTreeVector_2_toString = ProposedTreeVector_2.toString();
        System.out.println("tv2="+ProposedTreeVector_2_toString);
        assertTrue(ProposedTreeVector_2_toString.contains("tv2.sub_a"));
        assertTrue(ProposedTreeVector_2_toString.contains("tv2.sub_b"));
        assertTrue(ProposedTreeVector_2_toString.contains("[0.0,1.0,2.0]"));
    }

    public static void testProposedTreeVector_3() {
    	System.out.println("==============================================================================");
    	System.out.println("ProposedTreeVector basics : get and set");
    	System.out.println("==============================================================================");
    	ProposedTreeVector ProposedTreeVector_3 = new ProposedTreeVector("tv3");
    	//public ProposedTreeVector(String id, String caption, ProposedVector ProposedVector) {
        ProposedITreeVector subProposedTreeVector_a = new ProposedTreeVector("tv3.sub_a","Caption tv3.sub_a",
        		                                          new ProposedVector("[0.0,1.0,2.0]"));
        //public ProposedTreeVector(String Id, ProposedVector ProposedVector) {
        ProposedITreeVector subProposedTreeVector_b = new ProposedTreeVector("tv3.sub_b",new ProposedVector("[3.0,4.0]"));
        ProposedTreeVector_3.addChild(subProposedTreeVector_a);
        ProposedTreeVector_3.addChild(subProposedTreeVector_b);
        //public ProposedTreeVector(String id, String[] parameterIds, double[] parameterValues) {
        String ids[]    = {"par1","par2","par3","par4","par5"};
        double[] values = {1.0,2.0,3.0,4.0,5.0};
        ProposedTreeVector ProposedTreeVector_3b = new ProposedTreeVector("tv3b",ids,values);
        System.out.println("tv3b="+ProposedTreeVector_3b.toString());
        
        // get and set
        double p1_init = ProposedTreeVector_3b.getValueAsDouble(0);
        double p5_init = ProposedTreeVector_3b.getValueAsDouble(4);
        System.out.println("tv3b[0]="+p1_init);
        System.out.println("Should be tv3b[0]=1.0");
        System.out.println("tv3b[4]="+p5_init);
        System.out.println("Should be tv3b[4]=5.0");
        assertEquals("ProposedTreeVector_3b[0]",1.0, p1_init, 1e-6);
        assertEquals("ProposedTreeVector_3b[4]",5.0, p5_init, 1e-6);
        System.out.println("Set tv3b[0]=1.1");
        ProposedTreeVector_3b.setValueAsDouble(0,1.1);
        System.out.println("Set tv3b[4]=5.1");
        ProposedTreeVector_3b.setValueAsDouble(4,5.1);
        System.out.println("tv3="+ProposedTreeVector_3b.toString());
        double p1_modified = ProposedTreeVector_3b.getValueAsDouble(0);
        double p5_modified = ProposedTreeVector_3b.getValueAsDouble(4);
        System.out.println("tv3b[0]="+p1_init);
        System.out.println("Should be tv3b[0]=1.1");
        System.out.println("tv3b[4]="+p5_init);
        System.out.println("Should be tv3b[4]=5.1");
        assertEquals("ProposedTreeVector_3b[0]",1.1, p1_modified, 1e-6);
        assertEquals("ProposedTreeVector_3b[4]",5.1, p5_modified, 1e-6); 
    }

    public static void testProposedTreeVector_4() {
    	System.out.println("==============================================================================");
    	System.out.println("ProposedTreeVector linear algebra : div");
    	System.out.println("==============================================================================");
    	ProposedTreeVector v4 = new ProposedTreeVector("tv3");
    	//public ProposedTreeVector(String id, String caption, ProposedVector ProposedVector) {
        ProposedITreeVector sub_a = new ProposedTreeVector("tv3.sub_a","Caption tv3.sub_a",
        		                                          new ProposedVector("[0.0,1.0,2.0]"));
        //public ProposedTreeVector(String Id, ProposedVector ProposedVector) {
        ProposedITreeVector sub_b = new ProposedTreeVector("tv3.sub_b",new ProposedVector("[3.0,4.0]"));
        v4.addChild(sub_a);
        v4.addChild(sub_b);
  
        System.out.println(v4.toString());
        
        v4.scale(10.0);
        double[] values=v4.getValuesAsDoubles();
        double delta=1e-8;
        assertEquals(40.0, values[4], delta);
        ProposedITreeVector v4b = v4.clone();
        v4b.setConstant(1.0);
        values=v4b.getValuesAsDoubles();
        assertEquals(1.0, values[3], delta);
        v4.axpy(1.0, v4b);
        values=v4.getValuesAsDoubles();
        assertEquals(31.0, values[3], delta);

        double dot = v4.dotProduct(v4);
        assertEquals(3205.0, dot, delta);

    }
    
    public static void testProposedTreeVector_5() {
    	System.out.println("==============================================================================");
    	System.out.println("ProposedTreeVector exchangeItems : create and various");
    	System.out.println("==============================================================================");
    	ArrayExchangeItem windxEI = new ArrayExchangeItem("windx",Role.InOut);
    	IArray windxArray = new Array("{{1.0,1.1},{2.0,2.1},{3.0,3.1}}");
    	windxEI.setArray(windxArray);
    	IArrayTimeInfo times = new TimeInfo("201202010000,201202010600,201202011200",true);
    	windxEI.setTimeInfo(times);
    	IQuantityInfo windxQI=new QuantityInfo("wind_east", "m/s");
    	windxEI.setQuantityInfo(windxQI);
    	
    	ArrayExchangeItem windyEI = new ArrayExchangeItem("windy",Role.InOut);
    	IArray windyArray = new Array("{{4.0,4.1},{5.0,5.1},{6.0,6.1}}");
    	windyEI.setArray(windyArray);
    	windxEI.setTimeInfo(times);
    	IQuantityInfo windyQI=new QuantityInfo("wind_east", "m/s");
    	windyEI.setQuantityInfo(windyQI);
    	
    	ProposedITreeVector u = new ProposedTreeVector(windxEI);
    	ProposedITreeVector v = new ProposedTreeVector(windyEI);
    	ProposedTreeVector wind = new ProposedTreeVector("wind");
    	wind.addChild(u);
    	wind.addChild(v);
    	
    	System.out.println("wind="+wind);
    	
    	double[] values = wind.getValuesAsDoubles();
    	assertEquals(values.length,12);
    	double delta=1e-8;
    	assertEquals(1.0,values[0],delta);
    	assertEquals(2.1,values[3],delta);
    	assertEquals(4.0,values[6],delta);
    }
    //TODO More testing is needed here
    
}

 