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
package org.openda;
import junit.framework.TestCase;
import org.openda.NothingUseful;

public class NothingUsefulTest extends TestCase{
	
    public static void test_double() {
    	System.out.println("========================================================");
    	NothingUseful myObject = new NothingUseful();
    	double value=10.0;
    	myObject.setDouble(value);
    	double valueFromObject = myObject.getDouble();
    	assertEquals("double passed ok?",value, valueFromObject, 0.0001);
    	
    }

    public static void test_boolean() {
    	System.out.println("========================================================");
    	NothingUseful myObject = new NothingUseful();
    	boolean valueTrue=true;
    	myObject.setBoolean(valueTrue);
    	boolean valueFromObject = myObject.getBoolean();
    	assertEquals("boolean passed ok?",valueTrue, valueFromObject);
    	boolean valueFalse=false;
    	myObject.setBoolean(valueFalse);
    	valueFromObject = myObject.getBoolean();
    	assertEquals("boolean passed ok?",valueFalse, valueFromObject);
    }

}
