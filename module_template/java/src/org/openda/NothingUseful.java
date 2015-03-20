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
public class NothingUseful {
	
	double aDouble=1.0;
	boolean test=false;
	
	public double getDouble(){
		return this.aDouble;
	}
	
	public void setDouble(double newValue){
		this.aDouble=newValue;
	}

	public boolean getBoolean(){
		return this.test;
	}
	
	public void setBoolean(boolean newBool){
		this.test=newBool;
	}
	
	public static void main(String [] args) {
		NothingUseful myObject = new NothingUseful();
		myObject.setDouble(3.14);
		double myDouble = myObject.getDouble();
		System.out.println("pi is close to "+myDouble);
		
		myObject.setBoolean(true);
		boolean workingTest = myObject.getBoolean();
		System.out.println("This test is working? >"+workingTest);
		
	}
}
