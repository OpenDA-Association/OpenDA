/* OpenDA v2.4.3 
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

public class MyOberverTest extends TestCase{

    public static void testMyObserver() {
    	System.out.println("========================================================");
    	System.out.println(" Test for MyObserver MyObservable");
    	System.out.println("========================================================");

    	class MyObserver implements IMyObserver{
    		public boolean notified=false;
    		public void update(IMyObservable o, Object arg){
    			this.notified=true;
    		}
    	}
    	
    	MyObservable observable = new MyObservable();
    	MyObserver observer1 = new MyObserver();
    	MyObserver observer2 = new MyObserver();
    	observable.addObserver(observer1);
    	
    	assertEquals(false, observer1.notified);
    	assertEquals(false, observer2.notified);
    	observable.notifyObservers(); //This should trigger notification to observer1
    	assertEquals(true, observer1.notified);
    	assertEquals(false, observer2.notified);
    	observer1.notified=false; //reset for next test
    	observable.addObserver(observer2); // add second observer
    	observable.notifyObservers(); //Trigger notofication again, but now to both
    	assertEquals(true, observer1.notified);
    	assertEquals(true, observer2.notified);    	
    	
    }
}
