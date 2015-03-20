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
