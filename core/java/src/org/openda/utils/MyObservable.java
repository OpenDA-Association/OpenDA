package org.openda.utils;

import java.util.ArrayList;

/**
 * Subset of java.util.Observable, Observer methods. These classes in java annoyingly do not implement 
 * interfaces, so they can not be used when a class must extend another class, because java
 * does not support multiple inheritance
 * 
 * The two interfaces IMyObservable and IMyObserver together implement the observer-pattern.
 * Standard implementations MyObservable and MyObserver can be used to simplify implementation.
 * 
 * Intro: The observer pattern implements a call-bck mechanism to notify other classes when something 
 * has happened. This avoids frequent polling (asking if something has happened) and is more modular. 
 * 
 * @author verlaanm
 *
 */
public class MyObservable implements IMyObservable{
	ArrayList<IMyObserver> observers = new ArrayList<IMyObserver>();
	
	@Override
	public void addObserver(IMyObserver observer) {
		this.observers.add(observer);
	}

	@Override
	public void notifyObservers() {
		for(IMyObserver o : this.observers){
			o.update(this, null);
		}
	}

	
}
