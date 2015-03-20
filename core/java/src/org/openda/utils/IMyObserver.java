package org.openda.utils;

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
public interface IMyObserver {

	/**
	 * This method is called when a class that implements IMyObservable calls
	 * notifyObservers and this class has been added as an observer.
	 * @param observable instance that signals a change.
	 * @param arg not used. Will return null for now.
	 */
	public void update(IMyObservable observable, Object arg);
}
