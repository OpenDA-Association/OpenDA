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
	
	
	public void addObserver(IMyObserver observer) {
		this.observers.add(observer);
	}

	
	public void notifyObservers() {
		for(IMyObserver o : this.observers){
			o.update(this, null);
		}
	}

	
}
