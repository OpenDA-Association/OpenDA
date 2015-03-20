package org.openda.observers;
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

import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.ITime;
import org.openda.interfaces.IVector;

import java.io.Serializable;
import java.util.List;

/**
 * This is an implementation of a IObservationDescriptions that is serializable
 * This implementation is used to contain and serialize the content of not serializable
 * implementations of IObservationDescriptions is not serializable.
 *
 * The constructor will simply copies all content of the non serializable into the
 * SerializableObservationDescriptions
 *
 * Note the exchange items of the original IObservationDescriptions are assumed to be serializable!
 *
 * @author Nils van Velzen
 *
 */

public class SerializableObservationDescriptions implements IObservationDescriptions, Serializable {

	IVector[] allPropertiesVec;
	String[][] allPropertiesStr;
	String[] propertyKeys;
	int propertyCount;
	int observationCount;
	ITime[] times;
	List<IPrevExchangeItem> exchangeItems;


	/**
	 * Create a SerializableObservationDescriptions containing a copy of all key value pairs of the given
	 * IObservationDescriptions
	 * @param origin ObservationDescriptions instance which is probably not serializable and from which we will copy all key-value pairs into this IObservationDescriptions)
	 */
	public SerializableObservationDescriptions(IObservationDescriptions origin){
		try{
		   propertyKeys     = origin.getPropertyKeys();
		} catch (RuntimeException e){
			System.out.print("Debug, Observer does not implement getPropertyKeys");
		}
		try{
			propertyCount    = origin.getPropertyCount();
		} catch (RuntimeException e){
			System.out.print("Debug, Observer does not implement getPropertyCount");
		}

		try{
		    exchangeItems = origin.getExchangeItems();
		} catch (RuntimeException e){
			System.out.print("Debug, Observer does not implement getExchangeItems");
		}

		observationCount = origin.getObservationCount();
		times            = origin.getTimes();

		allPropertiesVec = new IVector[propertyCount];
		allPropertiesStr = new String[propertyCount][observationCount];


	    for (int iProperty=0;iProperty<propertyCount;iProperty++){
			/* First try to get the property as a IVector */
			try{
				allPropertiesVec[iProperty]=origin.getValueProperties(propertyKeys[iProperty]);
			}
			/* Fallback get the value as a string */
			catch(RuntimeException e) {
				String[] propertyStr=origin.getStringProperties(propertyKeys[iProperty]);
				for (int iObs=0; iObs<observationCount; iObs++){
					allPropertiesStr[iProperty][iObs]=propertyStr[iObs];
				}
			}
		}
	}


	public List<IPrevExchangeItem> getExchangeItems() {
	    return exchangeItems;
	}

	public IVector getValueProperties(String key) {
		int iProperty = propertyIndex(key);
		return allPropertiesVec[iProperty];
	}

	public String[] getStringProperties(String key) {
		int iProperty = propertyIndex(key);
		String[] properties = new String[propertyCount];

	    /* Check whether the properties are stored as array of strings or as vector */
		if (allPropertiesVec[iProperty] == null){
			/* -stored as string, just fill return vector */
			for (int iObs=0; iObs<observationCount; iObs++){
				properties[iObs]=allPropertiesStr[iProperty][iObs];
			}
		}
		else {
			/* -stored as vector, first extract values and convert them to string */
			double[] propertyValues=allPropertiesVec[iProperty].getValues();
			for (int iObs=0; iObs<observationCount; iObs++){
				properties[iObs]=properties[iObs].toString();
			}
		}
		return  properties;
	}

	public String[] getPropertyKeys() {
		return propertyKeys;
	}

	public int getPropertyCount() {
		return propertyCount;
	}

	public int getObservationCount() {
		return observationCount;
	}

	public ITime[] getTimes() {
		return times;
	}

	private int propertyIndex(String key){
		boolean found=false;
		int iProperty;
		for (iProperty=0; iProperty<propertyCount && ! found; iProperty++){
			found=key.equals(propertyKeys[iProperty]);
		}
		iProperty--;
		if (!found){
		    String msg="The specified key("+key+") is not available in this ObservationDescriptions instance\n"+
					   "The available keys are:"+propertyKeys;
			throw new RuntimeException(msg);
		}
		return iProperty;
	}

}
