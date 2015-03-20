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
package org.openda.observers;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IObservationDescriptions;
import org.openda.interfaces.ITime;
import org.openda.interfaces.IVector;

public class GroupObservationDesrciptions implements IObservationDescriptions {

	//Class fields
	GroupStochObserver obs = null;

	public GroupObservationDesrciptions(){
		throw new RuntimeException("Please use observer.getObservationDescriptions to create GroupObservationDesrciptions");
	}

	public GroupObservationDesrciptions(GroupStochObserver obs){
       this.obs = obs;
	}


	/*
	 *  Querying an existing Group
	 */


	public String[] getIds(){
		return this.obs.getIds();
	}

	public IObservationDescriptions getChild(String id){
		IObservationDescriptions result=null;
		result=this.obs.getChild(id).getObservationDescriptions();
		return result;
	}

	public IObservationDescriptions getChild(int index){
		IObservationDescriptions result=null;
		result = this.obs.getChild(index).getObservationDescriptions();
		return result;
	}

	/*
	 *
	 * regular methods for ObservationDescriptions
	 *
	 */

    public List<IPrevExchangeItem> getExchangeItems() {
        List<IPrevExchangeItem> exchangeItems = new ArrayList<IPrevExchangeItem>();
        IVector vector = obs.getValues();
        for (int i = 0; i < obs.getIds().length; i++) {
            String id = obs.getIds()[i];
            exchangeItems.add(new DoubleExchangeItem(id, vector.getValue(i)));
        }
        return exchangeItems;
    }

	@Override
	public int getObservationCount() {
		return this.obs.getCount();
	}

	public ITime[] getTimes() {
		return obs.getTimes();
	}

	@Override
	public int getPropertyCount() {
		//TODO
		return 0;
	}

	@Override
	public String[] getPropertyKeys() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String[] getStringProperties(String Key) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public IVector getValueProperties(String Key) {
		// TODO Auto-generated method stub
		return null;
	}

	/**
     * Give string representation of the data
     * @return StochOsberver as a string
     */
    public String toString(){
    	String result = "ObservationDescriptions{";
    	result += this.obs.toString();
    	result += "\n}\n";
    	return result;
    }

}
