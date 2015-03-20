package org.openda.observers;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.openda.exchange.DoubleExchangeItem;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IObservationDescriptions;
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
	
    public Collection<IExchangeItem> getExchangeItems() {
        List<IExchangeItem> exchangeItems = new ArrayList<IExchangeItem>();
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
