package org.openda.exchange;

import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;
import org.openda.utils.MyObservable;

/**
 * Simple exchangeItem for String data. You can add a listener in eg the IDataObject
 * to take action on changes.
 * 
 * @author verlaanm
 *
 */
public class DoubleExchangeItem extends MyObservable implements IExchangeItem{
	private double value;
	private double time=Double.NaN;
	private String id;
	private String description="";
	private Role role=Role.InOut;
	private IQuantityInfo quantityInfo;
	
	
	public DoubleExchangeItem(String id, Role role, double value){
		this.value = value;
		this.role = role;
		this.id = id;
	}
	
	public DoubleExchangeItem(String id, double value){
		this(id,Role.InOut,value);
	}

	public DoubleExchangeItem(String id, Role role){
		this(id, role, 0);
	}
	
	
	public Role getRole() {
		return this.role;
	}

	
	public String getId() {
		return this.id;
	}

	
	public String getDescription() {
		return this.description;
	}
	
	public void setDescription(String description){
		this.description=description;
	}

	
	public void copyValuesFromItem(IExchangeItem sourceItem) {
		ValueType sourceType=sourceItem.getValuesType();
		if(sourceType==ValueType.doubleType){
			this.value=(Double)sourceItem.getValues();
			this.notifyObservers();
		}
	}

	
	public ITimeInfo getTimeInfo() {
		return new TimeInfo(new double[]{this.time});
	}

	
	public IQuantityInfo getQuantityInfo() {
		return this.quantityInfo;
	}
	
	public void setQuantityInfo(IQuantityInfo quantityInfo){
		this.quantityInfo=quantityInfo;
		this.notifyObservers();
	}

	
	public IGeometryInfo getGeometryInfo() {
		return null;
	}

	
	public ValueType getValuesType() {
		return ValueType.doubleType;
	}

	
	public Object getValues() {
		return this.value;
	}

	public void setValue(double value){
		this.value=value;
		this.notifyObservers();
	}
	
	public double getValue(){
		return this.value;
	}
	
	public double getTime(){
		return this.time;
	}
	
	public void setTime(double time){
		this.time=time;
		this.notifyObservers();
	}
	
	/*
	 * =======================================================================
	 * The following methods are due to the old interface and will be removed
	 * later.
	 * =======================================================================
	 */
	
	
	@SuppressWarnings("rawtypes")
	
	@Deprecated public Class getValueType() {
		return double.class;
	}

	
	@Deprecated public double[] getTimes() {
		return null;
	}

	
	@Deprecated public void setTimes(double[] times) {
		throw new UnsupportedOperationException("setTimes method make no sense for a DoubleExchangeItem");		
	}

	
	@Deprecated public void setValuesAsDoubles(double[] values) {
		if(values.length==1){
			this.value = values[0];
			this.notifyObservers();
		}else{
			throw new RuntimeException("Number of values can only be 1 for DoubleExchangeItem");
		}
	}

	
	@Deprecated public double[] getValuesAsDoubles() {
		return new double[]{this.value};
	}

	
	@Deprecated public void axpyOnValues(double alpha, double[] axpyValues) {
		if(axpyValues.length==1){
			this.value += alpha*axpyValues[0];
			this.notifyObservers();
		}else{
			throw new RuntimeException("Number of values can only be 1 for DoubleExchangeItem");
		}
	}

	
	@Deprecated public void multiplyValues(double[] multiplicationFactors) {
		if(multiplicationFactors.length==1){
			this.value *= multiplicationFactors[0];
			this.notifyObservers();
		}else{
			throw new RuntimeException("Number of values can only be 1 for DoubleExchangeItem");
		}
	}

	
	@Deprecated public void setValues(Object values) {
		if(values instanceof Double){
			this.value = (Double) values;
			this.notifyObservers();
		}else{
			throw new UnsupportedOperationException("Can only set values for a Double object");
		}
	}
	
	public String toString(){
		return "{"+this.id+"="+this.value+"}";
	}

}
