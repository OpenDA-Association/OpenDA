package org.openda.model_damflow;

import org.openda.interfaces.*;

/**
 * Created by IntelliJ IDEA.
 * User: Julius Sumihar
 * Date: 2-7-12
 * Time: 10:24
 * To change this template use File | Settings | File Templates.
 */
public class DupuitCFileExchangeItem implements IExchangeItem {
	private String id;
	private DupuitCFile dupuitCFile;

	public DupuitCFileExchangeItem(String id, DupuitCFile dupuitCFile) {
		this.id = id;
		this.dupuitCFile = dupuitCFile;
	}

	
	public Role getRole() {
		return IPrevExchangeItem.Role.InOut;
	}

	
	public String getId() {
		return id;
	}

	
	public String getDescription() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitCFileExchangeItem - Method Name : getDescription");
	}

	
	public Class getValueType() {
		return double[].class;
	}

	
	public void copyValuesFromItem(IExchangeItem sourceItem) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitCFileExchangeItem - Method Name : copyValuesFromItem");
	}

	
	public ITimeInfo getTimeInfo() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitCFileExchangeItem - Method Name : getTimeInfo");
	}

	
	public IQuantityInfo getQuantityInfo() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitCFileExchangeItem - Method Name : getQuantityInfo");
	}

	
	public IGeometryInfo getGeometryInfo() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitCFileExchangeItem - Method Name : getGeometryInfo");
	}

	
	public ValueType getValuesType() {
		return ValueType.doublesType;
	}

	
	public Object getValues() {
		return getValuesAsDoubles();
	}

	
	public double[] getValuesAsDoubles() {
		return dupuitCFile.getHydraulicHead();
	}

	
	public void axpyOnValues(double alpha, double[] axpyValues) {
		dupuitCFile.axpyOnHydraulicHead(alpha,axpyValues);
	}

	
	public void multiplyValues(double[] multiplicationFactors) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitCFileExchangeItem - Method Name : multiplyValues");
	}

	
	public void setValues(Object values) {
		if (!(values instanceof double[])) {
			throw new RuntimeException("SetValues for" + this.getId() + ": unexpected object type: " + values.getClass().getName());
		}
		setValuesAsDoubles((double[]) values);
	}

	
	public void setValuesAsDoubles(double[] values) {
		dupuitCFile.setHydraulicHead(values);
	}

	
	public double[] getTimes() {
		return this.dupuitCFile.getTimes();
	}

	
	public void setTimes(double[] times) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitCFileExchangeItem - Method Name : setTimes");
	}
}
