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

	@Override
	public Role getRole() {
		return IPrevExchangeItem.Role.InOut;
	}

	@Override
	public String getId() {
		return id;
	}

	@Override
	public String getDescription() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitCFileExchangeItem - Method Name : getDescription");
	}

	@Override
	public Class getValueType() {
		return double[].class;
	}

	@Override
	public void copyValuesFromItem(IExchangeItem sourceItem) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitCFileExchangeItem - Method Name : copyValuesFromItem");
	}

	@Override
	public ITimeInfo getTimeInfo() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitCFileExchangeItem - Method Name : getTimeInfo");
	}

	@Override
	public IQuantityInfo getQuantityInfo() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitCFileExchangeItem - Method Name : getQuantityInfo");
	}

	@Override
	public IGeometryInfo getGeometryInfo() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitCFileExchangeItem - Method Name : getGeometryInfo");
	}

	@Override
	public ValueType getValuesType() {
		return ValueType.doublesType;
	}

	@Override
	public Object getValues() {
		return getValuesAsDoubles();
	}

	@Override
	public double[] getValuesAsDoubles() {
		return dupuitCFile.getHydraulicHead();
	}

	@Override
	public void axpyOnValues(double alpha, double[] axpyValues) {
		dupuitCFile.axpyOnHydraulicHead(alpha,axpyValues);
	}

	@Override
	public void multiplyValues(double[] multiplicationFactors) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitCFileExchangeItem - Method Name : multiplyValues");
	}

	@Override
	public void setValues(Object values) {
		if (!(values instanceof double[])) {
			throw new RuntimeException("SetValues for" + this.getId() + ": unexpected object type: " + values.getClass().getName());
		}
		setValuesAsDoubles((double[]) values);
	}

	@Override
	public void setValuesAsDoubles(double[] values) {
		dupuitCFile.setHydraulicHead(values);
	}

	@Override
	public double[] getTimes() {
		return this.dupuitCFile.getTimes();
	}

	@Override
	public void setTimes(double[] times) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitCFileExchangeItem - Method Name : setTimes");
	}
}
