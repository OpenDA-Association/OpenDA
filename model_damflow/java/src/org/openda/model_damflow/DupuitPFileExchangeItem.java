package org.openda.model_damflow;

import org.openda.interfaces.*;

/**
 * Created by IntelliJ IDEA.
 * User: Julius Sumihar
 * Date: 2-7-12
 * Time: 14:47
 * To change this template use File | Settings | File Templates.
 */
public class DupuitPFileExchangeItem implements IExchangeItem {
	private DupuitPFile dupuitPFile;
	private String id;
	private String[] availableIDs = new String[]{"sTime","eTime","sPolder"};

	public DupuitPFileExchangeItem(String id, DupuitPFile dupuitPFile) {
		this.id = id;
		this.dupuitPFile = dupuitPFile;
	}

	@Override
	public Role getRole() {
		return IPrevExchangeItem.Role.InOut;
	}

	@Override
	public String getId() {
		return this.id;
	}

	@Override
	public String getDescription() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitPFileExchangeItem - Method Name : getDescription");
	}

	@Override
	public Class getValueType() {
		return double.class;
	}

	@Override
	public void copyValuesFromItem(IExchangeItem sourceItem) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitPFileExchangeItem - Method Name : copyValuesFromItem");
	}

	@Override
	public ITimeInfo getTimeInfo() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitPFileExchangeItem - Method Name : getTimeInfo");
	}

	@Override
	public IQuantityInfo getQuantityInfo() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitPFileExchangeItem - Method Name : getQuantityInfo");
	}

	@Override
	public IGeometryInfo getGeometryInfo() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitPFileExchangeItem - Method Name : getGeometryInfo");
	}

	@Override
	public ValueType getValuesType() {
		return ValueType.doubleType;
	}

	@Override
	public Object getValues() {
		double value;
		if (this.id.equals(availableIDs[0])) {
			value = dupuitPFile.getSTime();
		} else if (this.id.equals(availableIDs[1])){
			value = dupuitPFile.getETime();
		} else {
			value = dupuitPFile.getSPolder();
		}
		return value;
	}

	@Override
	public double[] getValuesAsDoubles() {
        return new double[]{(Double)getValues()};
	}

	@Override
	public void axpyOnValues(double alpha, double[] axpyValues) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitPFileExchangeItem - Method Name : axpyOnValues");
	}

	@Override
	public void multiplyValues(double[] multiplicationFactors) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitPFileExchangeItem - Method Name : multiplyValues");
	}

	@Override
	public void setValues(Object values) {
		if (this.id.equals(availableIDs[0])){
			this.dupuitPFile.setSTime((Double)values);
		} else if (this.id.equals(availableIDs[1])){
			this.dupuitPFile.setETime((Double)values);
		} else if (this.id.equals(availableIDs[2])){
			this.dupuitPFile.setSPolder((Double)values);
		}
	}

	@Override
	public void setValuesAsDoubles(double[] values) {
		if (this.id.equals(availableIDs[0])){
			this.dupuitPFile.setSTime((Double)values[0]);
		} else if (this.id.equals(availableIDs[1])){
			this.dupuitPFile.setETime((Double)values[0]);
		} else if (this.id.equals(availableIDs[2])){
			this.dupuitPFile.setSPolder((Double)values[0]);
		}
	}

	@Override
	public double[] getTimes() {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitPFileExchangeItem - Method Name : getTimes");
	}

	@Override
	public void setTimes(double[] times) {
		throw new UnsupportedOperationException("Class Name : org.openda.model_damflow.DupuitPFileExchangeItem - Method Name : setTimes");
	}
}
