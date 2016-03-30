package org.openda.model_bmi;

import bmi.BMIModelException;
import bmi.EBMI;
import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.*;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by bos_en on 3/25/2016.
 */
public class BmiStateExchangeItem implements IExchangeItem {

	private final String[] ids;
	private final EBMI model;
	private double modelMissingValue;
	private Map<String, Integer> stateComponentLengths;
	private int totalNumberOfStateValues;
	private double[] values;

	public BmiStateExchangeItem(String[] ids, EBMI model, double modelMissingValue) {
		this.ids = ids;
		this.model = model;
		this.modelMissingValue = modelMissingValue;

		totalNumberOfStateValues = 0;
		stateComponentLengths = new HashMap<String, Integer>();
		for (String id: this.ids) {
			try {
				int length = this.model.getVarSize(id);
				stateComponentLengths.put(id, length);
				totalNumberOfStateValues += length;
			} catch (BMIModelException e) {
				throw new RuntimeException("org.openda.model_bmi.BmiStateExchangeItem.BmiStateExchangeItem() Bmi model does not know variable: " + id);
			}
		}
		values = new double[totalNumberOfStateValues];
	}

	public Role getRole() {
		return Role.InOut;
	}

	public String getId() {
		return "state";
	}

	public String getDescription() {
		return "Bmi state exchangeItem.";
	}

	public Class getValueType() {
		return double[].class;
	}

	public void copyValuesFromItem(IExchangeItem sourceItem) {
		throw new RuntimeException("org.openda.model_bmi.BmiStateExchangeItem.copyValuesFromItem() not implemented yet");
	}

	public ITimeInfo getTimeInfo() {
		return null;
	}

	public IQuantityInfo getQuantityInfo() {
		throw new RuntimeException("org.openda.model_bmi.BmiStateExchangeItem.getQuantityInfo() not implemented yet");
	}

	public IGeometryInfo getGeometryInfo() {
		throw new RuntimeException("org.openda.model_bmi.BmiStateExchangeItem.getGeometryInfo() not implemented yet");
	}

	public ValueType getValuesType() {
		return ValueType.doublesType;
	}

	public Object getValues() {
		return getValuesAsDoubles();
	}

	public double[] getValuesAsDoubles() {
		int offset = 0;

		double[] allModelValues = new double[values.length];
		for (String id: this.ids) {

			double[] modelValues;
			try {
				if ("float32".equals(model.getVarType(id))) {
					modelValues = BBUtils.toDoubleArray(model.getFloat(id));
				} else if ("float64".equals(model.getVarType(id))) {
					modelValues = model.getDouble(id);
				} else {
					throw new BMIModelException("unsupported variable data type: " + model.getVarType(id) + " currently only float and double types supported");
				}
			} catch (BMIModelException e) {
				throw new RuntimeException(e);
			}

			int numberOfValues = modelValues.length;
			System.arraycopy(modelValues, 0, allModelValues, offset, numberOfValues);

			offset += numberOfValues;
		}
		for (int i = 0; i < allModelValues.length; i++) {
			values[i] = Double.compare(allModelValues[i], modelMissingValue) == 0 ? Double.NaN : allModelValues[i];
		}

		return values;
	}

	private void updateModel() {
		int offset = 0;

		double[] checkedValues = new double[values.length];
		for (int i = 0; i < values.length; i++) {
			checkedValues[i] = Double.isNaN(values[i]) ? modelMissingValue: values[i];
		}

		for (String id: this.ids) {
			int sliceLength = stateComponentLengths.get(id);
			double[] slice = new double[sliceLength];
			System.arraycopy(checkedValues, offset, slice, 0, sliceLength);

			try {
				if ("float32".equals(model.getVarType(id))) {
					model.setFloat(id, BBUtils.toFloatArray(slice));
				} else if ("float64".equals(model.getVarType(id))) {
					model.setDouble(id, slice);
				} else {
					throw new BMIModelException("unsupported variable data type: " + model.getVarType(id) + " currently only float and double types supported");
				}
			} catch (BMIModelException e) {
				throw new RuntimeException(e);
			}
			offset += sliceLength;
		}
	}

	public void axpyOnValues(double alpha, double[] axpyValues) {
		if (axpyValues.length != totalNumberOfStateValues) {
			throw new RuntimeException("org.openda.model_bmi.BmiStateExchangeItem.axpyOnValues() incoming array is of wrong size");
		}

		for (int n = 0; n < values.length; n++) {
			values[n] += alpha * axpyValues[n];
		}

		updateModel();
	}

	public void multiplyValues(double[] multiplicationFactors) {
		if (multiplicationFactors.length != totalNumberOfStateValues) {
			throw new RuntimeException("org.openda.model_bmi.BmiStateExchangeItem.multiplyValues() incoming array is of wrong size");
		}

		for (int n = 0; n < values.length; n++) {
			values[n] *= multiplicationFactors[n];
		}

		updateModel();
	}

	public void setValues(Object values) {
		if (!(values instanceof double[])){
			throw new RuntimeException("org.openda.model_bmi.BmiStateExchangeItem.setValues() wrong type");
		}
		setValuesAsDoubles((double[])values);
	}

	public void setValuesAsDoubles(double[] values) {
		throw new RuntimeException("org.openda.model_bmi.BmiStateExchangeItem.setValuesAsDoubles() not implemented yet");
	}

	public double[] getTimes() {
		return null;
	}

	public void setTimes(double[] times) {
		throw new RuntimeException("org.openda.model_bmi.BmiStateExchangeItem.setTimes() not implemented yet");
	}
}
