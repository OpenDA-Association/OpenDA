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
package org.openda.model_bmi;
import bmi.BMIModelException;
import bmi.EBMI;
import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;

import java.util.ArrayList;
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
	private ArrayList<Integer> stateValuesRanges;
	private double[][] lowerLimits;
	private double[][] upperLimits;
	private double[] values;

	public BmiStateExchangeItem(String[] ids, double[][] lowerLimits, double[][] upperLimits, EBMI model, double modelMissingValue) {
		this.ids = ids;
		this.lowerLimits = lowerLimits;
		this.upperLimits = upperLimits;
		this.model = model;
		this.modelMissingValue = modelMissingValue;

		totalNumberOfStateValues = 0;
		stateComponentLengths = new HashMap<String, Integer>();
		stateValuesRanges = new ArrayList<Integer>();
		for (String id : this.ids) {
			try {
				int length = this.model.getVarSize(id);
				stateComponentLengths.put(id, length);
				totalNumberOfStateValues += length;
				stateValuesRanges.add(totalNumberOfStateValues);
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

		// Replace occurences of OpenDA's missing value by the Bmi model's missing value.
		double[] checkedValues = new double[values.length];
		for (int i = 0; i < values.length; i++) {
			checkedValues[i] = Double.isNaN(values[i]) ? modelMissingValue: values[i];
		}

		// Apply lower and upper limits to the values which OpenDA will send to the Bmi model.
		int idx1 = 0;  // slice start index
		int idx2;      // slice stop index
		for (int i = 0; i < stateValuesRanges.size(); i++) {
			idx2 = stateValuesRanges.get(i);
			for (int j = idx1; j < idx2; j++) {
				double lowerLimit = lowerLimits[i].length == 1 ? lowerLimits[i][0] : lowerLimits[i][j];
				if (checkedValues[j] != modelMissingValue && !Double.isNaN(lowerLimit)) {
					checkedValues[j] = Math.max(checkedValues[j], lowerLimit);
				}
			}
			for (int j = idx1; j < idx2; j++) {
				double upperLimit = upperLimits[i].length == 1 ? upperLimits[i][0] : upperLimits[i][j];
				if (checkedValues[j] != modelMissingValue && !Double.isNaN(upperLimit)) {
					checkedValues[j] = Math.min(checkedValues[j], upperLimit);
				}
			}
			idx1 = idx2;
		}

		// Each State variable has its values stored in a slice of OpenDA's total values array. Feed the Bmi model slices.
		int offset = 0;
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
