package org.openda.model_zero_mq;

import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

public class ZeroMqStateExchangeItem implements IExchangeItem {

	private final String[] ids;
	private final ZeroMqModelInstance model;
	private final double modelMissingValue;
	private final Map<String, Integer> stateComponentLengths;
	private int totalNumberOfStateValues;
	private final ArrayList<Integer> stateValuesRanges;
	private final double[][] lowerLimits;
	private final double[][] upperLimits;
	private final double[] values;

	public ZeroMqStateExchangeItem(String[] ids, double[][] lowerLimits, double[][] upperLimits, ZeroMqModelInstance model, double modelMissingValue) {
		this.ids = ids;
		this.lowerLimits = lowerLimits;
		this.upperLimits = upperLimits;
		this.model = model;
		this.modelMissingValue = modelMissingValue;

		totalNumberOfStateValues = 0;
		stateComponentLengths = new HashMap<>();
		stateValuesRanges = new ArrayList<>();
		for (String id : this.ids) {
			int bytesPerItem = this.model.getVarItemSize(id);
			int totalBytes = this.model.getVarNBytes(id);
			int numberOfItems = totalBytes / bytesPerItem;
			stateComponentLengths.put(id, numberOfItems);
			totalNumberOfStateValues += numberOfItems;
			stateValuesRanges.add(totalNumberOfStateValues);
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
		for (String id : this.ids) {
			double[] modelValues = model.getValues(id, new double[stateComponentLengths.get(id)]);
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
		// Replace occurrences of OpenDA's missing value by the Bmi model's missing value.
		double[] checkedValues = new double[values.length];
		for (int i = 0; i < values.length; i++) {
			checkedValues[i] = Double.isNaN(values[i]) ? modelMissingValue : values[i];
		}

		// Apply lower and upper limits to the values which OpenDA will send to the Bmi model.
		limitCheckedValues(checkedValues);

		// Each State variable has its values stored in a slice of OpenDA's total values array. Feed the Bmi model slices.
		feedSlicesToBmiModel(checkedValues);
	}

	private void limitCheckedValues(double[] checkedValues) {
		int idx1 = 0;  // slice start index
		for (int i = 0; i < stateValuesRanges.size(); i++) {
			int idx2 = stateValuesRanges.get(i); // slice stop index
			for (int j = idx1; j < idx2; j++) {
				limitCheckedValue(checkedValues, i, j);
			}
			idx1 = idx2;
		}
	}

	private void limitCheckedValue(double[] checkedValues, int i, int j) {
		double lowerLimit = lowerLimits[i].length == 1 ? lowerLimits[i][0] : lowerLimits[i][j];
		double upperLimit = upperLimits[i].length == 1 ? upperLimits[i][0] : upperLimits[i][j];
		if (checkedValues[j] != modelMissingValue && !Double.isNaN(lowerLimit)) {
			checkedValues[j] = Math.max(checkedValues[j], lowerLimit);
		}
		if (checkedValues[j] != modelMissingValue && !Double.isNaN(upperLimit)) {
			checkedValues[j] = Math.min(checkedValues[j], upperLimit);
		}
	}

	private void feedSlicesToBmiModel(double[] checkedValues) {
		int offset = 0;
		for (String id : this.ids) {
			int sliceLength = stateComponentLengths.get(id);
			double[] slice = new double[sliceLength];
			System.arraycopy(checkedValues, offset, slice, 0, sliceLength);

			model.setValue(id, slice);

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
		if (!(values instanceof double[])) {
			throw new RuntimeException("org.openda.model_bmi.BmiStateExchangeItem.setValues() wrong type");
		}
		setValuesAsDoubles((double[]) values);
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
