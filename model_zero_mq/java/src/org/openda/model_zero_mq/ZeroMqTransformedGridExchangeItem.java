package org.openda.model_zero_mq;

import org.openda.exchange.ArrayGeometryInfo;
import org.openda.exchange.QuantityInfo;
import org.openda.exchange.TimeInfo;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;

import java.util.Arrays;

public class ZeroMqTransformedGridExchangeItem implements IExchangeItem {
	private final String variable;
	private final ArrayGeometryInfo arrayGeometryInfo;
	private final int[] latitudeIndices;
	private final int[] longitudeIndices;
	private final QuantityInfo quantityInfo;
	private final ZeroMqModelInstance model;
	private final double[] values;
	private final double modelMissingValue;
	private final int longitudeLength;
	private final boolean passViaZeroMq;
	private final double[] dummyValuesArray;

	public ZeroMqTransformedGridExchangeItem(String variable, ArrayGeometryInfo arrayGeometryInfo, int[] latitudeIndices, int[] longitudeIndices, QuantityInfo quantityInfo, ZeroMqModelInstance zeroMqModelInstance, double modelMissingValue, int longitudeLength, boolean passViaZeroMq) {
		this.variable = variable;
		this.arrayGeometryInfo = arrayGeometryInfo;
		this.latitudeIndices = latitudeIndices;
		this.longitudeIndices = longitudeIndices;
		this.quantityInfo = quantityInfo;
		this.model = zeroMqModelInstance;
		values = new double[arrayGeometryInfo.getCellCount()];
		this.modelMissingValue = modelMissingValue;
		this.longitudeLength = longitudeLength;
		this.passViaZeroMq = passViaZeroMq;
		Arrays.fill(values, Double.NaN);
		int bytesPerItem = this.model.getVarItemSize(variable);
		int totalBytes = this.model.getVarNBytes(variable);
		dummyValuesArray = new double[totalBytes / bytesPerItem];
		getValuesAsDoubles();
	}

	@Override
	public Role getRole() {
		throw new RuntimeException("org.openda.model_zero_mq.ZeroMqAnalysisOutputExchangeItem.getRole() not implemented yet");
	}

	@Override
	public String getId() {
		return variable;
	}

	@Override
	public String getDescription() {
		throw new RuntimeException("org.openda.model_zero_mq.ZeroMqAnalysisOutputExchangeItem.getDescription() not implemented yet");
	}

	@Override
	public void copyValuesFromItem(IExchangeItem sourceItem) {
		throw new RuntimeException("org.openda.model_zero_mq.ZeroMqAnalysisOutputExchangeItem.copyValuesFromItem() not implemented yet");
	}

	public ITimeInfo getTimeInfo() {
		return new TimeInfo(getTimes());
	}

	@Override
	public IQuantityInfo getQuantityInfo() {
		return quantityInfo;
	}

	@Override
	public IGeometryInfo getGeometryInfo() {
		return arrayGeometryInfo;
	}

	@Override
	public ValueType getValuesType() {
		return IExchangeItem.ValueType.IVectorType;
	}

	@Override
	public Object getValues() {
		throw new RuntimeException("org.openda.model_zero_mq.ZeroMqAnalysisOutputExchangeItem.getValues() not implemented yet");
	}

	@Override
	public double[] getValuesAsDoubles() {
		double[] allModelValues = model.getValues(variable, dummyValuesArray);
		for (int i = 0; i < allModelValues.length; i++) {
			this.values[latitudeIndices[i] * longitudeLength + longitudeIndices[i]] = Double.compare(allModelValues[i], modelMissingValue) == 0 ? Double.NaN : allModelValues[i];
		}
		return values;
	}

	@Override
	public void axpyOnValues(double alpha, double[] axpyValues) {
		for (int i = 0; i < values.length; i++) {
			values[i] += alpha * axpyValues[i];
		}
		if (!passViaZeroMq) return;
		for (int i = 0; i < longitudeLength; i++) {
			dummyValuesArray[i] = this.values[latitudeIndices[i] * longitudeLength + longitudeIndices[i]];
		}
		model.setValue(variable, dummyValuesArray);
	}

	@Override
	public void multiplyValues(double[] multiplicationFactors) {
		for (int i = 0; i < multiplicationFactors.length; i++) {
			this.values[i] *= multiplicationFactors[i];
		}
		if (!passViaZeroMq) return;
		for (int i = 0; i < longitudeLength; i++) {
			dummyValuesArray[i] = this.values[latitudeIndices[i] * longitudeLength + longitudeIndices[i]];
		}
		model.setValue(variable, dummyValuesArray);
	}

	@Override
	public void setValues(Object values) {
		throw new RuntimeException("org.openda.model_zero_mq.ZeroMqAnalysisOutputExchangeItem.setValues() not implemented yet");
	}

	@Override
	public void setValuesAsDoubles(double[] setValues) {
		for (int i = 0; i < setValues.length; i++) {
			this.values[latitudeIndices[i] * longitudeLength + longitudeIndices[i]] = setValues[i];
		}
		if (!passViaZeroMq) return;
		for (int i = 0; i < longitudeLength; i++) {
			dummyValuesArray[i] = this.values[latitudeIndices[i] * longitudeLength + longitudeIndices[i]];
		}
		model.setValue(variable, dummyValuesArray);
	}

	public double[] getTimes() {
		return new double[]{model.getCurrentTime().getMJD()};
	}

	@Override
	public void setTimes(double[] times) {
		throw new RuntimeException("org.openda.model_zero_mq.ZeroMqAnalysisOutputExchangeItem.setTimes() not implemented yet");
	}
}
