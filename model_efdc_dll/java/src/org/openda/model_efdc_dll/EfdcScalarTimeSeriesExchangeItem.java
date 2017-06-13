/* OpenDA v2.4 
* Copyright (c) 2017 OpenDA Association 
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
package org.openda.model_efdc_dll;

import org.openda.exchange.TimeInfo;
import org.openda.interfaces.*;
import org.openda.utils.Time;
import org.openda.utils.Vector;

import java.util.Calendar;
import java.util.TimeZone;

/**
 * Exchange item representing values for a time series for a single location, a single parameter and a single layer
 * that are stored in the dll version of the EFDC model.
 *
 * @author Arno Kockx
 */
@SuppressWarnings("serial")
public class EfdcScalarTimeSeriesExchangeItem implements IExchangeItem {

	private final String id;
	/**
	 * Integer that corresponds to a certain location within the efdc model.
	 */
	private final int locationNumber;
	/**
	 * Integer that corresponds to a certain parameter within the efdc model.
	 */
	private final int parameterNumber;
	/**
	 * Integer that corresponds to a certain layer within the efdc model.
	 */
	private final int layerNumber; // integer that corresponds to layer (1=bottom layer)
	private final Role role;
	private final EfdcDLL modelDll;

	/**
	 * @param locationNumber
	 * @param parameterNumber
	 * @param layerNumber can be null if no layers.
	 * @param parameterId
	 * @param role
	 * @param modelDll
	 */
	public EfdcScalarTimeSeriesExchangeItem(int locationNumber, int parameterNumber, Integer layerNumber, String parameterId, Role role, EfdcDLL modelDll) {
		//id = "locationNumber_layerNumber.parameterId"
		//id = "locationNumber.parameterId" for a model with one layer
		if (layerNumber == null) { // one layer only model
			this.id = locationNumber + "." + parameterId;
			this.layerNumber = 1;
		} else {//if multiple layers.
			this.id = locationNumber + "_layer" + layerNumber + "." + parameterId;
			this.layerNumber = layerNumber;
		}
		this.locationNumber = locationNumber;
		this.parameterNumber = parameterNumber;
		this.role = role;
		this.modelDll = modelDll;
	}

	public String getId() {
		return this.id;
	}

	public String getDescription() {
		return null;
	}

	public Role getRole() {
		return this.role;
	}

	public ITimeInfo getTimeInfo() {
		return new TimeInfo(this.modelDll.getTimesForExchangeItem(this.parameterNumber, this.locationNumber));
	}

	//TODO this method is only present for backwards compatibility. This method should be removed
	//once all exchange items have been migrated to the new IExchangeItem approach. AK
	@Deprecated
	public double[] getTimes() {
		return getTimeInfo().getTimes();
	}

	//TODO this method is only present for backwards compatibility. This method should be removed
	//once all exchange items have been migrated to the new IExchangeItem approach. AK
	@Deprecated
	public void setTimes(double[] times) {
		throw new UnsupportedOperationException(getClass().getSimpleName() + ": setTimes not implemented.");
	}

	void setTimesForUnitTest(double[] times) {
		this.modelDll.setTimesForExchangeItem(this.parameterNumber, this.locationNumber, times);
	}

	public IQuantityInfo getQuantityInfo() {
		throw new UnsupportedOperationException(getClass().getSimpleName() + ": getQuantityInfo not implemented.");
	}

	public IGeometryInfo getGeometryInfo() {
		return null;
	}

	public ValueType getValuesType() {
		return ValueType.IVectorType;
	}

	public Class<?> getValueType() {
		return IVector.class;
	}

	/**
	 * Returns all values for this scalar time series.
	 */
	public Object getValues() {
		return new Vector(getValuesAsDoubles());
	}

	/**
	 * Returns all values for this scalar time series.
	 */
	public double[] getValuesAsDoubles() {
		double[] times = getTimeInfo().getTimes();
		ITime firstTime = new Time(times[0]);
		ITime lastTime = new Time(times[times.length - 1]);
		return this.modelDll.getValues(this.parameterNumber, this.locationNumber, this.layerNumber, firstTime, lastTime);
	}

	public void axpyOnValues(double alpha, double[] axpyValues) {
		double[] values = getValuesAsDoubles();
		if (axpyValues.length != values.length) {
			throw new IllegalStateException(getClass().getSimpleName() + ": axpyValues.length (" + axpyValues.length
					+ ") must equal valueCount (" + values.length + ") for variable " + this.id + ".");
		}

		for (int n = 0; n < values.length; n++) {
			values[n] += alpha * axpyValues[n];
		}
		setValuesAsDoubles(values);
	}

	public void multiplyValues(double[] multiplicationFactors) {
		double[] values = getValuesAsDoubles();
		if (multiplicationFactors.length != values.length) {
			throw new IllegalStateException(getClass().getSimpleName() + ": multiplicationFactors.length (" + multiplicationFactors.length
					+ ") must equal valueCount (" + values.length + ") for variable " + this.id + ".");
		}

		for (int n = 0; n < values.length; n++) {
			values[n] *= multiplicationFactors[n];
		}
		setValuesAsDoubles(values);
	}

	/**
	 * Sets all values for this scalar time series.
	 */
	public void setValues(Object vector) {
		if (!(vector instanceof IVector)) {
			throw new IllegalArgumentException(getClass().getSimpleName() + ": supply values as an IVector not as " + vector.getClass().getSimpleName());
		}

		setValuesAsDoubles(((IVector) vector).getValues());
	}

	/**
	 * Sets all values for this scalar time series.
	 */
	public void setValuesAsDoubles(double[] values) {
		//Note: if source item is a NetcdfScalarTimeSeriesExchangeItem, then missing values have been filtered out already at this point.
		//In that case can only check that number of values equals number of times and that there is at least one non-missing value present, i.e. check fails only if all values are missing.
		if (values == null || values.length <= 0) {
			throw new RuntimeException(getClass().getSimpleName() + ": no input values supplied (only missing values found) for time series with id '" + getId() + "'.");
		}
		double[] times = getTimeInfo().getTimes();
		if (values.length != times.length ) {
			throw new RuntimeException(getClass().getSimpleName() + ": number of input values (" + values.length
					+ ") should be equal to number of times (" + times.length + ") for time series with id '" + getId() + "'.");
		}
		for (int n = 0; n < values.length; n++) {
			if (Double.isNaN(values[n])) {
				//throw an exception if boundary or forcing values are missing values.
				Calendar calendar = Calendar.getInstance();
				calendar.setTimeZone(TimeZone.getTimeZone("GMT"));
				calendar.setTimeInMillis(Time.mjdToMillies(times[n]));
				throw new RuntimeException(getClass().getSimpleName() + ": Missing input value at " + calendar.getTime().toString() + " for time series with id '" + getId()
						+ "' as input for the model. The EFDC model cannot handle missing values within the model run period. Please check if the input data is correct.");
			}
		}

		ITime firstTime = new Time(times[0]);
		ITime lastTime = new Time(times[times.length - 1]);
		this.modelDll.setValues(this.parameterNumber, values, this.locationNumber, this.layerNumber , firstTime, lastTime);
	}

	/**
	 * This method reads a scalar time series from the given sourceItem
	 * and stores all times and values in this exchangeItem.
	 */
	public void copyValuesFromItem(IExchangeItem sourceItem) {
		if (sourceItem.getTimeInfo() == null || sourceItem.getTimeInfo().getTimes() == null) {
			throw new RuntimeException(getClass().getSimpleName() + ": cannot copy data from sourceExchangeItem '"
					+ sourceItem.getId() + "' of type " + sourceItem.getClass().getSimpleName()
					+ " because it contains no time info.");
		}
		if (sourceItem.getValuesType() != ValueType.IVectorType) {
			throw new RuntimeException(getClass().getSimpleName() + ": cannot copy data from sourceExchangeItem '"
					+ sourceItem.getId() + "' of type " + sourceItem.getClass().getSimpleName()
					+ " because its value type is not " + ValueType.IVectorType);
		}

		this.modelDll.setTimesForExchangeItem(this.parameterNumber, this.locationNumber,
				sourceItem.getTimeInfo().getTimes());
		setValues(sourceItem.getValues());
	}
}
