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

package org.openda.exchange;

import org.openda.blackbox.config.BBUtils;
import org.openda.interfaces.IArray;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.IVector;

/**
 * This exchangeItem wraps another exchangeItem and applies constant range validation limits
 * to the wrapped exchangeItem whenever the values in the exchangeItem are changed from the outside.
 *
 * @author Arno Kockx
 */
@SuppressWarnings("serial")
public class ConstantLimitsRangeValidationExchangeItem implements IPrevExchangeItem {
	/**
	 * Id of this constraintExchangeItem.
	 */
	private final String constraintExchangeItemId;

	private final IPrevExchangeItem wrappedExchangeItem;

	/**
	 * This is Double.NaN if not set.
	 */
	private final double lowerLimit;
	/**
	 * This is Double.NaN if not set.
	 */
	private final double upperLimit;

	/**
	 * Wraps the given exchangeItem and applies the given range validation limits
	 * to the wrapped exchangeItem whenever the values in the exchangeItem are changed from the outside.
	 * It is also possible to only set a lowerLimit or only an upperLimit.
	 *
	 * @param exchangeItem to apply rangeValidation limits to.
	 * @param lowerLimit Double.NaN means not set.
	 * @param upperLimit Double.NaN means not set.
	 */
	public ConstantLimitsRangeValidationExchangeItem(String constraintExchangeItemId,
			IPrevExchangeItem exchangeItem, double lowerLimit, double upperLimit) {

		if (constraintExchangeItemId == null || constraintExchangeItemId.isEmpty()) {
			throw new IllegalArgumentException("constraintExchangeItemId is empty.");
		}
		if (exchangeItem == null) {
			throw new IllegalArgumentException("exchangeItem is null.");
		}
		if (Double.isNaN(lowerLimit) && Double.isNaN(upperLimit)) {
			throw new IllegalArgumentException("lowerLimit and upperLimit are both NaN.");
		}
		if (lowerLimit > upperLimit) {
			throw new IllegalArgumentException("lowerLimit > upperLimit.");
		}

		this.constraintExchangeItemId = constraintExchangeItemId;
		this.wrappedExchangeItem = exchangeItem;
		this.lowerLimit = lowerLimit;
		this.upperLimit = upperLimit;
	}

	public String getId() {
		return this.constraintExchangeItemId;
	}

	public String getDescription() {
		return this.wrappedExchangeItem.getDescription();
	}

	public Role getRole() {
		return this.wrappedExchangeItem.getRole();
	}

	public Class<?> getValueType() {
		return this.wrappedExchangeItem.getValueType();
	}

	public double[] getTimes() {
		return this.wrappedExchangeItem.getTimes();
	}
	
	public void setTimes(double[] times) {
		this.wrappedExchangeItem.setTimes(times);
	}

	public Object getValues() {
		return this.wrappedExchangeItem.getValues();
	}

	public double[] getValuesAsDoubles() {
		return this.wrappedExchangeItem.getValuesAsDoubles();
	}

	public void axpyOnValues(double alpha, double[] axpyValues) {
		double[] values = getValuesAsDoubles();
		if (axpyValues.length != values.length) {
			throw new IllegalStateException(getClass().getSimpleName() + ": axpyValues.length (" + axpyValues.length
					+ ") must equal valueCount (" + values.length + ") for exchange item " + getId() + ".");
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
					+ ") must equal valueCount (" + values.length + ") for exchange item " + getId() + ".");
		}

		for (int n = 0; n < values.length; n++) {
			values[n] *= multiplicationFactors[n];
		}
		setValuesAsDoubles(values);
	}

	public void setValues(Object values) {
		//need to convert the given values from Object to double array, because method applyLimits only works for a double array.
		double[] doubles = convertValuesToDoubles(values);
		setValuesAsDoubles(doubles);
	}

	public void setValuesAsDoubles(double[] values) {
		double[] newValues = applyLimits(values, this.lowerLimit, this.upperLimit);
		this.wrappedExchangeItem.setValuesAsDoubles(newValues);
	}

	/**
	 * Converts the given values from Object to double array.
	 *
	 * @param values
	 * @return double array.
	 */
	private double[] convertValuesToDoubles(Object values) {
		if (values == null) {
			return null;
		}

		if (values instanceof Double) {
			return new double[]{(Double) values};

		} else if (values instanceof double[]) {
			return (double[]) values;

		} else if (values instanceof float[]) {
			return BBUtils.toDoubleArray((float[]) values);

		} else if (values instanceof IVector) {
			return ((IVector) values).getValues();

		} else if (values instanceof IArray) {
			return ((IArray) values).getValuesAsDoubles();

		} else {//if values is an instance of another type.
			throw new RuntimeException(getClass().getSimpleName() + ".convertValuesToDoubles: unknown object type: "
					+ values.getClass().getName());
		}
	}

	private static double[] applyLimits(double[] values, double lowerLimit, double upperLimit) {
		if (values == null) {
			return values;
		}

		//copy values so that original values are not changed.
		double[] newValues = new double[values.length];
		System.arraycopy(values, 0, newValues, 0, values.length);

		if (!Double.isNaN(lowerLimit) && !Double.isNaN(upperLimit)) {//if lower and upper limit set.
			//apply limits.
			for (int n = 0; n < newValues.length; n++) {
				if (newValues[n] < lowerLimit) {
					newValues[n] = lowerLimit;
				}
				if (newValues[n] > upperLimit) {
					newValues[n] = upperLimit;
				}
			}

		} else if (!Double.isNaN(lowerLimit)) {//if only lower limit set.
			//apply lower limit.
			for (int n = 0; n < newValues.length; n++) {
				if (newValues[n] < lowerLimit) {
					newValues[n] = lowerLimit;
				}
			}

		} else if (!Double.isNaN(upperLimit)) {//if only upper limit set.
			//apply upper limit.
			for (int n = 0; n < newValues.length; n++) {
				if (newValues[n] > upperLimit) {
					newValues[n] = upperLimit;
				}
			}
		}

		return newValues;
	}
}
