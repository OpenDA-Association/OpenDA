/* OpenDA v2.4.1 
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
package org.openda.model_delwaq;
import org.openda.interfaces.*;

/**
 * Created with IntelliJ IDEA.
 * User: bos_en
 * Date: 8/27/13
 */
public class delwaqSubstanceExchangeItem implements IExchangeItem {
	private String id;
	private String description;
	private Role role;
	private double[] values;
	private delwaqBinaryFile binaryFileObject;

	/**
	 * Constructor
	 *
	 * @param id
	 * @param description
	 * @param role
	 * @param values
	 * @param binaryFileObject
	 */
	public delwaqSubstanceExchangeItem(String id, String description, Role role, double[] values, delwaqBinaryFile binaryFileObject) {
		this.id = id;
		this.description = description;
		this.role = role;
		this.values = values;
		this.binaryFileObject = binaryFileObject;
	}

	/**
	 * Get the role of the exchange item (input, output, or both)
	 *
	 * @return The exchange items's role
	 */
	public Role getRole() {
		return role;
	}

	/**
	 * The identifier for the exchangeItem (must be unique within the context of a model instance).
	 *
	 * @return The string identifying the exchangeItem
	 */
	public String getId() {
		return id;
	}

	/**
	 * Optional additional description for the exchange item
	 *
	 * @return A descriptive string. A return value of null indicates that no description is specified.
	 */
	public String getDescription() {
		return description;
	}

	/**
	 * Ask which object type will be returned by getValues() call
	 *
	 * @return The object type that will be returned by getValues() call
	 */
	public Class getValueType() {
		return double[].class;
	}

	/**
	 * Copy content and possibly meta data from the given source item into this item.
	 * RuntimeExceptions can be thrown if:
	 * - the type of the source exchangeItem is not recognized
	 *
	 * @param sourceItem The item to copy the values from.
	 */
	public void copyValuesFromItem(IExchangeItem sourceItem) {
		throw new RuntimeException("not implemented");
	}

	/**
	 * Return information about time if it exists.
	 * If null is returned, the object does not now about time. The means that it can repeated produce and/or
	 * accept values (that may vary per call), but that you can not get or set values for multiple times.
	 *
	 * @return time info
	 */
	public ITimeInfo getTimeInfo() {
		return null;
	}

	/**
	 * Return information about the content of the data if it exists. Returns null if no quantity info exists.
	 * This is about quantity, unit, etc...
	 *
	 * @return quantity info
	 */
	public IQuantityInfo getQuantityInfo() {
		return null;
	}

	/**
	 * Return information about the spatial aspect of the data if it exists. Returns null if no geometry info exists.
	 * This is about latitude, longitude, etc...
	 *
	 * @return geometry info
	 */
	public IGeometryInfo getGeometryInfo() {
		return null;
	}

	/**
	 * Ask which object type will be returned by getValues() call
	 *
	 * @return The object type that will be returned by getValues() call
	 */
	public ValueType getValuesType() {
		return ValueType.doubleType;
	}

	/**
	 * Get the values of the exchange item
	 *
	 * @return The values, according the type as defined in <code>getValueType()</code>
	 */
	public Object getValues() {
		return values;
	}

	/**
	 * Get the values of the exchange item as an array of doubles
	 *
	 * @return The values as an array of doubles
	 */
	public double[] getValuesAsDoubles() {
		return values;
	}

	/**
	 * Perform a values += alpha * axpyValues</c> operation on each value in the exchange item.
	 *
	 * @param alpha      The <c>alpha</c> in <c>state variable += alpha * vector</c>.
	 * @param axpyValues The values for the axpy-operation on all values in the exchange item.
	 */
	public void axpyOnValues(double alpha, double[] axpyValues) {
		if (values.length != axpyValues.length) throw new RuntimeException("Unequal number of values and axpyValues.");
		for (int i=0; i<axpyValues.length; i++) {
			values[i] += alpha * axpyValues[i];
		}
	}

	/**
	 * Multiply each value in the exchange item's value with the related multiplication factor.
	 *
	 * @param multiplicationFactors The multiplication factors for all exchange time values.
	 */
	public void multiplyValues(double[] multiplicationFactors) {
		if (values.length != multiplicationFactors.length) throw new RuntimeException("Unequal number of values and multiplicationFactors.");
		for (int i=0; i<multiplicationFactors.length; i++) {
			values[i] *= multiplicationFactors[i];
		}
	}

	/**
	 * Set the values of the exchange item
	 *
	 * @param values The values to be set, ccording the type as defined in <code>getValueType()</code>
	 */
	public void setValues(Object values) {
		throw new RuntimeException("not implemented");
	}

	/**
	 * Set the values of the exchange item
	 *
	 * @param values The values as an array of doubles
	 */
	public void setValuesAsDoubles(double[] values) {
		if (this.values.length != values.length) throw new RuntimeException("Unequal number of old values and new values.");
		this.values = values;
	}

	/**
	 * Check if the exchange item is a time series
	 *
	 * @return Array of time stamps in milliseconds since January 1, 1970, 00:00:00 GMT
	 */
	public double[] getTimes() {
		return binaryFileObject.getTimes();
	}

	/**
	 * Set the times for the exchangeItem
	 * setTimes is only allowed if getTimes() != null
	 * and if the exchangeItem has role Input or InOut
	 *
	 * @param times The times as an array of doubles
	 */
	public void setTimes(double[] times) {
		throw new RuntimeException("not implemented");
	}
}
