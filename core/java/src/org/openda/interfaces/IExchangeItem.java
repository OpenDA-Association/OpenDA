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


package org.openda.interfaces;

import java.io.Serializable;

/**
 * Item for which the values can be retrieved from or provided to the model.
 * The model tells which exchange items it has, after which the exchange item is used to access the values.
 * The IExchange item currently extends the 'previous' version.
 */
public interface IExchangeItem extends Serializable {

    /**
     * List of available data types.
     */
    enum ValueType {
        /** corresponds to java int */
        intType,
        /** corresponds to java double */
        doubleType,
        /** corresponds to java double[]   -> to be replaced by IArray */
        doublesType,
		/** corresponds to java float */
		floatType,
        /** corresponds to java float[]    -> to be replaced by IArray */
        floatsType,
        /** corresponds to java double[][] -> to be replaced by IArray */
        doubles2dType,
        /** corresponds to java String */
        StringType,
		/** corresponds to java Boolean */
		BooleanType,
        /** IVector -> in future IVector will extend IArray */
        IVectorType,
        /** IArray */
        IArrayType,
        /** corresponds to java Date */
        DateType,
        /** corresponds to ITime */
        ITimeType,
		/** can be any type of object, only used for very specific implementations */
		custom
    }

    /**
     * List of possible roles for the exchange values.
     */
    enum Role {
        /** Input for the model.*/
        Input,
        /** Output from the model.*/
        Output,
        /** Both input for the model and output from the model.*/
        InOut
    }

    /**
     * Get the role of the exchange item (input, output, or both)
     * @return The exchange items's role
     */
    Role getRole();

    /**
     * The identifier for the exchangeItem (must be unique within the context of a model instance).
     * @return The string identifying the exchangeItem
     */
    String getId();

    /**
     * Optional additional description for the exchange item
     * @return A descriptive string. A return value of null indicates that no description is specified.
     */
    String getDescription();

    /**
     * Copy content and possibly meta data from the given source item into this item.
     * RuntimeExceptions can be thrown if:
     * - the type of the source exchangeItem is not recognized
	 * @param sourceItem The item to copy the values from.
	 *
	 */
    void copyValuesFromItem(IExchangeItem sourceItem);

	/**
	 * Return information about time if it exists.
	 * If null is returned, the object does not now about time. The means that it can repeated produce and/or
	 * accept values (that may vary per call), but that you can not get or set values for multiple times.
	 *
	 * @return time info
	 */
    ITimeInfo getTimeInfo();

	/**
	 * Return information about the content of the data if it exists. Returns null if no quantity info exists.
	 * This is about quantity, unit, etc...
	 *
	 * @return quantity info
	 */
    IQuantityInfo getQuantityInfo();

	/**
	 * Return information about the spatial aspect of the data if it exists. Returns null if no geometry info exists.
	 * This is about latitude, longitude, etc...
	 *
	 * @return geometry info
	 */
    IGeometryInfo getGeometryInfo();

    /**
     * Ask which object type will be returned by getValues() call
     * @return The object type that will be returned by getValues() call
     */
    ValueType getValuesType();

    /**
     * Get the values of the exchange item
     * @return The values, according the type as defined in <code>getValueType()</code>
     */
    Object getValues();

	/**
	 * Get the values of the exchange item as an array of doubles
	 * @return The values as an array of doubles
	 */
	double[] getValuesAsDoubles();

	/**
	 * Perform a values += alpha * axpyValues</c> operation on each value in the exchange item.
	 * @param alpha The <c>alpha</c> in <c>state variable += alpha * vector</c>.
	 * @param axpyValues  The values for the axpy-operation on all values in the exchange item.
	 */
	void axpyOnValues(double alpha, double[] axpyValues);

	/**
	 * Multiply each value in the exchange item's value with the related multiplication factor.
	 * @param multiplicationFactors  The multiplication factors for all exchange time values.
	 */
	void multiplyValues(double[] multiplicationFactors);

	/**
	 * Set the values of the exchange item
	 * @param values  The values to be set, according the type as defined in <code>getValueType()</code>
	 */
	void setValues(Object values);

	/**
	 * Set the values of the exchange item
	 * @param values  The values as an array of doubles
	 */
	void setValuesAsDoubles(double[] values);

	/**
	 * Check if the exchange item is a time series
	 * @return null if the exchange item is time independent, series of time stamps/spans otherwise
	 */
	double[] getTimes();

	/**
	 * Set the times for the exchangeItem
	 * setTimes is only allowed if getTimes() != null
	 * and if the exchangeItem has role Input or InOut
	 * @param times The times as an array of doubles
	 */
	void setTimes(double[] times);

}
