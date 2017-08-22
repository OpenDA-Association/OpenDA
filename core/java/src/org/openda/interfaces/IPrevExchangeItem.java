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


package org.openda.interfaces;

import java.io.Serializable;

/**
 * Item for which the values can be retrieved from or provided to the model.
 * The model tells which exchange items it has, after which the exchange item is used to access the values.
 * This version of the exchange item is called the 'previous' version. It will become deprecated not too
 * long from now, so new model and data object developers should implement
 * @see IExchangeItem
 */
public interface IPrevExchangeItem extends Serializable {

    /**
     * List of possible roles for the exchange values.
     */
    public enum Role {
        /** Input for the model.*/
        Input,
        /** Output from the model.*/
        Output,
        /** Both input for the model and output from the model.*/
        InOut
    }

    /**
     * The identifier for the exchangeItem (must be unique within the context of a model instance).
     * @return The string identifying the exchangeItem
     */
    String getId();

    /**
     * Optional additional description for the exchange item
     * @return A descriptive string. A return value of null indicates that no description is specfied.
     */
    String getDescription();

    /**
     * Ask which object type will be returned by getValues() call
     * @return The object type that will be returned by getValues() call
     */
	public Class getValueType();

    /**
     * Get the role of the exchange item (input, output, or both)
     * @return The exchange items's role
     */
    public Role getRole();

    /**
     * Get the values of the exchange item
     * @return The values, according the type as defined in <code>getValueType()</code>
     */
    public Object getValues();

    /**
     * Get the values of the exchange item as an array of doubles
     * @return The values as an array of doubles
     */
    public double[] getValuesAsDoubles();

    /**
     * Perform a values += alpha * axpyValues</c> operation on each value in the exchange item.
     * @param alpha The <c>alpha</c> in <c>state variable += alpha * vector</c>.
     * @param axpyValues  The values for the axpy-operation on all values in the exchange item.
     */
    public void axpyOnValues(double alpha, double[] axpyValues);

	/**
	 * Multiply each value in the exchange item's value with the related multiplication factor.
	 * @param multiplicationFactors  The multiplication factors for all exchange time values.
	 */
	public void multiplyValues(double[] multiplicationFactors);

    /**
     * Set the values of the exchange item
     * @param values  The values to be set, ccording the type as defined in <code>getValueType()</code>
     */
    public void setValues(Object values);

    /**
     * Set the values of the exchange item
     * @param values  The values as an array of doubles
     */
    public void setValuesAsDoubles(double[] values);

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
    public void setTimes(double[] times);
}
