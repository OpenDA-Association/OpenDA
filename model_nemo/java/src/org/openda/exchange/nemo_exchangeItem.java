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
import org.openda.interfaces.IVector;
import org.openda.utils.Vector;

/**
 * Created with IntelliJ IDEA.
 * User: nils
 * Date: 7/18/13
 * Time: 9:02 PM
 * To change this template use File | Settings | File Templates.
 */
public class nemo_exchangeItem extends ExchangeItem {

	IVector vector;

	public nemo_exchangeItem(String id) {
		super(id);
	}

	public void setVector(IVector vector){
		this.vector=vector;
	}

	/**
	 * Ask which object type will be returned by getValues() call
	 *
	 * @return The object type that will be returned by getValues() call
	 */
	public ValueType getValuesType() {
		return ValueType.IVectorType;
	}

	/**
	 * Ask which object type will be returned by getValues() call
	 *
	 * @return The object type that will be returned by getValues() call
	 */
	public Class getValueType() {
		return IVector.class;
	}

	/**
	 * Get the values of the exchange item
	 *
	 * @return The values, according the type as defined in <code>getValueType()</code>
	 */
	public Object getValues() {
		return vector;  //To change body of implemented methods use File | Settings | File Templates.
	}

	/**
	 * Get the values of the exchange item as an array of doubles
	 *
	 * @return The values as an array of doubles
	 */
	public double[] getValuesAsDoubles() {
		return vector.getValues();
	}

	/**
	 * Perform a values += alpha * axpyValues</c> operation on each value in the exchange item.
	 *
	 * @param alpha      The <c>alpha</c> in <c>state variable += alpha * vector</c>.
	 * @param axpyValues The values for the axpy-operation on all values in the exchange item.
	 */
	public void axpyOnValues(double alpha, double[] axpyValues) {
		vector.axpy(alpha, new Vector(axpyValues));
		//To change body of implemented methods use File | Settings | File Templates.
	}

	/**
	 * Multiply each value in the exchange item's value with the related multiplication factor.
	 *
	 * @param multiplicationFactors The multiplication factors for all exchange time values.
	 */
	public void multiplyValues(double[] multiplicationFactors) {
        vector.pointwiseMultiply(new Vector(multiplicationFactors));
		//To change body of implemented methods use File | Settings | File Templates.
	}

	/**
	 * Get the values of the exchange item
	 *
	 * @param values The values to be set, ccording the type as defined in <code>getValueType()</code>
	 */
	public void setValues(Object values) {
		vector = (IVector) values;
	}

	/**
	 * Get the values of the exchange item
	 *
	 * @param values The values as an array of doubles
	 */
	public void setValuesAsDoubles(double[] values) {
		vector.setValues(values);
		//To change body of implemented methods use File | Settings | File Templates.
	}
}
