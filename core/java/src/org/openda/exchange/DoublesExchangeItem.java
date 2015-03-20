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
package org.openda.exchange;
import org.openda.utils.Array;

/**
 * Small extension of ArrayExchangeItem to keep the depending classes happy.
 */
public class DoublesExchangeItem extends ArrayExchangeItem{

	/**
	 * Constructor starting from an array of doubles.
	 * @param id The exchange item's id
	 * @param role Input, Outpu or both
	 * @param values The exchange item's values
	 */
	public DoublesExchangeItem(String id, Role role, double[] values) {
		super(id,role);
		this.array = new Array(values);
	}

	/**
	 * Simplified constructor for only one double
	 * @param id The exchange item's id
	 * @param role Input, Outpu or both
	 * @param value The exchange item's value
	 */
	public DoublesExchangeItem(String id, Role role, double value) {
		super(id,role);
		this.array = new Array(new double[1]);
		this.array.setValueAsDouble(0, value);
	}

	public void setValues(double[] values){
		if(values.length==this.array.length()){
			this.array.setValuesAsDoubles(values);			
		}else{
			this.array = new Array(values);
		}
		this.notifyObservers();
	}
	
}
