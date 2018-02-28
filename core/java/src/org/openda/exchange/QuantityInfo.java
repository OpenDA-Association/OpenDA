/* OpenDA v2.4.3 
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

import org.openda.interfaces.IQuantityInfo;

/**
 * Direct implementation of IQuantityInfo interface.
 * 
 * @author verlaanm
 *
 */
public class QuantityInfo implements IQuantityInfo {

	private String quantity=null;
	private String unit=null;
	
	public QuantityInfo(String quantity, String unit){
		this.quantity=quantity;
		this.unit=unit;
	}
	
	
	public String getQuantity() {
		return quantity;
	}

	
	public String getUnit() {
		return unit;
	}

	public void setUnit(String unit){
		this.unit = unit;
	}
	
	public void setQuantity(String quantity){
		this.quantity = quantity;
	}
	
	public String toString(){
		String result = "{QuantityInfo quantity="+this.quantity+", unit="+this.unit+" }";
		return result;
	}

	
	public boolean equals(Object o) {
		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass() || o.hashCode() != hashCode()) {
			return false;
		}
		QuantityInfo that = (QuantityInfo) o;

		if (this.quantity == null) {
			if (that.quantity != null) return false;
		} else {//if this.quantity != null.
			if (!this.quantity.equals(that.quantity)) return false;
		}
		if (this.unit == null) {
			if (that.unit != null) return false;
		} else {//if this.unit != null.
			if (!this.unit.equals(that.unit)) return false;
		}

		return true;
	}

	
	public int hashCode() {
		int h = this.quantity == null ? 0 : this.quantity.hashCode();
		h = 31 * h + (this.unit == null ? 0 : this.unit.hashCode());
		return h;
	}
}
