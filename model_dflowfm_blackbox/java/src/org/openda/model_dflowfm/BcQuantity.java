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
package org.openda.model_dflowfm;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by prevel on 27-Nov-15.
 */
public class BcQuantity
{
	private BcProperty quantity;
	private BcProperty unit;
	private List<Double> values;

	public BcQuantity(BcProperty quantity)
	{
		this.quantity = quantity;
		values = new ArrayList<>();
	}

	public BcQuantity(BcProperty quantity, BcProperty unit)
	{
		this(quantity);
		this.unit = unit;
	}

	public void setUnit(BcProperty unit) { this.unit = unit; }
	public void setColumnData(List<Double> values) { this.values = values; }
	public void addColumnData(Double value) { values.add(value); }

	public BcProperty getQuantity() { return quantity; }
	public BcProperty getUnit() { return unit; }
	public List<Double> getValues() { return values; }
}
