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

package org.openda.model_hspf;

import org.openda.interfaces.IPrevExchangeItem;

/**
 * Wraps an exchange item to add an ensembleMemberId to its id.
 *
 * @author Arno Kockx
 */
public class EnsembleMemberExchangeItem implements IPrevExchangeItem {
	private final IPrevExchangeItem wrappedExchangeItem;
	private final String ensembleMemberId;

	public EnsembleMemberExchangeItem(IPrevExchangeItem wrappedExchangeItem, String ensembleMemberId) {
		if (wrappedExchangeItem == null) throw new IllegalArgumentException("wrappedExchangeItem == null");
		if (ensembleMemberId == null) throw new IllegalArgumentException("ensembleMemberId == null");

		this.wrappedExchangeItem = wrappedExchangeItem;
		this.ensembleMemberId = ensembleMemberId;
	}

	public String getId() {
		return wrappedExchangeItem.getId() + ".EM" + ensembleMemberId;
	}

	public String getDescription() {
		return wrappedExchangeItem.getDescription();
	}

	public Class getValueType() {
		return wrappedExchangeItem.getValueType();
	}

	public Role getRole() {
		return wrappedExchangeItem.getRole();
	}

	public Object getValues() {
		return wrappedExchangeItem.getValues();
	}

	public double[] getValuesAsDoubles() {
		return wrappedExchangeItem.getValuesAsDoubles();
	}

	public void axpyOnValues(double alpha, double[] axpyValues) {
		wrappedExchangeItem.axpyOnValues(alpha, axpyValues);
	}

	public void multiplyValues(double[] multiplicationFactors) {
		wrappedExchangeItem.multiplyValues(multiplicationFactors);
	}

	public void setValues(Object values) {
		wrappedExchangeItem.setValues(values);
	}

	public void setValuesAsDoubles(double[] values) {
		wrappedExchangeItem.setValuesAsDoubles(values);
	}

	public double[] getTimes() {
		return wrappedExchangeItem.getTimes();
	}

	public void setTimes(double[] times) {
		wrappedExchangeItem.setTimes(times);
	}
}
