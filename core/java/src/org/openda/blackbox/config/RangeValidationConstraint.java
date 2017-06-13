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
package org.openda.blackbox.config;

import org.openda.interfaces.IPrevExchangeItem;

/**
 * Interface to be implemented by classes that can wrap an exchange item in a rangeValidationExchangeItem
 * to apply certain range validation constraints (Decorator pattern).
 *
 * @author Arno Kockx
 */
public interface RangeValidationConstraint {
	/**
	 * @return id of the target exchangeItem for this constraint.
	 */
	public String getConstraintExchangeItemId();

	/**
	 * @return id of the source exchangeItem for this constraint.
	 */
	public String getAffectedExchangeItemId();

	/**
	 * The implementing class should wrap the given affectedExchangeItem
	 * with a rangeValidationExchangeItem and return the rangeValidationExchangeItem.
	 *
	 * The affectedExchangeItem is wrapped in a new rangeValidationExchangeItem
	 * that applies certain constraints in its set methods, such as
	 * e.g. ConstantLimitsRangeValidationExchangeItem. Whenever these set methods
	 * are called on the rangeValidationExchangeItem, then the constraints are applied
	 * to the values before they are set in the wrapped affectedExchangeItem (Decorator pattern).
	 *
	 * A rangeValidationExchangeItem should always have a different id from the affectedExchangeItem that it wraps.
	 *
	 * @param affectedExchangeItem the item to be wrapped.
	 * @return rangeValidationExchangeItem that wraps the given item.
	 */
	public IPrevExchangeItem wrapAffectedExchangeItem(IPrevExchangeItem affectedExchangeItem);
}
