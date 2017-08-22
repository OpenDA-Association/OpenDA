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
package org.openda.blackbox.config;

import org.openda.exchange.ConstantLimitsRangeValidationExchangeItem;
import org.openda.interfaces.IPrevExchangeItem;

/**
 * Constraint that limits the range of the values of an exchangeItem using a constant lower and/or upper limit.
 * If the value of the exchange item is below the lower limit, then the value is changed to the lower limit.
 * If the value of the exchange item is above the upper limit, then the value is changed to the upper limit.
 * If the exchangeItem has more than one value (e.g. a 2D grid), then the limits are applied to all values in the exchangeItem.
 *
 * @author Arno Kockx
 */
public class ConstantLimitsRangeValidationConstraint implements RangeValidationConstraint {
	/**
	 * Id of the constraintExchangeItem that is created by the call to method this.wrapAffectedExchangeItem.
	 */
	private final String constraintExchangeItemId;

	/**
	 * Id of the exchangeItem to which this constraint should be applied.
	 */
	private final String affectedExchangeItemId;

	/**
	 * This is Double.NaN if not set.
	 */
	private final double lowerLimit ;
	/**
	 * This is Double.NaN if not set.
	 */
	private final double upperLimit;

	/**
	 * @param constraintExchangeItemId id of the constraintExchangeItem that is created by the call to method this.wrapAffectedExchangeItems.
	 * @param affectedExchangeItemId id of the exchangeItem to which this constraint should be applied.
	 * @param lowerLimit
	 * @param upperLimit
	 */
	public ConstantLimitsRangeValidationConstraint(String constraintExchangeItemId, String affectedExchangeItemId,
			double lowerLimit, double upperLimit) {

		if (constraintExchangeItemId == null || constraintExchangeItemId.isEmpty()) {
			throw new IllegalArgumentException("constraintExchangeItemId is empty.");
		}
		if (affectedExchangeItemId == null || affectedExchangeItemId.isEmpty()) {
			throw new IllegalArgumentException("affectedExchangeItemId is empty.");
		}
		if (Double.isNaN(lowerLimit) && Double.isNaN(upperLimit)) {
			throw new IllegalArgumentException("lowerLimit and upperLimit are both NaN.");
		}
		if (lowerLimit > upperLimit) {
			throw new IllegalArgumentException("lowerLimit > upperLimit.");
		}

		this.constraintExchangeItemId = constraintExchangeItemId;
		this.affectedExchangeItemId = affectedExchangeItemId;
		this.lowerLimit = lowerLimit;
		this.upperLimit = upperLimit;
	}

	/**
	 * @return id of the target exchangeItem for this constraint.
	 */
	public String getConstraintExchangeItemId() {
		return this.constraintExchangeItemId;
	}

	/**
	 * @return id of the source exchangeItem for this constraint.
	 */
	public String getAffectedExchangeItemId() {
		return this.affectedExchangeItemId;
	}

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
	public IPrevExchangeItem wrapAffectedExchangeItem(IPrevExchangeItem affectedExchangeItem) {
		if (affectedExchangeItem == null) {
			throw new IllegalArgumentException("affectedExchangeItem is null.");
		}
		if (!this.affectedExchangeItemId.equals(affectedExchangeItem.getId())) {
			throw new IllegalStateException("ExchangeItem with id '" + affectedExchangeItem.getId() + "' not affected by this constraint.");
		}

		//wrap exchange item (Decorator pattern).
		return new ConstantLimitsRangeValidationExchangeItem(this.constraintExchangeItemId, affectedExchangeItem, this.lowerLimit, this.upperLimit);
	}
}
