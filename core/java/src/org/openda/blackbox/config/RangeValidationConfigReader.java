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

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.openda.core.io.castorgenerated.RangeValidationConfigXML;
import org.openda.core.io.castorgenerated.RangeValidationConstantLimitsConstraintXML;
import org.openda.core.io.castorgenerated.RangeValidationConstraintXML;
import org.openda.utils.io.CastorUtils;

/**
 * Configuration reader for RangeValidationConfig.
 *
 * @author Arno Kockx
 */
public class RangeValidationConfigReader {
	private final RangeValidationConstraint[] rangeValidationConstraints;

	public RangeValidationConfigReader(File configFile) {
		List<RangeValidationConstraint> constraints = new ArrayList<RangeValidationConstraint>();

		RangeValidationConfigXML rangeValidationConfigXML =
				(RangeValidationConfigXML) CastorUtils.parse(configFile, RangeValidationConfigXML.class);
		for (RangeValidationConstraintXML rangeValidationConstraintXML : rangeValidationConfigXML.getConstraint()) {
			String constraintId = rangeValidationConstraintXML.getId();

			RangeValidationConstantLimitsConstraintXML constantLimitsConstraint = rangeValidationConstraintXML.getConstantLimits();
			if (constantLimitsConstraint != null) {
				String targetExchangeItemIdSuffix = constantLimitsConstraint.getTargetExchangeItemIdSuffix();
				if (targetExchangeItemIdSuffix == null || targetExchangeItemIdSuffix.isEmpty()) {
					throw new RuntimeException(getClass().getSimpleName()
							+ ": targetExchangeItemIdSuffix is empty in constraint with id '"
							+ constraintId + "' in rangeValidationConfig file. Please specify valid id in the configuration.");
				}

				//get limits.
				double lowerLimit = Double.NaN;
				double upperLimit = Double.NaN;
				if (constantLimitsConstraint.hasLowerLimit() && constantLimitsConstraint.hasUpperLimit()) {//if lower and upper limit configured.
					lowerLimit = constantLimitsConstraint.getLowerLimit();
					if (Double.isNaN(lowerLimit)) {
						throw new RuntimeException(getClass().getSimpleName() + ": lowerLimit is NaN in constraint with id '"
								+ constraintId + "' in rangeValidationConfig file."
								+ " Please specify a valid lowerLimit or no lowerLimit in the configuration.");
					}
					upperLimit = constantLimitsConstraint.getUpperLimit();
					if (Double.isNaN(upperLimit)) {
						throw new RuntimeException(getClass().getSimpleName() + ": upperLimit is NaN in constraint with id '"
								+ constraintId + "' in rangeValidationConfig file."
								+ " Please specify a valid upperLimit or no upperLimit in the configuration.");
					}
					if (lowerLimit > upperLimit) {
						throw new RuntimeException(getClass().getSimpleName() + ": lowerLimit > upperLimit in constraint with id '"
								+ constraintId + "' in rangeValidationConfig file. Please specify valid limits in the configuration.");
					}

				} else if (constantLimitsConstraint.hasLowerLimit()) {//if only lowerLimit configured.
					lowerLimit = constantLimitsConstraint.getLowerLimit();
					if (Double.isNaN(lowerLimit)) {
						throw new RuntimeException(getClass().getSimpleName() + ": lowerLimit is NaN in constraint with id '"
								+ constraintId + "' in rangeValidationConfig file."
								+ " Please specify a valid lowerLimit or no lowerLimit in the configuration.");
					}

				} else if (constantLimitsConstraint.hasUpperLimit()) {//if only upperLimit configured.
					upperLimit = constantLimitsConstraint.getUpperLimit();
					if (Double.isNaN(upperLimit)) {
						throw new RuntimeException(getClass().getSimpleName() + ": upperLimit is NaN in constraint with id '"
								+ constraintId + "' in rangeValidationConfig file."
								+ " Please specify a valid upperLimit or no upperLimit in the configuration.");
					}

				} else {//if no limits configured.
					throw new RuntimeException(getClass().getSimpleName() + ": no lowerLimit or upperLimit configured in constraint with id '"
							+ constraintId + "' in rangeValidationConfig file. Please specify valid limits in the configuration.");
				}

				//create constraints.
				for (String sourceExchangeItemId : constantLimitsConstraint.getSourceExchangeItemId()) {
					if (sourceExchangeItemId == null || sourceExchangeItemId.isEmpty()) {
						throw new RuntimeException(getClass().getSimpleName()
								+ ": sourceExchangeItemId is empty in constraint with id '"
								+ constraintId + "' in rangeValidationConfig file. Please specify valid id in the configuration.");
					}

					String targetExchangeItemId = sourceExchangeItemId + targetExchangeItemIdSuffix;
					ConstantLimitsRangeValidationConstraint constraint = new ConstantLimitsRangeValidationConstraint(targetExchangeItemId,
							sourceExchangeItemId, lowerLimit, upperLimit);
					constraints.add(constraint);
				}

			} else {
				throw new UnsupportedOperationException(getClass().getSimpleName()
						+ "Unsupported range validation constraint type configured in rangeValidationConfig file.");
			}
		}

		this.rangeValidationConstraints = constraints.toArray(new RangeValidationConstraint[constraints.size()]);
	}

	public RangeValidationConstraint[] getRangeValidationConstraints() {
		return this.rangeValidationConstraints;
	}
}
