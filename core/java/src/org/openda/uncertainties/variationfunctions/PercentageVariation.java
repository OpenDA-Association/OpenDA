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


package org.openda.uncertainties.variationfunctions;

import org.openda.uncertainties.FunctionParameter;


/**
 * PercentageVariation is used to store a percentage used for
 * variation per parameter calibration method.
 */
public class PercentageVariation extends Variation {

	//distribution name.
    private static final String DISTRIBUTION_NAME = "Percentage";

	//constants for the names of the parameters of this distribution.
	private static final String PARAM_PERCENTAGE = "Percentage";

	//constants for the indices of the parameters in the ArrayList params.
	private static final int PARAM_INDEX_PERCENTAGE = 0;

	//default values for the parameters.
	private static final double DEFAULT_PERCENTAGE = 0.0;


	public PercentageVariation newInstance() {
        return new PercentageVariation();
    }

	/**
	 * Construct a percentage variation with default values for the parameters.
	 */
	public PercentageVariation() {
		super();

		params.add(new FunctionParameter(PARAM_PERCENTAGE, DEFAULT_PERCENTAGE));
	}

    /**
     * Construct a percentage variation. The percentage indicates the size
     * of the possible variation in the variation per parameters calibration method.
     *
     * @param percentage the percentage.
     * @throws IllegalArgumentException if percentage < 0
     */
	public PercentageVariation(double percentage) throws IllegalArgumentException {
		super();

		if (percentage < 0) {
			throw new IllegalArgumentException("The percentage parameter must be positive or zero.");
		}

		params.add(new FunctionParameter(PARAM_PERCENTAGE, percentage));
	}

	/**
     * Set a specified parameter.
     *
     * @param String paramType the name of the parameter
     * @param double value of the parameter
     * @throws IllegalArgumentException if percentage < 0
     */
	public void setParam(String paramType, double value) throws IllegalArgumentException {

		if (paramType.equalsIgnoreCase(PARAM_PERCENTAGE)) {
		    if (value < 0) {
				throw new IllegalArgumentException("The percentage parameter must be positive or zero.");
	        }
		    else {
				this.params.get(PARAM_INDEX_PERCENTAGE).setValue(value);
		    }

		} else {
			throw new IllegalArgumentException("Invalid parameter type supplied.");
		}
	}

    /**
     * Get a realization for the specified value using
     * percentage variation.
     *
     * @param value the value to be varied
     * @param seed
     * @return realization the value including variation
     */
	public double getRealizationFromSeed(double value, long seed) {

		double realization;

        if (seed % 2 == 0) { // even
            realization = value + this.getPercentage() * value;
        } else {
            realization = value - this.getPercentage() * value;
        }

	    return realization;
	}

	/**
	 * Get the name of this distribution.
	 * @return String name of this distribution.
	 */
	public String toString() {
		return DISTRIBUTION_NAME;
	}

	/**
	 * Get the value of parameter percentage.
	 * @return double percentage.
	 */
	public double getPercentage() {
		return this.params.get(PARAM_INDEX_PERCENTAGE).getValue();
	}

}
