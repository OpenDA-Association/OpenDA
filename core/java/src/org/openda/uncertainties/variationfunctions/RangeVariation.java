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
 * RangeVariation is used to store a range (lower limit, upper limit) used for
 * variation per parameter calibration method.
 */
public class RangeVariation extends Variation {

	//distribution name.
	private static final String DISTRIBUTION_NAME = "Range";

	//constants for the names of the parameters of this distribution.
	private static final String PARAM_LOWER_LIMIT = "Lower Limit";
	private static final String PARAM_UPPER_LIMIT = "Upper Limit";

	//constants for the indices of the parameters in the ArrayList params.
	private static final int PARAM_INDEX_LOWER_LIMIT = 0;
	private static final int PARAM_INDEX_UPPER_LIMIT = 1;

	//default values for the parameters.
	private static final double DEFAULT_LOWER_LIMIT = 0.0;
	private static final double DEFAULT_UPPER_LIMIT = 1.0;


    public RangeVariation newInstance() {
        return new RangeVariation();
    }

	/**
	 * Construct a range variation with default values for the parameters.
	 */
	public RangeVariation() {
		super();

		params.add(new FunctionParameter(PARAM_LOWER_LIMIT, DEFAULT_LOWER_LIMIT));
		params.add(new FunctionParameter(PARAM_UPPER_LIMIT, DEFAULT_UPPER_LIMIT));
	}

    /**
     * Construct a range variation. The lowerLimit and upperLimit indicate the range
     * for the possible variation in the variation per parameters calibration method.
     *
     * @param lowerLimit the lower limit.
     * @param upperLimit the upper limit.
     * @throws IllegalArgumentException if lowerLimit >= upperLimit
     */
	public RangeVariation(double lowerLimit, double upperLimit) throws IllegalArgumentException {
		super();

		//check if supplied lowerLimit value < supplied upperLimit value.
	    if (lowerLimit >= upperLimit) {
	        throw new IllegalArgumentException("Lower limit must be less than upper limit.");
	    }

		params.add(new FunctionParameter(PARAM_LOWER_LIMIT, lowerLimit));
		params.add(new FunctionParameter(PARAM_UPPER_LIMIT, upperLimit));
	}

    /**
     * Set a specified parameter.
     *
     * @param String paramType the name of the parameter
     * @param double value of the parameter
     * @throws IllegalArgumentException if lowerLimit >= upperLimit
     */
	public void setParam(String paramType, double value) throws IllegalArgumentException {

		if (paramType.equalsIgnoreCase(PARAM_LOWER_LIMIT)) {
		    if (value >= this.params.get(PARAM_INDEX_UPPER_LIMIT).getValue()) {
		        throw new IllegalArgumentException("Lower limit must be less than upper limit.");
		    }
		    else {
    			this.params.get(PARAM_INDEX_LOWER_LIMIT).setValue(value);
		    }

		} else if (paramType.equalsIgnoreCase(PARAM_UPPER_LIMIT)) {
		    if (value <= this.params.get(PARAM_INDEX_LOWER_LIMIT).getValue()) {
		        throw new IllegalArgumentException("Lower limit must be less than upper limit.");
		    }
		    else {
    			this.params.get(PARAM_INDEX_UPPER_LIMIT).setValue(value);
		    }

		} else {
			throw new IllegalArgumentException("Invalid parameter type supplied.");
		}
	}

    /**
     * Get a realization using range variation.
     *
     * @param seed
     * @return realization the value including variation
     */
	public double getRealizationFromSeed(long seed) {

		double realization;

	    if (seed % 2 == 0) { // even
	        realization = this.getUpperLimit();
	    } else {
	        realization = this.getLowerLimit();
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
	 * Get the value of parameter lowerLimit.
	 * @return double lowerLimit.
	 */
	public double getLowerLimit() {
		return this.params.get(PARAM_INDEX_LOWER_LIMIT).getValue();
	}

	/**
	 * Get the value of parameter upperLimit.
	 * @return double upperLimit.
	 */
	public double getUpperLimit() {
		return this.params.get(PARAM_INDEX_UPPER_LIMIT).getValue();
	}

}
