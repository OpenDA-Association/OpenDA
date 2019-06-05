/*
 * Copyright (c) 2019 OpenDA Association
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


package org.openda.uncertainties.pdfs;

import org.openda.uncertainties.FunctionParameter;



/**
 * The GumbelMaximumDistribution class implements the Type II Gumbel probability distribution
 * function with location and scale as input parameters.  The function
 * is used for maximum extreme values.
 *
 * This class is derived from DUE version 3.0 by brown@science.uva.nl
 */
public class GumbelMaximumDistribution extends PDF {

	//distribution name.
	private static final String DISTRIBUTION_NAME = "GumbelMaximum";

	//constants for the names of the parameters of this distribution.
	private static final String PARAM_LOCATION = "Location";
	private static final String PARAM_SCALE = "Scale";

	//constants for the indices of the parameters in the ArrayList params.
	private static final int PARAM_INDEX_LOCATION = 0;
	private static final int PARAM_INDEX_SCALE = 1;

	//default values for the parameters.
	private static final double DEFAULT_LOCATION = 0.0;
	private static final double DEFAULT_SCALE = 1.0;

    public GumbelMaximumDistribution newInstance() {
        return new GumbelMaximumDistribution() ;
    }

	/**
	 * Construct a gumbel maximum distribution with default values for the parameters.
	 */
	public GumbelMaximumDistribution() {
        super();

        params.add(new FunctionParameter(PARAM_LOCATION, DEFAULT_LOCATION));
		params.add(new FunctionParameter(PARAM_SCALE, DEFAULT_SCALE));
	}

    /**
     * Construct the PDF with a location and scale parameter.
     *
     * @param location parameter
     * @param scale parameter
     * @throws IllegalArgumentException if scale <= 0
     */
	public GumbelMaximumDistribution(double location, double scale) throws IllegalArgumentException {
        super();

	    if (scale <= 0) {
            throw new IllegalArgumentException("The scale parameter must be greater than zero.");
        }

		params.add(new FunctionParameter(PARAM_LOCATION, location));
		params.add(new FunctionParameter(PARAM_SCALE, scale));
	}


    /**
     * Get the cumulative probability P(X <= x) for a specified x.
     *
     * @param x the integration limit
     * @return the cumulative probability
     */

	public double getCDFPoint(double x) throws ArithmeticException {
		double location = getLocation();
		double scale = getScale();

        return Math.exp(- Math.exp(-( (x-location) / scale ) ) );
	}

    /**
     * Get the probability density P(X = x).
     *
     * @param x a double value for which the pdf is required
     * @return the probability
     */

	public double getPDFPoint(double x) throws ArithmeticException {
		double location = getLocation();
		double scale = getScale();

        return ( 1.0 / scale ) * ( Math.exp(-( (x-location) / scale ) ) * Math.exp( - Math.exp(-( (x-location) / scale ) ) ) );
	}

    /**
     * Get the inverse cumulative probability for a given P(X <= x).
     *
     * @param probability the probability
     * @return the inverse cumulative probability
     */
    public double getInverseCDFPoint(double probability) throws ArithmeticException, IllegalArgumentException {
		double location = getLocation();
		double scale = getScale();

		if (probability < 0.0 || probability > 1.0) {
            throw new IllegalArgumentException("Incorrect input");
        } else {
            return location + ( scale * ( -Math.log( Math.log( 1.0/ (probability) ) ) ) );
        }
    }


    public double getLowerLimit() {
        return super.getLowerLimit();
    }

    public double getUpperLimit() {
        return super.getUpperLimit();
    }

    public double getCumulativeLowerLimit() {
        return super.getCumulativeLowerLimit();
    }

    public double getCumulativeUpperLimit() {
        return super.getCumulativeUpperLimit();
    }

    /**
     * Set a specified parameter.
     *
     * @param paramType the name of the parameter
     * @param value of the parameter
     * @throws IllegalArgumentException if scale <= 0
     */
	public void setParam(String paramType, double value) throws IllegalArgumentException {

		if (paramType.equalsIgnoreCase(PARAM_LOCATION)) {
			this.params.get(PARAM_INDEX_LOCATION).setValue(value);

		} else if (paramType.equalsIgnoreCase(PARAM_SCALE)) {
		    if (value <= 0) {
	            throw new IllegalArgumentException("The scale parameter must be greater than zero.");
	        }
		    else {
    			this.params.get(PARAM_INDEX_SCALE).setValue(value);
		    }
		} else {
			throw new IllegalArgumentException();
		}
	}

	/**
	 * Get the name of this distribution.
	 * @return String name of this distribution.
	 */
	public String toString() {
		return DISTRIBUTION_NAME;
	}

	/**
	 * Get the value of parameter location.
	 * @return double location.
	 */
	public double getLocation() {
		return this.params.get(PARAM_INDEX_LOCATION).getValue();
	}

	/**
	 * Get the value of parameter scale.
	 * @return double scale.
	 */
	public double getScale() {
		return this.params.get(PARAM_INDEX_SCALE).getValue();
	}
}
