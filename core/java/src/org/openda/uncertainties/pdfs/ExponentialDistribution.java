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


package org.openda.uncertainties.pdfs;

import org.openda.uncertainties.FunctionParameter;


/**
 * The ExponentialDistribution class implements the Exponential probability distribution
 * function with an exponential distribution parameter (rate parameter or lambda) and a location
 * parameter as input.
 *
 * This class is derived from DUE version 3.0 by brown@science.uva.nl
 */
public class ExponentialDistribution extends PDF {

	//distribution name.
	private static final String DISTRIBUTION_NAME = "Exponential";

	//constants for the names of the parameters of this distribution.
	private static final String PARAM_LOCATION = "Location";
	private static final String PARAM_RATE = "Rate";

	//constants for the indices of the parameters in the ArrayList params.
	private static final int PARAM_INDEX_LOCATION = 0;
	private static final int PARAM_INDEX_RATE = 1;

	//default values for the parameters.
	private static final double DEFAULT_LOCATION = 0.0;
	private static final double DEFAULT_RATE = 1.0;

    public ExponentialDistribution newInstance() {
        return new ExponentialDistribution() ;
    }

	/**
	 * Construct an exponential distribution with default values for the parameters.
	 */
	public ExponentialDistribution() {
        super();

        params.add(new FunctionParameter(PARAM_LOCATION, DEFAULT_LOCATION));
		params.add(new FunctionParameter(PARAM_RATE, DEFAULT_RATE));
	}

    /**
     * Construct an exponential distribution with a location parameter and a rate
     * parameter, which is typically specified as lambda, where scale = 1/lambda.
     * This constructor takes the location parameter and rate.  Specify a location
     * parameter of zero to construct the one parameter exponential distribution.
     *
     * @param location the location parameter of the exponential distribution
     * @param rate the rate parameter.
     * @throws IllegalArgumentException if rate <= 0
     */
	public ExponentialDistribution(double location, double rate) throws IllegalArgumentException {
        super();

		if (rate <= 0) {
            throw new IllegalArgumentException("The rate parameter must be greater than zero.");
        }

		params.add(new FunctionParameter(PARAM_LOCATION, location));
		params.add(new FunctionParameter(PARAM_RATE, rate));
	}


    /**
     * Get the cumulative probability P(X <= x) for a specified x.
     *
     * @param x the integration limit
     * @return the cumulative probability
     */
	public double getCDFPoint(double x) throws ArithmeticException {
		double location = this.getLocation();
		double rate = this.getRate();

		return ( 1.0 - Math.exp(-rate * (x-location) ) );
	}

	/**
     * Get the probability density P(X = x).
     *
     * @param x a double value for which the pdf is required
     * @return the probability
     */
	public double getPDFPoint(double x) throws ArithmeticException {
		double location = this.getLocation();
		double rate = this.getRate();

		return rate * Math.exp( -( rate * (x-location) ) );
	}

    /**
     * Get the inverse cumulative probability for a given P(X <= x).
     *
     * @param probability the probability
     * @return the inverse cumulative probability
     */
    public double getInverseCDFPoint(double probability) throws ArithmeticException, IllegalArgumentException {
		double location = this.getLocation();
		double rate = this.getRate();

        return (-Math.log(1.0-probability)/rate) + location;
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
     * @param String paramType the name of the parameter
     * @param double value of the parameter
     * @throws IllegalArgumentException if rate <= 0
     */
	public void setParam(String paramType, double value) throws IllegalArgumentException {

		if (paramType.equalsIgnoreCase(PARAM_LOCATION)) {
			this.params.get(PARAM_INDEX_LOCATION).setValue(value);

		} else if (paramType.equalsIgnoreCase(PARAM_RATE)) {
		    if (value <= 0) {
	            throw new IllegalArgumentException("The rate parameter must be greater than zero.");
	        }
		    else {
				this.params.get(PARAM_INDEX_RATE).setValue(value);
		    }

		} else {
			throw new IllegalArgumentException("Invalid parameter type supplied.");
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
	 * Get the value of parameter rate.
	 * @return double rate.
	 */
	public double getRate() {
		return this.params.get(PARAM_INDEX_RATE).getValue();
	}
}
