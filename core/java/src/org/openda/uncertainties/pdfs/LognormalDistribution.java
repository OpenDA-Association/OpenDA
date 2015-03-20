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

import due.utilities.mathutil.*;

/**
 * The LognormalDistribution class implements the two-parameter lognormal probability distribution
 * function with a mean of log-transform (location) and a variance of log-transform (scale)
 * as input parameters.
 *
 * This class is derived from DUE version 3.0 by brown@science.uva.nl
 */
public class LognormalDistribution extends PDF {

    /**
     * Instance of the normal distribution (used for calculating the inverseCDF).
     */
    private NormalDistribution normal = null;

	//distribution name.
    private static final String DISTRIBUTION_NAME = "Lognormal";

	//constants for the names of the parameters of this distribution.
	private static final String PARAM_MEAN = "Mean";
	private static final String PARAM_STD = "Std";

	//constants for the indices of the parameters in the ArrayList params.
	private static final int PARAM_INDEX_MEAN = 0;
	private static final int PARAM_INDEX_STD = 1;

	//default values for the parameters.
	private static final double DEFAULT_MEAN = 2.0;
	private static final double DEFAULT_STD = 0.5;


    public LognormalDistribution newInstance() {
        return new LognormalDistribution() ;
    }

	/**
	 * Construct a lognormal distribution with default values for the parameters.
	 */
	public LognormalDistribution() {
        super();

        params.add(new FunctionParameter(PARAM_MEAN, DEFAULT_MEAN));
		params.add(new FunctionParameter(PARAM_STD, DEFAULT_STD));
	}

	/**
     * Construct lognormal distribution.
     *
     * @param mean the location parameter (mean of log transform)
     * @param std the scale parameter (standard deviation of the log transform)
     * @throws IllegalArgumentException if std <= 0
     */
	public LognormalDistribution(double mean, double std) throws IllegalArgumentException {
        super();

		if (std <= 0) {
            throw new IllegalArgumentException("The standard deviation must be greater than zero.");
        }

		params.add(new FunctionParameter(PARAM_MEAN, mean));
		params.add(new FunctionParameter(PARAM_STD, std));
	}

    /**
     * Get the cumulative probability P(X <= x) for a specified x.
     *
     * @param x the integration limit
     * @return the cumulative probability
     */
	public double getCDFPoint(double x) throws ArithmeticException {
		double location = getMean();
		double scale = getStd();

        if (x < 0) {
            throw new IllegalArgumentException("Incorrect input");
        }
        else {
            return 0.5 * (1.0 + Mathematics.errorFunction(  ( Math.log(x) - location ) / ( scale * Math.sqrt(2.0) ) ) );
        }
	}

    /**
     * Get the probability density P(X = x).
     *
     * @param x a double value for which the pdf is required
     * @return the probability
     */
	public double getPDFPoint(double x) throws ArithmeticException {
		double location = getMean();
		double scale = getStd();

        double first = 1.0 / (x * scale * Math.sqrt(2.0 * Math.PI));
        double second = (Math.log(x)- location);
        double third = Math.exp(- (second  * second ) / (2 * ( scale * scale ) ) );
        return first * third;
	}

    /**
     * Get the inverse cumulative probability for a given P(X <= x).
     * Makes use of NormalDistribution.
     *
     * @param probability the probability
     * @return the inverse cumulative probability
     */
    public double getInverseCDFPoint(double probability) throws ArithmeticException, IllegalArgumentException {

        if (probability < 0.0 || probability >= 1.0) {
            throw new IllegalArgumentException("Incorrect input");
        }
        else {

        	//Don't use getStd() as this uses isStdFactor
            normal = new NormalDistribution(this.getMean(),this.getStd(), 0);
        	return Math.exp(normal.getInverseCDFPoint(probability) ); //Return the inverse CDF value of the lognormal distribution.
        }
    }

    public double getLowerLimit() {
        return getInverseCDFPoint(0.0001);
    }

    public double getUpperLimit() {
        return getInverseCDFPoint(0.99);
    }

    public double getCumulativeLowerLimit() {
        return getInverseCDFPoint(0.001);
    }

    public double getCumulativeUpperLimit() {
        return getInverseCDFPoint(0.95);
    }

    /**
     * Set a specified parameter.
     *
     * @param paramType the name of the parameter
     * @param value of the parameter
     * @throws IllegalArgumentException if std <= 0
     */
	public void setParam(String paramType, double value) throws IllegalArgumentException {

		if (paramType.equalsIgnoreCase(PARAM_MEAN)) {
			this.params.get(PARAM_INDEX_MEAN).setValue(value);

		} else if (paramType.equalsIgnoreCase(PARAM_STD)) {
		    if (value <= 0) {
	            throw new IllegalArgumentException("The standard deviation must be greater than zero.");
	        }
		    else {
    			this.params.get(PARAM_INDEX_STD).setValue(value);
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
	 * Get the value of parameter mean.
	 * @return double mean.
	 */
	public double getMean() {
		return this.params.get(PARAM_INDEX_MEAN).getValue();
	}

	/**
	 * Get the value of parameter std.
	 * @return double std.
	 */
	public double getStd() {

		double std = this.params.get(PARAM_INDEX_STD).getValue();

		return std;
	}

}
