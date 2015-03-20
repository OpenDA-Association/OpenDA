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
 * The UniformDistribution class implements the continuous Uniform probability
 * distribution function with a lower limit (xMin) and an upper limit (xMax) as input
 * parameters.
 *
 * This class is derived from DUE version 3.0 by brown@science.uva.nl
 */
public class UniformDistribution extends PDF {

    //distribution name.
	private static final String DISTRIBUTION_NAME = "Uniform";

	//constants for the names of the parameters of this distribution.
	private static final String PARAM_X_MIN = "X Min";
	private static final String PARAM_X_MAX = "X Max";

	//constants for the indices of the parameters in the ArrayList params.
	private static final int PARAM_INDEX_X_MIN = 0;
	private static final int PARAM_INDEX_X_MAX = 1;

	//default values for the parameters.
	private static final double DEFAULT_X_MIN = 0.0;
	private static final double DEFAULT_X_MAX = 1.0;


    public UniformDistribution newInstance() {
        return new UniformDistribution() ;
    }

	/**
	 * Construct a uniform distribution with default values for the parameters.
	 */
	public UniformDistribution() {
        super();

        params.add(new FunctionParameter(PARAM_X_MIN, DEFAULT_X_MIN));
		params.add(new FunctionParameter(PARAM_X_MAX, DEFAULT_X_MAX));
	}

    /**
     * Construct the PDF with lower and upper limits on x.
     *
     * @param xMin the lower limit
     * @param xMax the upper limit
     * @throws IllegalArgumentException if xMin >= xMax
     */
	public UniformDistribution(double xMin, double xMax) throws IllegalArgumentException {
        super();

		//check if supplied xMin value < supplied xMax value.
	    if (xMin >= xMax) {
	        throw new IllegalArgumentException("Lower limit must be less than upper limit.");
	    }

		params.add(new FunctionParameter(PARAM_X_MIN, xMin));
		params.add(new FunctionParameter(PARAM_X_MAX, xMax));
	}

    /**
     * Get the cumulative probability P(X <= x) for a specified x.
     *
     * @param x the integration limit
     * @return the cumulative probability
     */
	public double getCDFPoint(double x) throws ArithmeticException {

		double xMin = this.getXMin();
		double xMax = this.getXMax();

		double pdfValue = 0.0;


		if (x < xMin) {
			pdfValue = 0.0;
        } else if (x >= xMax) {
        	pdfValue = 1.0;
        } else {
        	pdfValue = ((x-xMin)/(xMax-xMin));
        }
		return pdfValue;
	}

	/**
     * Get the probability density P(X = x).
     *
     * @param x a double value for which the pdf is required
     * @return the probability
     */
	public double getPDFPoint(double x) throws ArithmeticException {

		double xMin = this.getXMin();
		double xMax = this.getXMax();
        double pdfValue = 0.0;

		if (xMin <= x && x <= xMax) {
			pdfValue = 1.0/(xMax-xMin);
        } else {
        	pdfValue = 0.0;
        }

	    return pdfValue;
	}

    /**
     * Get the inverse cumulative probability for a given P(X <= x).
     *
     * @param probability the probability for which x is required.
     * @return the cumulative probability
     */
    public double getInverseCDFPoint(double probability) throws ArithmeticException, IllegalArgumentException {

		double xMin = this.getXMin();
		double xMax = this.getXMax();

    	if (probability < 0.0 || probability > 1.0) {
            throw new IllegalArgumentException("Incorrect input");
        } else {
            return (xMin+(probability*(xMax-xMin)));
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
     * @param String paramType the name of the parameter
     * @param double value of the parameter
     * @throws IllegalArgumentException if xMin >= xMax
     */
	public void setParam(String paramType, double value) throws IllegalArgumentException {

		if (paramType.equalsIgnoreCase(PARAM_X_MIN)) {
			//check if supplied xMin value < current xMax value.
		    if (value >= this.params.get(PARAM_INDEX_X_MAX).getValue()) {
		        throw new IllegalArgumentException("Lower limit must be less than upper limit.");
		    } else {
    			this.params.get(PARAM_INDEX_X_MIN).setValue(value);
		    }
		} else if (paramType.equalsIgnoreCase(PARAM_X_MAX)) {
			//check if current xMin value < supplied xMax value.
		    if (value <= this.params.get(PARAM_INDEX_X_MIN).getValue()) {
		        throw new IllegalArgumentException("Lower limit must be less than upper limit.");
		    } else {
    			this.params.get(PARAM_INDEX_X_MAX).setValue(value);
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
	 * Get the value of parameter xMin.
	 * @return double xMin.
	 */
	public double getXMin() {
		return this.params.get(PARAM_INDEX_X_MIN).getValue();
	}

	/**
	 * Get the value of parameter xMax.
	 * @return double xMax.
	 */
	public double getXMax() {
		return this.params.get(PARAM_INDEX_X_MAX).getValue();
	}

}
