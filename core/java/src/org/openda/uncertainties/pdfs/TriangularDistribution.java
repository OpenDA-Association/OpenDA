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
 * The TriangularDistribution class implements the Triangular probability distribution
 * function with a lower limit, mode and upper limit as input parameters.
 *
 * This class is derived from DUE version 3.0 by brown@science.uva.nl
 */
public class TriangularDistribution extends PDF {


	//distribution name.
	private static final String DISTRIBUTION_NAME = "Triangular";

	//constants for the names of the parameters of this distribution.
	private static final String PARAM_X_MIN = "X Min";
	private static final String PARAM_MODE = "Mode";
	private static final String PARAM_X_MAX = "X Max";

	//constants for the indices of the parameters in the ArrayList params.
	private static final int PARAM_INDEX_X_MIN = 0;
	private static final int PARAM_INDEX_MODE = 1;
	private static final int PARAM_INDEX_X_MAX = 2;

	//default values for the parameters.
	private static final double DEFAULT_X_MIN = 0.0;
	private static final double DEFAULT_MODE = 0.5;
	private static final double DEFAULT_X_MAX = 1.0;


    public TriangularDistribution newInstance() {
        return new TriangularDistribution() ;
    }

	/**
	 * Construct a triangular distribution with default values for the parameters.
	 */
	public TriangularDistribution() {
        super();

		params.add(new FunctionParameter(PARAM_X_MIN, DEFAULT_X_MIN));
		params.add(new FunctionParameter(PARAM_MODE, DEFAULT_MODE));
		params.add(new FunctionParameter(PARAM_X_MAX, DEFAULT_X_MAX));
	}

    /**
     * Construct the PDF with a lower x limit, mode and upper x limit.
     *
     * @param xMin the lower limit of the triangular distribution
     * @param mode the mode of the triangular distribution
     * @param xMax the upper limit of the triangular distribution
     * @throws IllegalArgumentException if not xMin < mode < xMAx
     */
	public TriangularDistribution(double xMin, double mode, double xMax) throws IllegalArgumentException {
        super();

		if (xMin >= mode || mode >= xMax || xMin >= xMax) {
            throw new IllegalArgumentException("Lower limit must be less than mode and mode must be less than upper limit.");
        }

		params.add(new FunctionParameter(PARAM_X_MIN, xMin));
		params.add(new FunctionParameter(PARAM_MODE, mode));
		params.add(new FunctionParameter(PARAM_X_MAX, xMax));
	}

    /**
     * Get the cumulative probability P(X <= x) for a specified x.
     *
     * @param x the integration limit
     * @return the cumulative probability
     */
	public double getCDFPoint(double x) throws ArithmeticException {
		double xMin = getXMin();
		double mode = getMode();
		double xMax = getXMax();

        if (x < xMin) {
            return 0.0;
        } else if (xMin <=x && x <= mode) {
            return ( (Math.pow(x - xMin, 2) )/( (xMax-xMin) * (mode-xMin) ) );
        } else if (mode <=x && x <= xMax) {
            return (1-( (Math.pow(xMax - x, 2) )/( (xMax-xMin) * (xMax-mode) ) ) );
        } else {
            return 1.0;
        }
	}

	/**
     * Get the probability density P(X = x).
     *
     * @param x a double value for which the pdf is required
     * @return the probability
     */
	public double getPDFPoint(double x) throws ArithmeticException {
		double xMin = getXMin();
		double mode = getMode();
		double xMax = getXMax();

		if (xMin <= x && x < mode) {
            return ( ( 2.0 * (x - xMin) ) / ( (mode-xMin) * (xMax-xMin) ) );
        } else if (mode <= x && x <= xMax) {
            return ( ( 2.0 * (xMax - x) ) / ( (xMax-mode) * (xMax-xMin) ) ) ;
        } else {
            return 0.0;
        }
	}

    /**
     * Get the inverse cumulative probability for a given P(X <= x).
     *
     * @param probability the probability
     * @return the inverse cumulative probability
     */
    public double getInverseCDFPoint(double probability) throws ArithmeticException, IllegalArgumentException {
		double xMin = getXMin();
		double mode = getMode();
		double xMax = getXMax();

        if (probability < 0.0 || probability > 1.0) {
            throw new IllegalArgumentException("Incorrect input");
        } else if (probability >= 0 && probability <= ( (mode-xMin) / (xMax-xMin) ) ) {
            return ( xMin +  Math.pow( (xMax-xMin) * (mode-xMin) * probability, 0.5) );
        } else {
            return ( xMax -  Math.pow( (xMax-xMin) * (xMax-mode) * (1 - probability), 0.5) );
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
     * @throws IllegalArgumentException if not xMin < mode < xMax
     */
	public void setParam(String paramType, double value) throws IllegalArgumentException {

		if (paramType.equalsIgnoreCase(PARAM_X_MIN)) {
	        if (value >= this.params.get(PARAM_INDEX_MODE).getValue() || value >= this.params.get(PARAM_INDEX_X_MAX).getValue()) {
	            throw new IllegalArgumentException("Lower limit must be less than mode and mode must be less than upper limit.");
	        }
	        else {
    			this.params.get(PARAM_INDEX_X_MIN).setValue(value);
	        }

		} else if (paramType.equalsIgnoreCase(PARAM_MODE)) {
	        if (value <= this.params.get(PARAM_INDEX_X_MIN).getValue() || value >= this.params.get(PARAM_INDEX_X_MAX).getValue()) {
	            throw new IllegalArgumentException("Lower limit must be less than mode and mode must be less than upper limit.");
	        }
	        else {
    			this.params.get(PARAM_INDEX_MODE).setValue(value);
	        }

		} else if (paramType.equalsIgnoreCase(PARAM_X_MAX)) {
	        if (value <= this.params.get(PARAM_INDEX_X_MIN).getValue() || value <= this.params.get(PARAM_INDEX_MODE).getValue()) {
	            throw new IllegalArgumentException("Lower limit must be less than mode and mode must be less than upper limit.");
	        }
	        else {
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
	 * Get the value of parameter mode.
	 * @return double mode.
	 */
	public double getMode() {
		return this.params.get(PARAM_INDEX_MODE).getValue();
	}

	/**
	 * Get the value of parameter xMax.
	 * @return double xMax.
	 */
	public double getXMax() {
		return this.params.get(PARAM_INDEX_X_MAX).getValue();
	}

}
