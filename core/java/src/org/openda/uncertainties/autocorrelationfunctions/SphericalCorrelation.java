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

package org.openda.uncertainties.autocorrelationfunctions;

import org.openda.uncertainties.FunctionParameter;

/**
 * Class implementing a correlation model with a spherical structure
 * for autocorrelation.
 *
 * This class is derived from DUE version 3.0 by brown@science.uva.nl
 */
public class SphericalCorrelation extends AutoCorrelationFunction {

	//distribution name.
	private static final String DISTRIBUTION_NAME = "Spherical";


	/**
	 * Returns a new instance of this correlation object for cloning purposes.
	 *
	 * @return new instance of SphericalCorrelation.
	 */
	public SphericalCorrelation newInstance() {
        return new SphericalCorrelation();
    }

	/**
	 * Construct a spherical correlation model with default values for the parameters.
	 */
	public SphericalCorrelation() {
		super();

		params.add(new FunctionParameter(PARAM_SILL, DEFAULT_SILL));
		params.add(new FunctionParameter(PARAM_RANGE, DEFAULT_RANGE));
	}

	/**
	 * Construct an isotropic spherical correlation model with a sill and range as input parameters
	 *
	 * @param sill the sill parameter
	 * @param range the range parameter
	 * @throws IllegalArgumentException if range <= 0
	 */
	public SphericalCorrelation(double sill, double range) throws IllegalArgumentException {
		super();

		if (range <= 0.0) {
			throw new IllegalArgumentException("The range parameter must be positive.");
		}

		params.add(new FunctionParameter(PARAM_SILL, sill));
		params.add(new FunctionParameter(PARAM_RANGE, range));

		orderOfAnisotropy = ISOTROPIC;
	}

	/**
	 * Evaluates this isotropic correlation model for the given lag distance.
	 *
	 * @param lagDistance the lag distance between points
	 * @return the value of the function
     * @throws ArithmeticException
     * @throws IllegalArgumentException if orderOfAnisotropy != ISOTROPIC
	 */
	public double evaluateFunction(double lagDistance) throws ArithmeticException, IllegalArgumentException {
		double sill = this.getSill();
		double range = this.getRange();

        //The model is isotropic
        if (orderOfAnisotropy == ISOTROPIC) {
            if (lagDistance < range) {
            	return sill * ( 1.0-(1.0-( (3.0 * lagDistance)/(2.0 * range) ) + ( 0.5 * Math.pow(lagDistance/range, 3) ) ) );
            }
            else {
            	return sill * 1.0;
            }
        }
        else {
			throw new IllegalArgumentException("Error: the correlation model is two-dimensional. Please specify two- or three-dimensional coordinates.");
        }
	}

    /**
     * Method returns lower limit on horizontal axis for correlation model graph plotting.
     * @return lower limit
     */
	public double getGraphXLowerLimit() {
        return this.graphXLowerLimit;
	}

    /**
     * Method returns upper limit on horizontal axis for correlation model graph plotting.
     * @return upper limit
     */
	public double getGraphXUpperLimit() {
        return this.getRange()*this.graphXUpperLimitFactor;
	}

	/**
	 * Get the name of this correlation model.
	 * @return String name of this correlation model.
	 */
	public String toString() {
		return DISTRIBUTION_NAME;
	}
}
