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
 * Class implementing a correlation model with a periodic structure
 * for autocorrelation.
 *
 * This class is derived from DUE version 3.0 by brown@science.uva.nl
 */
public class PeriodicCorrelation extends AutoCorrelationFunction {

	//distribution name.
	private static final String DISTRIBUTION_NAME = "Periodic";


	/**
	 * Returns a new instance of this correlation object for cloning purposes.
	 *
	 * @return new instance of PeriodicCorrelation.
	 */
	public PeriodicCorrelation newInstance() {
        return new PeriodicCorrelation();
    }

	/**
	 * Construct a periodic correlation model with default values for the parameters.
	 */
	public PeriodicCorrelation() {
		super();

		params.add(new FunctionParameter(PARAM_SILL, DEFAULT_SILL));
		params.add(new FunctionParameter(PARAM_RANGE, DEFAULT_RANGE));
	}

	/**
	 * Construct an isotropic periodic correlation model with a sill and range as input parameters
	 *
	 * @param sill the sill parameter
	 * @param range the range parameter
	 * @throws IllegalArgumentException if range <= 0
	 */
	public PeriodicCorrelation(double sill, double range) throws IllegalArgumentException {
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
            return sill * ( (1.0-Math.cos( ((2.0 * Math.PI * lagDistance) / range)*(Math.PI/180) ) )/2.0 );
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
     * Custom upper limit for periodic correlation model.
     *
     * @return upper limit
     */
	public double getGraphXUpperLimit() {
        return 10*Math.PI*this.getRange()*this.graphXUpperLimitFactor;
	}

	/**
	 * Get the name of this correlation model.
	 * @return String name of this correlation model.
	 */
	public String toString() {
		return DISTRIBUTION_NAME;
	}
}
