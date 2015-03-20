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
 * Class implementing a correlation model with a nugget structure for autocorrelation.
 * The nugget implies that no correlation occurs above lag distance = 0
 * (i.e. the data are not correlated).
 *
 * This class is derived from DUE version 3.0 by brown@science.uva.nl
 */
public class NuggetCorrelation extends AutoCorrelationFunction {

	//distribution name.
	private static final String DISTRIBUTION_NAME = "Nugget";


	/**
	 * Returns a new instance of this correlation object for cloning purposes.
	 *
	 * @return new instance of NuggetCorrelation.
	 */
	public NuggetCorrelation newInstance() {
        return new NuggetCorrelation();
    }

	/**
	 * Construct a nugget correlation model with a default value for
	 * the sill parameter.
	 * Nugget correlation does not use the range parameter, since
	 * nugget correlation should always have range = 0, which
	 * is returned by the overridden getRange() method.
     */
	public NuggetCorrelation() {
		super();

		params.add(new FunctionParameter(PARAM_SILL, DEFAULT_SILL));
	}

	/**
	 * Construct an isotropic nugget correlation model with a sill and range
	 * as input parameters.
	 * Nugget correlation does not use the range parameter, since
	 * nugget correlation should always have range = 0, which
	 * is returned by the overridden getRange() method.
	 *
	 * @param sill the sill parameter
	 */
	public NuggetCorrelation(double sill) {
		super();

		params.add(new FunctionParameter(PARAM_SILL, sill));

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

        if (orderOfAnisotropy == ISOTROPIC) {
        	if (lagDistance == 0) {
    	        return 0.0;
        	}
        	else {
    	        return sill;
        	}
		}
		else {
			throw new IllegalArgumentException("Error: the correlation model is two-dimensional. Please specify two- or three-dimensional coordinates.");
		}
	}

	/**
     * Set a specified parameter.
     * Overrides the setParam method of the CorrelationModel class to ensure
     * that range is always 0 for the nugget correlation model.
     *
     * @param paramType paramType the name of the parameter
     * @param value value of the parameter
     * @throws IllegalArgumentException
     */
	public void setParam(String paramType, double value) throws IllegalArgumentException {

		if (paramType.equalsIgnoreCase(PARAM_SILL)) {
			this.params.get(PARAM_INDEX_SILL).setValue(value);

		} else {
			throw new IllegalArgumentException("Invalid parameter type supplied.");
		}
	}

    /**
     * Method returns lower limit on horizontal axis for correlation model graph plotting.
     * Custom lower limit for nugget correlation model.
     *
     * @return lower limit
     */
    public double getGraphXLowerLimit() {
        return 0.0;
	}

    /**
     * Method returns upper limit on horizontal axis for correlation model graph plotting.
     * Custom upper limit for nugget correlation model.
     *
     * @return upper limit
     */
    public double getGraphXUpperLimit() {
        return 1.0*this.graphXUpperLimitFactor;
	}

	/**
	 * Get the value of parameter range.
     * Overrides the getRange() method of the superclass to ensure
     * that range is always 0 for the nugget correlation model.
     *
	 * @return double range.
	 */
	public double getRange() {
		return 0.0;
	}

	/**
	 * Get the name of this correlation model.
	 * @return String name of this correlation model.
	 */
	public String toString() {
		return DISTRIBUTION_NAME;
	}
}
