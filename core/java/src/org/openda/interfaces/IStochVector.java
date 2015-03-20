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

package org.openda.interfaces;

import java.io.Serializable;

/**
 * Stochastic tree vector interface.
 */
public interface IStochVector extends Serializable {

    /**
     * Draw a realization from the uncertainty internal to the StochVector
     * @return generated vector created, most often from pseudo-random numbers
     */
    IVector createRealization();

    /**
     * Evaluate the probability density function.
     * @param tv value for which to check pdf
     * @return Pdf value
     */
    double evaluatePdf(IVector tv);

    /**
     * Get expectation values of the uncertain Vector
     * @return expectation values
     */
    IVector getExpectations();

    /**
     * Get square-root of the covariance as an object. A square-root of the
     * covariance P is a matrix L that satisfies P=L*transpose(L). Most often working with the
     * square-root is more convenient than workin with the covariance itself.
     * @return Square-root of the covariance.
     */
    ISqrtCovariance getSqrtCovariance();

    /**
     * True if the elements of the stochastic vector are uncorrelated. You can treat the values one
     * by one in a linear context if this is so.
     * @return <code>true</code>if independent, false if dependencies exist.
     */
    boolean hasCorrelatedElements();

    /**
     * Get the standard deviation for each element of the stochastic vector.
     * @return Vector with standard deviations
     */
    IVector getStandardDeviations();

}
