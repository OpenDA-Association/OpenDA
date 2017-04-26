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

package org.openda.observationOperators;

import org.openda.interfaces.*;

/**
 * Created with IntelliJ IDEA.
 * User: dirk
 * Date: 1-6-16
 * Time: 15:05
 * Observation Operator H
 *
 */
public class ObservationOperatorDeprecatedModel implements IObservationOperator {

	private IStochModelInstanceDeprecated ModelInstance;

	/**
	 * Get the observed values of the Model.
	 * This returns what the observations would look like, if reality would be equal to the current model state.
	 *
	 * @param model observation description
	 * @return Model prediction interpolated to each observation (location).
	 */
	public ObservationOperatorDeprecatedModel(IStochModelInstanceDeprecated model) {
		ModelInstance = model;
	}

	/**
	 * Get the observed values of the Model.
	 * This returns what the observations would look like, if reality would be equal to the current model state.
	 *
	 * @param observationDescriptions observation description
	 * @return Model prediction interpolated to each observation (location) as provided by the StochModel.
	 */
	public IVector getObservedValues(IObservationDescriptions observationDescriptions)	{
		return ModelInstance.getObservedValues(observationDescriptions);
	}

	/**
	 * Get the observed values of the Model.
	 * This returns what the observations would look like, if reality would be equal to the current model state.
	 *
	 * @param observationDescriptions observation description
	 * @param iDomain localization domain
	 * @return Model prediction interpolated to each observation (location) as provided by the StochModel.
	 */
	public IVector getObservedValues(IObservationDescriptions observationDescriptions, int iDomain)	{
		//throw new UnsupportedOperationException("Method getObservedValues(IObservationDescriptions observationDescriptions, int iDomain) not implemented."+this.getClass().getName());

        // Theo:  Temporary solution, for each iDomain we transmit all the observations
        return ModelInstance.getObservedValues(observationDescriptions);
	}

}
