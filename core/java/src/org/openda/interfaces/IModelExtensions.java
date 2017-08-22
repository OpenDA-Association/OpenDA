/* OpenDA v2.4.1 
* Copyright (c) 2017 OpenDA Association 
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

/**
 * Created with IntelliJ IDEA.
 * User: nils
 * Date: 6-3-13
 * Time: 13:31
 * Extensions to the IModelInterface to optimize/customize the behaviour of Models without braking old models
 *
 * Important note:
 * This is only used in the BBStochModelInstance.
 */
public interface IModelExtensions {
	/**
	 * Returns the localization weights for each observation location.
	 *
	 * @param stateExchangeItemID id of the state vector for which the localization weights should be returned.
	 * @param observationDescriptions observation description
	 * @param distance characteristic distance for Cohn's formula
	 * @return weight vector for each observation location.
	 *         The size of the returned array must equal the number of observation locations in the given observationDescriptions.
	 *         The size of each vector in the returned array must equal the size of the state vector with the given stateExchangeItemID.
	 */
	//Note: this method is in principle not related to method getObservedValues.
	public IVector[] getObservedLocalization(String stateExchangeItemID, IObservationDescriptions observationDescriptions, double distance);

	/**
	 * Tell model that it can expect to be asked for model values corresponding to the observations
	 * described. The model can make arrangement to save these values. The method compute run over a long
	 * interval at once, not stopping at each time with observations. This is meant to increase the performance
	 * especially of calibration algorithms.
	 *
	 * @param observationDescriptions An ObservationDescriptions object with meta data for the observations
	 */
	void announceObservedValues(IObservationDescriptions observationDescriptions);

	/**
	 * Get the observed values of the Model.
	 * This returns what the observations would look like, if reality would be equal to the current model state.
	 *
	 * @param observationDescriptions observation description
	 * @return Model prediction interpolated to each observation (location).
	 */
	//Note: this method is in principle not related to method getObservedLocalization.
	public IVector getObservedValues(IObservationDescriptions observationDescriptions);
}
