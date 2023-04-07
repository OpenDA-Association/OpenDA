/*
* Copyright (c) 2023 OpenDA Association 
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
 * User: dirk
 * Date: 1-6-16
 * Time: 15:05
 * Observation Operator H
 *
 */
public interface IObservationOperator{
	/**
	 * Get the observed values of the Model.
	 * This returns what the observations would look like, if reality would be equal to the current model state.
	 *
	 * @param observationDescriptions observation description
	 * @return Model prediction interpolated to each observation (location).
	 */
	//Note: this method is in principle not related to method getObservedLocalization.
	IVector getObservedValues(IObservationDescriptions observationDescriptions);

	/**
	 * Get the observed values of the Model.
	 * This returns what the observations would look like, if reality would be equal to the current model state.
	 *
	 * @param observationDescriptions observation description
	 * @param iDomain localization domain
	 * @return Model prediction interpolated to each observation (location).
	 */
	//Note: this method is in principle not related to method getObservedLocalization.
	IVector getObservedValues(IObservationDescriptions observationDescriptions, int iDomain);
}
