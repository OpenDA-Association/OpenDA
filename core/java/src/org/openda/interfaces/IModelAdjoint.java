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

/**
 * Model instance interface for the adjoint part of a model
 */
public interface IModelAdjoint {

    /**
     * Get the resulting delta in the model values that correspond to a number of observations, given
     * a modification of the model state (delta).
     *
     * @param deltaState          The delta on the state.
     * @param observationMetaData The observation items for which the tangent has to be computed.
     *
     * @return resulting delta in the model values corresponding to each observation given in the descriptions
     */
    IVector applyObservationTangent(IVector deltaState, IObservationDescriptions observationMetaData);

    /**
     * Get the resulting adjoint model values that correspond to a number of lamby values for the observations.
     *
     * @param lambaY       The lambdaY adjoint values for the observations, for which the 
     *                     lambaX vector for the model state has to be computed.
     * @param observationMetaData The observation items for which the tangent has to be computed.
     * @return The lambaX vector for the state
     */
    IVector applyObservationAdjoint(IVector lambaY, IObservationDescriptions observationMetaData);

}
