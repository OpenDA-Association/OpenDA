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
 * Interface to specify the functions for a stochastic model.
 */
public interface IStochModelInstanceDeprecated{
    /**************************************
     *** Matching to observations
     *    TODO How to handle missing values.
     *    TODO How to handle subselection within a model
     **************************************/

    /**
     * Get model values corresponding to a number of observations
     *
     * @param observationDescriptions An ObservationDescriptions object with meta data for the observations
     * @return vector with the model values corresponding to each observation given in the descriptions
     */

    IVector getObservedValues(IObservationDescriptions observationDescriptions);


}

