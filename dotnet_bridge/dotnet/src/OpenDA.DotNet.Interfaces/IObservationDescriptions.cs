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


using System;
using System.Collections.Generic;

namespace OpenDA.DotNet.Interfaces
{
    /**
    * Observation Descriptions
    */
    public interface IObservationDescriptions
    {
        /// <summary>
        /// Get the exchange items describing the measures available in the stoch. observer.
        /// </summary>
        /// <returns>All exchange items in the stoch. observer.</returns>
        List<IExchangeItem> ExchangeItems { get; }

        /// <summary>
        /// Get properties (values) that correspond to a given key.
        /// </summary>
        /// <param name="Key">I  key for which the value is asked</param>
        /// <returns>Properties (column of data from observation descriptions)</returns>
        IVector GetValueProperties(String Key);

        /// <summary>
        /// Get properties (strings) that correspond to a given key.
        /// </summary>
        /// <param name="Key">I  key for which the value is asked</param>
        /// <returns>Properties (column of data from observation descriptions)</returns>
        String[] GetStringProperties(String Key);

        /// <summary>
        /// Get names of all keys.
        /// </summary>
        /// <returns>error status: All keys of the observation descriptions</returns>
        String[] PropertyKeys { get; }

        /// <summary>
        /// Get number of properties/keys.
        /// </summary>
        /// <returns>number of properties </returns>
        int PropertyCount { get; }

        /// <summary>
        /// Get number of observations.
        /// </summary>
        /// <returns>number of observations</returns>
        int ObservationCount { get; }

        /// <summary>
        /// Get all different times in increasing order. There is at least one observation for each time.
        /// It is likely that observer.createSelection(time[i]) will be used to walk through the
        /// observations. The implementation of the stochobserver should garantee that al observations are
        /// returned in exactly one batch this way.
        /// </summary>
        /// <returns>array with all uniquely different times.</returns>
        ITime[] Times { get; }
    }
}
