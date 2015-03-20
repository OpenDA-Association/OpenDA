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
using System.Linq;
using System.Text;
using OpenMI.Standard2.TimeSpace;


namespace OpenDA.DotNet.OpenMI.Bridge
{
    /// <summary>
    /// For additional support for OpenMI models, this interface provides some additional methods for better/more complete support of your OpenMI model in OpenDA
    /// </summary>
    public interface ITimeSpaceComponentExtensions:ITimeSpaceComponent
    {
        /// <summary>
        /// There seems no proper support to get the cuttent time of a Model in the OpenMI2 standard. But some vendors some extension
        /// </summary>
        /// <returns></returns>
        ITime currentTime();
     
        /// <summary>
        /// For easy observation handling a model can provide some additional exhange items  especially for model predictions at observation locations
        /// </summary>
        /// <returns></returns>
        IList<OpenDA.DotNet.Interfaces.IExchangeItem> getAdditionalExchangeItem();

        /// <summary>
        /// OpenMI does not know about localization. In this method, the user can implement localization for OpenMI models in this method
        /// </summary>
        /// <param name="ExchangeItemID"></param>
        /// <param name="observationDescriptions"></param>
        /// <param name="distance"></param>
        /// <returns></returns>
        double [][] getLocalization(string ExchangeItemID, OpenDA.DotNet.Interfaces.IObservationDescriptions observationDescriptions, double distance);

        /// <summary>
        /// OpenMI does not know about observations but the model does
        /// </summary>
        /// <param name="observationDescriptions"></param>
        /// <returns>Model state interpolated to the observations</returns>
        double[] getObservedValues(OpenDA.DotNet.Interfaces.IObservationDescriptions observationDescriptions);

        // TEMPORARY - NEED A BETTER WAY!
        IList<int> CreateModelIndicesHashTable(OpenDA.DotNet.Interfaces.IObservationDescriptions observationDescriptions);

        double[] ModelValuesAtProvidedIndices(OpenDA.DotNet.Interfaces.IObservationDescriptions observationDescriptions,
                                         IList<int> indices);
    }
}
