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

namespace OpenDA.DotNet.Interfaces
{
	public interface IModelInstance : IInstance
	{
		//
		// Time information / Computing
		//
		
		/// <summary>
		/// Get the computational time horizon of the model (begin and end time). 
		/// </summary>
		ITime TimeHorizon { get; }

		/// <summary>
		/// Get the model instance's current simulation time stamp. 
		/// </summary>
		ITime CurrentTime { get; }

		/// <summary>
		/// Let the model instance compute to the requested target time stamp.
		/// This function can not be used to go back in time. Use saveState and restoreState instead.
		/// </summary>
		/// <param name="targetTime"></param>
		void Compute(ITime targetTime);

		//
		// Exchanging data items between models
		//

		/// <summary>
		/// Get the identifiers of the exchange items that can be retreived from and set to the model.
		/// </summary>
		string[] ExchangeItemIDs { get; }

		/// <summary>
		/// Get the identifiers of the exchange items that can be retreived from and set to the model,
		/// according to the specified role (input, output, both)
		/// </summary>
		/// <param name="roleAsInt">Input (0), Output (1), or InOut (2, i.e. both)</param>
		/// <returns>The array of exchange item identifiers.</returns>
		string[] GetExchangeItemIDs(int roleAsInt);

		/// <summary>
		/// Get the exchange item specified by <c>exchangeItemID</c>.
		/// </summary>
		/// <param name="exchangeItemID">The exchange item identifier.</param>
		/// <returns>The requested exchange item.</returns>
		IExchangeItem GetExchangeItem(string exchangeItemID);

		//
		//Save/restore full internal state
		//

		/// <summary>
		/// Save the current state of te model to file or otherwise.
		/// The implementation is modeldependent and may do much more than write the state to disk.
		/// </summary>
		/// <returns>Handle referring to the saved state.</returns>
		IModelState SaveInternalState();

		/// <summary>
		/// Restore a previously saved state of te model.
		/// The implementation is modeldependent and may do much more than read the state from disk.
		/// </summary>
		/// <param name="savedInternalState">Handle to the (previously saved) state to be restored.</param>
		void RestoreInternalState(IModelState savedInternalState);

		/// <summary>
		/// Release resources used to save a state at some earlier time.
		/// The implementation is modeldependent and may do much more than write the state to disk.
		/// </summary>
		/// <param name="savedInternalState">Handle to the (previously saved) state to be released.</param>
		void ReleaseInternalState(IModelState savedInternalState);

		/**
		 * Load an internal model state from file
		 *
		 * @param algorithmStateFile File to read state from
		 * @return The model state read from file
		 */
		IModelState LoadPersistentState(String algorithmStateFilePath);

        /**
         * Get the localization vector
         * @param observationDescriptions observation description
         * @param distance characteristic distance for Cohn's formula
         * @return weight vector for each observation location.
         */
        IVector[] GetObservedLocalization(IObservationDescriptions observationDescriptions, double distance);

        /**
         * Interpolate model state to observations
         * @param observationDescriptions observation description
         * @return interpolated model state
         */
        IVector GetObservedValues(IObservationDescriptions observationDescriptions);
        /**
         * Get the localization vector
         * @param exchangeItemID, exchangeItemID for which we want the localization vector
         * @param observationDescriptions observation description
         * @param distance characteristic distance for Cohn's formula
         * @return weight vector for each observation location.
         */
        IVector[] GetObservedLocalization(String exchageItemID, IObservationDescriptions observationDescriptions, double distance);

        /// <summary>
		/// Get the directory where the instance runs
		/// </summary>
		string ModelRunDirPath { get;  }

		/// <summary>
		/// Tell the model instance that it will never be called again, so it can perform its finalization actions
		/// </summary>
		void Finish();
	}
}