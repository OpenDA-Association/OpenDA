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

import java.io.File;

/**
 * Model Instance interface.
 * This interface specifies the functions for a deterministic model.
 */
public interface IModelInstance extends IDataObject, IInstance {

	/*************************************
	 * Exchange items (to be replaced by the IDataObject.getExchangeItem once al io-objects and exchange items
	 * have been migrated to the new IDataObject / IExchangeItem approach.
	 *************************************/

	/**
	 * Get the exchange item specified by <c>exchangeItemID</c>.
	 *
	 * @param exchangeItemID The exchange item identifier.
	 * @return The required exchange item.
	 */
	IPrevExchangeItem getExchangeItem(String exchangeItemID);

	/*************************************
	 *** Time information / Computing
	 *************************************/

    /**
     * Get the computational time horizon of the model (begin and end time).
     *
     * @return The time horizon (containing begin and end time).
     */
    ITime getTimeHorizon();

    /**
     * Get the stochastic model instance's current simulation time stamp.
     *
     * @return The model's current simulation time stamp.
     */
    ITime getCurrentTime();

    /**
     * Let the stochastic model instance compute to the requested target time stamp.
     * This function can not be used to go back in time. Use saveState and restoreState instead.
     * Use Infinity if the targetTime is unknown
     *
     * @param targetTime Time stamp to compute to.
     */
    void compute(ITime targetTime);

    /**
	 * Returns the localization weights for each observation location.
	 * This method assumes that there is only one state vector.
	 *
	 * @param observationDescriptions observation description
	 * @param distance characteristic distance for Cohn's formula
	 * @return weight vector for each observation location.
	 *         The size of the returned array must equal the number of observation locations in the given observationDescriptions.
	 *         The size of each vector in the returned array must equal the size of the state vector of the implementing modelInstance.
     */
    IVector[] getObservedLocalization(IObservationDescriptions observationDescriptions, double distance);

    /**************************************
     *** Save/restore full internal state
     **************************************/

    /**
     * Save the current state of the model to file or otherwise.
     * The implementation is model dependent and may do much more than write the state to disk.
	 *
	 * Ask the model to store its current state (either to file or in memory) for future reference
	 * and return a ModelState object that is just an identifier for the stored state.
	 *
	 * @return modelState object that refers to the saved state.
	 */
    IModelState saveInternalState();

    /**
     * Restore a previously saved state of the model.
     * The implementation is model dependent and may do much more than read the state from disk.
	 *
	 * Set the model instance's current state to the state identified by the given ModelState object.
	 *
	 * @param savedInternalState handle to a (previously saved) state to be restored.
	 */
    void restoreInternalState(IModelState savedInternalState);

    /**
     * Release resources used to save a state at some earlier time.
     * The implementation is model dependent and may for example remove a file from disk.
	 *
	 * Ask the model to delete the file/memory of the state identified by the given ModelState object.
	 *
	 * @param savedInternalState handle to the (previously saved) state to be released.
	 */
    void releaseInternalState(IModelState savedInternalState);

	/**
	 * Load an internal model state from file.
	 *
	 * Ask the model to internally store the state from the given file,
	 * this can be stored in file or in memory. This does not change the model's current state,
	 * this only stores the state and returns a ModelState object that is just an identifier
	 * for the stored state. Updating the current model state can be done after this
	 * by calling the restoreInternalState method.
	 * This method is the inverse of method IModelState.savePersistentState.
	 *
	 * @param persistentStateFile file to read state from.
	 * @return modelState object that refers to the saved state.
	 */
	IModelState loadPersistentState(File persistentStateFile);

	/**
     * Return the directory where the instance runs.
     * @return Directory with result files.
     */
    File getModelRunDir();

}
