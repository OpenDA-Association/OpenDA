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

import java.io.File;

/**
 * Interface to the data assimilation or parameter calibration algorithm.
 */
public interface IAlgorithm extends IConfigurable, IInstance {

    /**
     * List of available algorithm types.
     */
    public static enum Type {
        Unknown,
        Filtering,
        Calibration
    }

	void initialize(File workingDir, String[] arguments);

    /**
     * Set the components that the algorithm will work with (the stochastic observer and the stochmodel factory)
     * @param stochObserver The stochastic observer
     * @param stochModelFactory The factory that will create the stochastic model instances
     */
	void setStochComponents(
            IStochObserver stochObserver,
            IStochModelFactory stochModelFactory
    );

    /**
     * Let the algorithm prepare itself
     */
    void prepare();

	/**
	* Run whole algorithm at once. No interuption is facilitated.
 	* This is equivalent to
 	* while(algorithm.hasNext(){algorithm.next();}
	*/
	void run();

	/**
	 * Are there any more steps for this algorithm.
	 * @return has next step
	 */
	boolean hasNext();
	
	/**
     * Run next step of the algorithm.
	 */
	void next();
	
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
     *
     * @param targetTime Time stamp to compute to.
     */
    void compute(ITime targetTime);



    /**************************************
     *** Save/restore full internal state
     **************************************/

    /**
     * Save the current state of te model to file or otherwise.
     * The implementation is model dependent and may do much more than write the state to disk.
     *
     * @return Handle referring to the saved state.
     */
    IModelState saveInternalState();

    /**
     * Restore a previously saved state of te model.
     * The implementation is model dependent and may do much more than read the state from disk.
     *
     * @param savedInternalState Handle to the (previously saved) state to be restored.
     */
    void restoreInternalState(IModelState savedInternalState);

	/**
	 * Release resources used to save a state at some earlier time.
	 * The implementation is model dependent and may for example remove a file from disk.
	 *
	 * @param savedInternalState Handle to the (previously saved) state to be restored.
	 */
	void releaseInternalState(IModelState savedInternalState);

	/**
     * Read the algorithm state from file
     * @param algorithmStateFile File to read state from
     * @return The algorithm state read from file
     */
    IModelState loadPersistentState(File algorithmStateFile);


	/**
	 * Tell the algorithm that it will never be called again, so it can perform its finalization actions
	 */
	void finish();

	//TODO temporary solution
    /**
     * Get the full set of state variables from a model.
     *
     * @return A Vector containing the values of all state variables in the model.
     */
    IVector getState();

}
