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
package org.openda.algorithms.kalmanFilter;
import org.openda.interfaces.*;
import org.openda.utils.*;
import java.io.File;
import java.io.IOException;

/**
 * This algorithm has been replaced with the option to store the gain in the regular ensemble Kalman filters.
 * The easiest way to fix your example is to replace the algorithm org.openda.algorithms.EnSSKF with
 * org.openda.algorithms.kalmanFilter.EnKF and add the saveGain option to the configuration of this algorithm.
 */
@Deprecated
public class EnSSKF extends Instance implements IAlgorithm {


    public void initialize(File workingDir, String[] arguments) {
		throw new UnsupportedOperationException(
				" The EnSSKF algorithm has been replaced with the option to store the gain in the regular ensemble Kalman\n"
				+"filters. The easiest way to fix your example is to replace the algorithm org.openda.algorithms.EnSSKF\n"
				+"with org.openda.algorithms.kalmanFilter.EnKF and add the saveGain option to the configuration of this\n"
				+"algorithm.");
    }

    /**
	 * Intialization function
	 */
	public void setStochComponents(IStochObserver stochObserver, IStochModelFactory stochModelFactory){
		throw new UnsupportedOperationException("EnSSKF has been removed. Please use EnKF instead.");
    }

    public void prepare() {
		throw new UnsupportedOperationException("EnSSKF has been removed. Please use EnKF instead.");
    }

    /**
	 * Main routine this method is called to start the computation
	 */
	public void run() {
		throw new UnsupportedOperationException("EnSSKF has been removed. Please use EnKF instead.");
	}

	/**
	 * Are there any more steps for this algorithm
	 *
	 * @return has next step
	 */
	public boolean hasNext() {
		throw new UnsupportedOperationException("EnSSKF has been removed. Please use EnKF instead.");
	}

	/**
	 * Run next step of the algorithm
	 */
	public void next() {
		throw new UnsupportedOperationException("EnSSKF has been removed. Please use EnKF instead.");
	}

    public IModelState saveInternalState() {
		throw new UnsupportedOperationException("EnSSKF has been removed. Please use EnKF instead.");
    }

    public void restoreInternalState(IModelState savedInternalState) {
		throw new UnsupportedOperationException("EnSSKF has been removed. Please use EnKF instead.");
    }

	public void releaseInternalState(IModelState savedInternalState) {
		throw new UnsupportedOperationException("EnSSKF has been removed. Please use EnKF instead.");
	}

	public IModelState loadPersistentState(File algorithmStateFile) {
		throw new UnsupportedOperationException("EnSSKF has been removed. Please use EnKF instead.");
	}

	public void finish() {
		throw new UnsupportedOperationException("EnSSKF has been removed. Please use EnKF instead.");
	}

	public void forecast(ITime targetTime) {
		throw new UnsupportedOperationException("EnSSKF has been removed. Please use EnKF instead.");
	}

	public void analysis(IStochObserver obs) throws IOException {
		throw new UnsupportedOperationException("EnSSKF has been removed. Please use EnKF instead.");
    }

	@Override
	public ITime getTimeHorizon() {
		throw new UnsupportedOperationException("EnSSKF has been removed. Please use EnKF instead.");
	}

	@Override
	public ITime getCurrentTime() {
		//throw new UnsupportedOperationException("EnSSKF has been removed. Please use EnKF instead.");
		return null;
	}

	@Override
	public void compute(ITime targetTime) {
		throw new UnsupportedOperationException("EnSSKF has been removed. Please use EnKF instead.");
	}

	@Override
	public IVector getState() {
		throw new UnsupportedOperationException("EnSSKF has been removed. Please use EnKF instead.");
	}
}
