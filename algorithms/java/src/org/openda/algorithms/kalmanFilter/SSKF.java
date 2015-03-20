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

/**
 * This algorithm has been replaced with the option to store the gain in the regular ensemble Kalman filters.
 * The easiest way to fix your example is to replace the algorithm org.openda.algorithms.SSKF with
 * org.openda.algorithms.kalmanFilter.SteadyStateFilter and change the configuration of this algorithm.
 */
@Deprecated
public class SSKF extends Instance implements IAlgorithm {

    public void initialize(File workingDir, String[] arguments) {
		throw new UnsupportedOperationException(
				"The SSKF algorithm has been replaced with the option to store the gain in the regular\n"
				+"ensemble Kalman filters. The easiest way to fix your example is to replace the algorithm\n"
				+"org.openda.algorithms.SSKF with org.openda.algorithms.kalmanFilter.SteadyStateFilter\n"
				+"and change the configuration of this algorithm.");
    }

	public void setStochComponents(IStochObserver stochObserver, IStochModelFactory stochModelFactory){
			throw new UnsupportedOperationException("SSKF has been removed. Please use SteadyStateFilter instead.");
    }

    public void prepare() {
		throw new UnsupportedOperationException("SSKF has been removed. Please use SteadyStateFilter instead.");
    }

	public void run() {
		throw new UnsupportedOperationException("SSKF has been removed. Please use SteadyStateFilter instead.");
	}

	public boolean hasNext() {
		throw new UnsupportedOperationException("SSKF has been removed. Please use SteadyStateFilter instead.");
	}

	public void next() {
		throw new UnsupportedOperationException("SSKF has been removed. Please use SteadyStateFilter instead.");
	}

    public IModelState saveInternalState() {
		throw new UnsupportedOperationException("SSKF has been removed. Please use SteadyStateFilter instead.");
    }

    public void restoreInternalState(IModelState savedInternalState) {
		throw new UnsupportedOperationException("SSKF has been removed. Please use SteadyStateFilter instead.");
    }

	public void releaseInternalState(IModelState savedInternalState) {
		throw new UnsupportedOperationException("SSKF has been removed. Please use SteadyStateFilter instead.");
	}

	public IModelState loadPersistentState(File algorithmStateFile) {
		throw new UnsupportedOperationException("SSKF has been removed. Please use SteadyStateFilter instead.");
	}

	public void finish() {
		throw new UnsupportedOperationException("SSKF has been removed. Please use SteadyStateFilter instead.");
	}

	public void forecast(ITime targetTime) {
		throw new UnsupportedOperationException("SSKF has been removed. Please use SteadyStateFilter instead.");
	}

	public void analysis(IStochObserver obs) {
		throw new UnsupportedOperationException("SSKF has been removed. Please use SteadyStateFilter instead.");
	}

	public ITime getTimeHorizon() {
		throw new UnsupportedOperationException("SSKF has been removed. Please use SteadyStateFilter instead.");
	}

	public ITime getCurrentTime() {
		//throw new UnsupportedOperationException("SSKF has been removed. Please use SteadyStateFilter instead.");
		return null;
	}

	public void compute(ITime targetTime) {
		throw new UnsupportedOperationException("SSKF has been removed. Please use SteadyStateFilter instead.");
	}

	public IVector getState() {
		throw new UnsupportedOperationException("SSKF has been removed. Please use SteadyStateFilter instead.");
	}
}
