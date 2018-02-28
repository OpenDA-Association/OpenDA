/* OpenDA v2.4.3 
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
package org.openda.blackbox.wrapper;

import org.openda.utils.Instance;
import org.openda.interfaces.*;

import java.io.File;

/**
 * Dummy algorithm to test black box wrapper inside openda application
 */
public class DummyTestAlgorithm extends Instance implements IAlgorithm {
    private IStochObserver stochObserver;
    private IStochModelFactory stochModelFactory;
    private final int instanceCount = 3;
    private int currentInstance = 0;

    public void initialize(File workingDir, String[] arguments) {
        // no action
    }

    public void setStochComponents(IStochObserver stochObserver, IStochModelFactory stochModelFactory) {
        this.stochObserver = stochObserver;
        this.stochModelFactory = stochModelFactory;
    }

    public void prepare() {
        // no action
    }

    public void run() {
        while (hasNext()) {
            next();
        }
    }

    public boolean hasNext() {
        return currentInstance < instanceCount;
    }

    public void next() {
        currentInstance++;
        IStochModelInstance stochModelInstance = stochModelFactory.getInstance(IStochModelFactory.OutputLevel.Suppress);
        IVector parameters = stochModelInstance.getParameters();
        IStochVector parameterUncertainty = stochModelInstance.getParameterUncertainty();
        IVector newParameters = parameters.clone();
        for (int j = 0; j < parameters.getSize(); j++) {
            newParameters.setValue(j, parameters.getValue(j) +
                            parameterUncertainty.getStandardDeviations().getValue(j) * 0.1 * instanceCount);
        }
        stochModelInstance.setParameters(newParameters);
        stochModelInstance.compute(stochModelInstance.getTimeHorizon().getEndTime());
        IVector observedValues =
                stochModelInstance.getObservedValues(stochObserver.getObservationDescriptions());
		stochModelInstance.finish();
    }

    public IModelState saveInternalState() {
        throw new UnsupportedOperationException("org.openda.blackbox.wrapper.BBApplicationTest.DummyTestAlgorithm.saveInternalState(): Not implemented yet.");
    }

    public void restoreInternalState(IModelState savedInternalState) {
        throw new UnsupportedOperationException("org.openda.blackbox.wrapper.BBApplicationTest.DummyTestAlgorithm.restoreInternalState(): Not implemented yet.");
    }

	public void releaseInternalState(IModelState savedInternalState) {
		throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyTestAlgorithm.releaseInternalState(): Not implemented yet.");
	}

	public IModelState loadPersistentState(File algorithmStateFile) {
		throw new UnsupportedOperationException("org.openda.blackbox.wrapper.DummyTestAlgorithm.loadPersistentState(): Not implemented yet.");
	}

	public void finish() {
		// no action needed
	}

	public IVector getState() {
		// TODO Auto-generated method stub
		return null;
	}

	public ITime getTimeHorizon() {
		// TODO Auto-generated method stub
		return null;
	}

	public ITime getCurrentTime() {
		// TODO Auto-generated method stub
		return null;
	}

	public void compute(ITime targetTime) {
		// TODO Auto-generated method stub
	}
}
