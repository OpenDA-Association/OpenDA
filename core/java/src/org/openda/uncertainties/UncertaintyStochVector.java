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
package org.openda.uncertainties;

import org.openda.interfaces.ISqrtCovariance;
import org.openda.interfaces.IStochVector;
import org.openda.interfaces.ITreeVector;
import org.openda.interfaces.IVector;
import org.openda.utils.SqrtCovariance;
import org.openda.utils.TreeVector;
import org.openda.utils.Vector;

import java.util.ArrayList;

/**
 * Created by IntelliJ IDEA.
 * User: Juzer Dhondia
 * Date: 09-Mar-2010
 * Time: 11:46:37
 */
public class UncertaintyStochVector implements IStochVector {

    private UncertaintyEngine uncertaintyEngine;
    private ITreeVector parameters;
    private int realizationCounter = -1;

	public UncertaintyStochVector(UncertaintyEngine uncertaintyEngine, ITreeVector parameters, int realizationCounter) {
        this.uncertaintyEngine = uncertaintyEngine;
        this.parameters = parameters;
        this.realizationCounter = realizationCounter;
	}

	public UncertaintyStochVector(UncertaintyEngine uncertaintyEngine, ITreeVector parameters) {
		this.uncertaintyEngine = uncertaintyEngine;
		this.parameters = parameters;
	}

	public UncertaintyStochVector(UncertaintyStochVector source, int realizationCounter) {
		this.uncertaintyEngine = source.uncertaintyEngine;
		this.parameters = source.parameters;
		this.realizationCounter = realizationCounter;
	}

	public IVector createRealization() {
		if (realizationCounter == -1) {
			throw new RuntimeException("Initialization counter not yet set");
		}
		TreeVector realization = new TreeVector(parameters.getId() + "-realization");
		for (String name : parameters.getSubTreeVectorIds()) {
			double[] childValues = parameters.getSubTreeVector(name).getValues();
			double[] noise = uncertaintyEngine.getNoise(name, realizationCounter, childValues);
			realization.addChild(name, noise);
		}
		return realization;
	}

    @Override
    public double evaluatePdf(IVector tv) {
        throw new RuntimeException("UncertaintyStochVector.evaluatePdf() is not yet implemented");
    }

    @Override
    public IVector getExpectations() {
        return parameters;
    }

    @Override
    public ISqrtCovariance getSqrtCovariance() {
        return new SqrtCovariance(getStandardDeviations());
    }

    @Override
    public boolean hasCorrelatedElements() {
        throw new RuntimeException("UncertaintyStochVector.hasCorrelatedElements() is not yet implemented");
    }

    @Override
    public IVector getStandardDeviations() {
		ArrayList<String> subTreeVectorIds = parameters.getSubTreeVectorIds();
		double[] stdDevsVectorArray = new double[subTreeVectorIds.size()];
		int i = 0;
		for (String name : parameters.getSubTreeVectorIds()) {
			double stdDev = uncertaintyEngine.getStdDev(name);
			for (int j = 0; j < parameters.getSubTreeVector(name).getSize(); j++) {
				stdDevsVectorArray[i] = stdDev;
				i++;
			}
		}
		return new Vector(stdDevsVectorArray);
	}
}
