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
package org.openda.blackbox.config;
import org.openda.blackbox.config.BBUncertOrArmaNoiseConfig.Operation;

import java.util.ArrayList;
import java.util.List;

/**
 * Config. info for an exchange item in a noise model or an uncertainty module
 */
public class NoiseModelExchangeItemConfig {

	private String id;
	private List<String> modelExchangeItemIds;
	private BBUncertOrArmaNoiseConfig.Operation operation;
	private int transformation;
	private boolean skipFirstTimeStep;
	private boolean addOnlyNoiseDifference;
	private int stateSizeNoiseSizeRatio;
    private boolean addStateNoiseAfterCompute;


    public NoiseModelExchangeItemConfig(String id, List<String> modelExchangeItemIds,
                                        Operation operation, int transformation, boolean skipFirstTimeStep, boolean addOnlyNoiseDifference, int stateSizeNoiseSizeRatio, boolean addStateNoiseAfterCompute) {
		this.id = id;		
		this.modelExchangeItemIds = modelExchangeItemIds;
		this.operation = operation;
		this.transformation = transformation;
		this.skipFirstTimeStep = skipFirstTimeStep;
		this.addOnlyNoiseDifference = addOnlyNoiseDifference;
		this.stateSizeNoiseSizeRatio = stateSizeNoiseSizeRatio;
		this.addStateNoiseAfterCompute = addStateNoiseAfterCompute;
	}
	
	public NoiseModelExchangeItemConfig(String id, String modelExchangeItemId,
										Operation operation, int transformation, boolean skipFirstTimeStep, boolean addOnlyNoiseDifference) {
		
		// call default constructor with empty modelExchangeItemIds list
		this(id, new ArrayList<String>(), operation, transformation, skipFirstTimeStep, addOnlyNoiseDifference, 1, false);
		// add modelExchangeItemId if defined
		if (modelExchangeItemId != null) {
			this.modelExchangeItemIds.add(modelExchangeItemId);

		} else {
			throw new RuntimeException("modelExchangeItemId is not defined");
		}
	}
	
	public String getId() {
		return id;
	}

	public List<String> getModelExchangeItemIds() {
		return modelExchangeItemIds;
	}

	public BBUncertOrArmaNoiseConfig.Operation getOperation() {
		return operation;
	}

	public int getTransformation() {
		return transformation;
	}

	public boolean doSkipFirstTimeStep() {
		return skipFirstTimeStep;
	}

	public boolean doAddOnlyNoiseDifference() {
		return addOnlyNoiseDifference;
	}

    public int getStateSizeNoiseSizeRatio() {
        return stateSizeNoiseSizeRatio;
    }

    public boolean isAddStateNoiseAfterCompute() {
        return addStateNoiseAfterCompute;
    }
}
