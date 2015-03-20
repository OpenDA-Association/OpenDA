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
package org.openda.blackbox.config;
import java.util.ArrayList;
import java.util.List;

import org.openda.blackbox.config.BBUncertOrArmaNoiseConfig.Operation;

/**
 * Config. info for an exchange item in a noise model or an uncertainty module
 */
public class NoiseModelExchangeItemConfig {

	private String id;
	private List<String> modelExchangeItemIds;
	private BBUncertOrArmaNoiseConfig.Operation operation;
	private int transformation;

	
	public NoiseModelExchangeItemConfig(String id, List<String> modelExchangeItemIds,
			BBUncertOrArmaNoiseConfig.Operation operation, int transformation) {
		this.id = id;		
		this.modelExchangeItemIds = modelExchangeItemIds;
		this.operation = operation;
		this.transformation = transformation;
	}
	
	public NoiseModelExchangeItemConfig(String id, String modelExchangeItemId,
										BBUncertOrArmaNoiseConfig.Operation operation, int transformation) {	
		
		// call default constructor with empty modelExchangeItemIds list
		this(id, new ArrayList<String>(), operation, transformation);
		// add modelExchangeItemId if defined
		if (modelExchangeItemId != null) {
			this.modelExchangeItemIds.add(modelExchangeItemId);

		} else {
			throw new RuntimeException("modelExchangeItemId is not defined");
		}
	}
	
	/**
	 *@deprecated Please use NoiseModelExchangeItemConfig(String id, List<String> modelExchangeItemIds,
			BBUncertOrArmaNoiseConfig.Operation operation, int transformation)
	 *@see NoiseModelExchangeItemConfig(String id, List<String> modelExchangeItemIds,
			BBUncertOrArmaNoiseConfig.Operation operation, int transformation)
	 */
	public NoiseModelExchangeItemConfig(String id, String modelExchangeItemId, List<String> modelExchangeItemIds,
			BBUncertOrArmaNoiseConfig.Operation operation, int transformation) {
		
		// call default constructor with empty modelExchangeItemIds list
		this(id, new ArrayList<String>(), operation, transformation);
		// implement deprecated constructors behavior
		// if modelExchangeItemId is defined use this,
		// otherwise use the list modelExchangeItemIds
		if (modelExchangeItemId != null) {
			this.modelExchangeItemIds.add(modelExchangeItemId);

		} else {
			this.modelExchangeItemIds = modelExchangeItemIds;
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
}
