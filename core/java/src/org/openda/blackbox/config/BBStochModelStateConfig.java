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

import java.util.Collection;
import java.util.List;

/**
 * Configuration for the vectors in the parameter part, state part and prediction part of a stoch. model
 */
public class BBStochModelStateConfig {

	private Collection<BBNoiseModelConfig> noiseModelConfigs;
	private Collection<BBUncertOrArmaNoiseConfig> uncertaintyOrArmaNoiseConfigs;
    private Collection<BBStochModelVectorConfig> vectorCollection;

    public BBStochModelStateConfig(List<BBNoiseModelConfig> noiseModelConfigs,
								   Collection<BBUncertOrArmaNoiseConfig> uncertaintyOrArmaNoiseConfigs,
								   Collection<BBStochModelVectorConfig> vectorCollection) {
		this.noiseModelConfigs = noiseModelConfigs;
		this.uncertaintyOrArmaNoiseConfigs = uncertaintyOrArmaNoiseConfigs;
        this.vectorCollection = vectorCollection;
    }

	public Collection<BBNoiseModelConfig> getNoiseModelConfigs() {
		return noiseModelConfigs;
	}

	public Collection<BBUncertOrArmaNoiseConfig> getUncertaintyOrArmaNoiseConfigs() {
        return uncertaintyOrArmaNoiseConfigs;
    }

    public Collection<BBStochModelVectorConfig> getVectorCollection() {
        return vectorCollection;
    }
}
