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

/**
 * Configuration for a noise model that is part of the model state.
 */
public class BBUncertOrArmaNoiseConfig {

    public enum NoiseModelType {
        UncertainItem,
        UncertainItemWithArmaConstants,
        ArmaModel,
		Ar1Model
    }

	public enum Operation {
		Set,
		Add,
		Multiply
	}

    private String id;
    private NoiseModelType noiseModelType;
	private BBStochModelVectorConfig[] vectorConfigs;
    private String uncertainItemId;
    private double stdDev;
    private double[] armaConstants;
	private Operation operation;

    public BBUncertOrArmaNoiseConfig(String noiseModelId,
										NoiseModelType noiseModelType,
										BBStochModelVectorConfig[] vectorConfigs,
										String uncertainItemId,
										double stdDev,
										double[] armaConstants, Operation operation) {
		this.id = (noiseModelId != null) ? noiseModelId : "noise-on-" + composeNoiseModelId(vectorConfigs);
        this.noiseModelType = noiseModelType;
        this.vectorConfigs = vectorConfigs;
        this.uncertainItemId = uncertainItemId;
        this.stdDev = stdDev;
        this.armaConstants = armaConstants;
		this.operation = operation;
    }

	private String composeNoiseModelId(BBStochModelVectorConfig[] vectorConfigs) {
		if (vectorConfigs.length == 0) {
			return "no-vectors";
		}
		return vectorConfigs[0].getId() +
				(vectorConfigs.length > 1 ? "-a.o." : "");
	}

	public String getId() {
        return id;
    }

    public NoiseModelType getNoiseModelType() {
        return noiseModelType;
    }

    public BBStochModelVectorConfig[] getVectorConfigs() {
        return vectorConfigs;
    }

    public String getUncertainItemId() {
        return uncertainItemId;
    }

    public double getStdDev() {
        return stdDev;
    }

    public double[] getArmaConstants() {
        return armaConstants;
    }

	public Operation getOperation() {
		return operation;
	}
}
