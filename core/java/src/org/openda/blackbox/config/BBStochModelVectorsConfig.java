/* OpenDA v2.4 
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

import java.util.Collection;
import java.util.List;

/**
 * Configuration for the vectors in the parameter part, state part and prediction part of a stoch. model
 */
public class BBStochModelVectorsConfig {

	private List<BBNoiseModelConfig> paramsUncertaintyModelConfigs;
    private Collection<BBRegularisationConstantConfig> regularisationConstantCollection;
    private Collection<BBCartesianToPolarConfig> cartesianToPolarCollection;
    private BBStochModelStateConfig stateConfig;
    private Collection<BBStochModelVectorConfig> predictorVectorCollection;
    private final RangeValidationConstraint[] rangeValidationConstraints;
    private boolean collectPredictorTimeSeries;

    public BBStochModelVectorsConfig(List<BBNoiseModelConfig> paramsUncertaintyModelConfigs,
                                     Collection<BBRegularisationConstantConfig> regularisationConstantCollection,
                                     Collection<BBCartesianToPolarConfig> cartesianToPolarCollection,
                                     BBStochModelStateConfig stateConfig,
                                     Collection<BBStochModelVectorConfig> predictorVectorCollection,
                                     boolean collectPredictorTimeSeries, RangeValidationConstraint[] rangeValidationConstraints) {
		this.paramsUncertaintyModelConfigs = paramsUncertaintyModelConfigs;
		this.regularisationConstantCollection = regularisationConstantCollection;
        this.cartesianToPolarCollection = cartesianToPolarCollection;
        this.stateConfig = stateConfig;
        this.predictorVectorCollection = predictorVectorCollection;
        this.collectPredictorTimeSeries = collectPredictorTimeSeries;
        this.rangeValidationConstraints = rangeValidationConstraints;
    }

	public List<BBNoiseModelConfig> getParamsUncertaintyModelConfigs() {
		return paramsUncertaintyModelConfigs;
	}

	public Collection<BBRegularisationConstantConfig> getRegularisationConstantCollection() {
        return regularisationConstantCollection;
    }

    public Collection<BBCartesianToPolarConfig> getCartesianToPolarCollection() {
        return cartesianToPolarCollection;
    }

    public BBStochModelStateConfig getStateConfig() {
        return stateConfig;
    }

    public Collection<BBStochModelVectorConfig> getPredictorVectorCollection() {
        return predictorVectorCollection;
    }

    public RangeValidationConstraint[] getRangeValidationConstraints() {
    	return rangeValidationConstraints;
    }

    public boolean isCollectPredictorTimeSeries(){
        return collectPredictorTimeSeries;
    }
}
