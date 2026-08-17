package org.openda.model_zero_mq;

import java.util.ArrayList;
import java.util.List;

public class ModelConfigurations {
	private final String modelConfigFile;
	private final List<ZeroMqModelForcingConfig> forcingConfiguration;
	private final List<ZeroMqModelForcingConfig> staticLimitConfiguration;
	private final ArrayList<String> transformVariableIds;

	public ModelConfigurations(String modelConfigFile, List<ZeroMqModelForcingConfig> forcingConfiguration, List<ZeroMqModelForcingConfig> staticLimitConfiguration, ArrayList<String> transformVariableIds) {
		this.modelConfigFile = modelConfigFile;
		this.forcingConfiguration = forcingConfiguration;
		this.staticLimitConfiguration = staticLimitConfiguration;
		this.transformVariableIds = transformVariableIds;
	}

	public String getModelConfigFile() {
		return modelConfigFile;
	}

	public List<ZeroMqModelForcingConfig> getForcingConfiguration() {
		return forcingConfiguration;
	}

	public List<ZeroMqModelForcingConfig> getStaticLimitConfiguration() {
		return staticLimitConfiguration;
	}

	public ArrayList<String> getTransformVariableIds() {
		return transformVariableIds;
	}
}
