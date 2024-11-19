package org.openda.model_zero_mq;

import java.util.List;

public class ModelConfigurations {
	private final String modelConfigFile;
	private final List<ZeroMqModelForcingConfig> forcingConfiguration;
	private final List<ZeroMqModelForcingConfig> staticLimitConfiguration;

	public ModelConfigurations(String modelConfigFile, List<ZeroMqModelForcingConfig> forcingConfiguration, List<ZeroMqModelForcingConfig> staticLimitConfiguration) {
		this.modelConfigFile = modelConfigFile;
		this.forcingConfiguration = forcingConfiguration;
		this.staticLimitConfiguration = staticLimitConfiguration;
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
}
