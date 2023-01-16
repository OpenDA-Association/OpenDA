package org.openda.model_zero_mq;

import org.openda.model_zero_mq.io.castorgenerated.ZeroMqModelFactoryConfigXML;
import org.openda.model_zero_mq.io.castorgenerated.ZeroMqModelStateExchangeItemXML;
import org.openda.utils.io.CastorUtils;

import java.io.File;
import java.util.Arrays;
import java.util.List;

public class ZeroMqModelFactoryConfigReader {
	private final String executable;
	private final List<String> executableArguments;
	private final String host;
	private final Integer port;
	private final String modelConfigFile;
	private final String modelTemplateDirectory;
	private final String inputStateDirectory;
	private final String outputStateDirectory;
	private final double missingValue;
	private final ZeroMqModelStateExchangeItemXML zeroMqModelStateExchangeItems;

	public ZeroMqModelFactoryConfigReader(File configFile) {
		ZeroMqModelFactoryConfigXML castor = (ZeroMqModelFactoryConfigXML) CastorUtils.parse(configFile, ZeroMqModelFactoryConfigXML.class);

		executable = castor.getExecutable();
		executableArguments = Arrays.asList(castor.getExecutableArguments().getArgument());
		host = castor.getHost();
		port = castor.getPort();

		modelConfigFile = castor.getModelConfigFile();
		modelTemplateDirectory = castor.getModelTemplateDirectory();
		inputStateDirectory = castor.getInputStateDirectory();
		outputStateDirectory = castor.getOutputStateDirectory();
		missingValue = castor.getMissingValue();
		zeroMqModelStateExchangeItems = castor.getZeroMqModelStateExchangeItems();
	}

	public String getExecutable() {
		return executable;
	}

	public List<String> getExecutableArguments() {
		return executableArguments;
	}

	public String getHost() {
		return host;
	}

	public Integer getPort() {
		return port;
	}

	public String getModelConfigFile() {
		return modelConfigFile;
	}

	public String getModelTemplateDirectory() {
		return modelTemplateDirectory;
	}

	public String getInputStateDirectory() {
		return inputStateDirectory;
	}

	public String getOutputStateDirectory() {
		return outputStateDirectory;
	}

	public double getMissingValue() {
		return missingValue;
	}

	public ZeroMqModelStateExchangeItemXML getZeroMqModelStateExchangeItems() {
		return zeroMqModelStateExchangeItems;
	}
}
