package org.openda.model_zero_mq;

import org.openda.model_zero_mq.io.castorgenerated.*;
import org.openda.utils.io.CastorUtils;

import java.io.File;

public class ZeroMqModelFactoryConfigReader {
	private final String executable;
	private final String executableArguments;
	private final String host;
	private final Integer port;

	public ZeroMqModelFactoryConfigReader(File configFile) {
		ZeroMqModelFactoryConfigXML castor = (ZeroMqModelFactoryConfigXML) CastorUtils.parse(configFile, ZeroMqModelFactoryConfigXML.class);

		executable = castor.getExecutable();
		executableArguments = castor.getExecutableArguments();
		host = castor.getHost();
		port = castor.getPort();
	}

	public String getExecutable() {
		return executable;
	}

	public String getExecutableArguments() {
		return executableArguments;
	}

	public String getHost() {
		return host;
	}

	public Integer getPort() {
		return port;
	}
}
