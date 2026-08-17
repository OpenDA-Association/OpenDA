package org.openda.model_zero_mq;

public class ZeroMqModelForcingConfig {
	private final String className;
	private final String fileName;
	private final String[] arguments;

	public ZeroMqModelForcingConfig (String className, String fileName, String[] arguments){
		this.className = className;
		this.fileName = fileName;
		this.arguments = arguments;
	}

	public String getClassName() {
		return this.className;
	}

	public String getDataObjectFileName() {
		return this.fileName;
	}

	public String[] getArguments() {
		return this.arguments;
	}
}
