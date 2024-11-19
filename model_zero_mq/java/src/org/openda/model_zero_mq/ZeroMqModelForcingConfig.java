package org.openda.model_zero_mq;

import java.io.File;

public class ZeroMqModelForcingConfig {
	private String className;
	private File workDir;
	private String fileName;
	private String[] arguments;

	public ZeroMqModelForcingConfig (String className, File workDir, String fileName, String[] arguments){
		this.className = className;
		this.workDir = workDir;
		this.fileName = fileName;
		this.arguments = arguments;
	}

	public String getClassName() {
		return this.className;
	}

	public String getDataObjectFileName() {
		return this.fileName;
	}

	public File getDataObjectWorkDir() {
		return this.workDir;
	}

	public String[] getArguments() {
		return this.arguments;
	}
}
