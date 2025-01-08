package org.openda.model_zero_mq;

import java.io.File;

public class ModelDirectories {
	private final File modelRunDir;
	private final String inputStateDirectory;
	private final String outputStateDirectory;

	public ModelDirectories(File modelRunDir, String inputStateDirectory, String outputStateDirectory) {
		this.modelRunDir = modelRunDir;
		this.inputStateDirectory = inputStateDirectory;
		this.outputStateDirectory = outputStateDirectory;
	}

	public File getModelRunDir() {
		return modelRunDir;
	}

	public String getInputStateDirectory() {
		return inputStateDirectory;
	}

	public String getOutputStateDirectory() {
		return outputStateDirectory;
	}
}
