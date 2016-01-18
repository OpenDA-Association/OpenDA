package org.openda.model_bmi;

import java.io.File;

/**
 * Created by bos_en on 12/10/2015.
 */

public class BmiModelForcingConfig {
	private String className;
	private File workDir;
	private String fileName;
	private String[] arguments;

	public BmiModelForcingConfig (String className, File workDir, String fileName, String[] arguments){
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
