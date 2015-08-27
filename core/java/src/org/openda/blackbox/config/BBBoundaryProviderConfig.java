package org.openda.blackbox.config;

import java.io.File;
import java.util.ArrayList;

/**
 * Created by hummel on 07-Aug-15.
 */
public class BBBoundaryProviderConfig {
	private String className;
	private File workDir;
	private String fileName;
	private String[] arguments;
	private ArrayList<BBBoundaryMappingConfig> boundaryMappingConfigs;

	public BBBoundaryProviderConfig(String className, File workDir, String fileName, String[] arguments,
									ArrayList<BBBoundaryMappingConfig> boundaryMappingConfigs) {
		this.className = className;
		this.workDir = workDir;
		this.fileName = fileName;
		this.arguments = arguments;
		this.boundaryMappingConfigs = boundaryMappingConfigs;
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

	public ArrayList<BBBoundaryMappingConfig> getBoundaryMappingConfigs() {
		return this.boundaryMappingConfigs;
	}
}
