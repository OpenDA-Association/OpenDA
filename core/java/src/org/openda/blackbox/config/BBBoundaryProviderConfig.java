package org.openda.blackbox.config;

import java.io.File;
import java.util.ArrayList;

/**
 * Created by hummel on 07-Aug-15.
 */
public class BBBoundaryProviderConfig {
	private String className;
	private File dataObjectFile;
	private String[] arguments;
	private ArrayList<BBBoundaryMappingConfig> boundaryMappingConfigs;

	public BBBoundaryProviderConfig(String className, File dataObjectFile, String[] arguments,
									ArrayList<BBBoundaryMappingConfig> boundaryMappingConfigs) {
		this.className = className;
		this.dataObjectFile = dataObjectFile;
		this.arguments = arguments;
		this.boundaryMappingConfigs = boundaryMappingConfigs;
	}

	public String getClassName() {
		return className;
	}

	public File getDataObjectFile() {
		return dataObjectFile;
	}

	public String[] getArguments() {
		return arguments;
	}

	public ArrayList<BBBoundaryMappingConfig> getBoundaryMappingConfigs() {
		return boundaryMappingConfigs;
	}
}
