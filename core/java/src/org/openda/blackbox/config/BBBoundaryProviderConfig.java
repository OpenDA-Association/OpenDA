/* OpenDA v2.4 
* Copyright (c) 2017 OpenDA Association 
* All rights reserved.
* 
* This file is part of OpenDA. 
* 
* OpenDA is free software: you can redistribute it and/or modify 
* it under the terms of the GNU Lesser General Public License as 
* published by the Free Software Foundation, either version 3 of 
* the License, or (at your option) any later version. 
* 
* OpenDA is distributed in the hope that it will be useful, 
* but WITHOUT ANY WARRANTY; without even the implied warranty of 
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
* GNU Lesser General Public License for more details. 
* 
* You should have received a copy of the GNU Lesser General Public License
* along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.
*/
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
