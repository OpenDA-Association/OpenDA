/* OpenDA v2.4.3 
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
package org.openda.model_efdc_dll;

import java.io.File;
import java.util.TimeZone;

import org.openda.exchange.timeseries.TimeUtils;
import org.openda.model_edfc_dll.io.castorgenerated.EfdcModelFactoryConfigXML;
import org.openda.utils.io.CastorUtils;

/**
 * Configuration reader for EfdcModelFactoryConfig for dll version of efdc model.
 *
 * @author Arno Kockx
 */
public class EfdcModelFactoryConfigReader {
	private final File efdcDllFile;
	private final File templateDirectory;
	private final File instanceDirectoryWithoutPostfix;
	private final String[] inputFilePaths;
	private final String relativeModelOutputFilePath;
	private final String relativeAnalysisOutputFilePath;
	private final TimeZone modelTimeZone;

	public EfdcModelFactoryConfigReader(File configFile) {
		EfdcModelFactoryConfigXML efdcModelFactoryConfigXML =
				(EfdcModelFactoryConfigXML) CastorUtils.parse(configFile, EfdcModelFactoryConfigXML.class);

		File configDir = configFile.getParentFile();
		this.efdcDllFile = new File(configDir, efdcModelFactoryConfigXML.getEfdcDllFile());
		if (!this.efdcDllFile.exists()) {
			throw new RuntimeException(getClass().getSimpleName() + ": Cannot find configured efdc dll file " + this.efdcDllFile.getAbsolutePath());
		}
		this.templateDirectory = new File(configDir, efdcModelFactoryConfigXML.getTemplateDirectory());
		if (!this.templateDirectory.exists()) {
			throw new RuntimeException(getClass().getSimpleName() + ": Cannot find configured template directory "
					+ this.templateDirectory.getAbsolutePath());
		}
		this.instanceDirectoryWithoutPostfix = new File(configDir, efdcModelFactoryConfigXML.getInstanceDirectory());

		this.inputFilePaths = efdcModelFactoryConfigXML.getInputFile();
		this.relativeModelOutputFilePath = efdcModelFactoryConfigXML.getModelOutputFile();
		this.relativeAnalysisOutputFilePath = efdcModelFactoryConfigXML.getAnalysisOutputFile();
		this.modelTimeZone = TimeUtils.createTimeZoneFromDouble(efdcModelFactoryConfigXML.getTimeZoneOffset());
	}

	public File getEfdcDllFile() {
		return this.efdcDllFile;
	}

	public File getTemplateDirectory() {
		return this.templateDirectory;
	}

	public File getInstanceDirectoryWithoutPostfix() {
		return this.instanceDirectoryWithoutPostfix;
	}

	public String[] getRelativeInputFilePaths() {
		return this.inputFilePaths;
	}

	public String getRelativeModelOutputFilePath() {
		return this.relativeModelOutputFilePath;
	}

	public String getRelativeAnalysisOutputFilePath() {
		return this.relativeAnalysisOutputFilePath;
	}

	public TimeZone getModelTimeZone() {
		return this.modelTimeZone;
	}

}
