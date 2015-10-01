/* MOD_V2.0
 * Copyright (c) 2012 OpenDA Association
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

package org.openda.model_bmi;

import org.openda.model_bmi.io.castorgenerated.BmiModelFactoryConfigXML;
import org.openda.model_bmi.io.castorgenerated.BmiPythonModelXML;
import org.openda.utils.io.CastorUtils;

import java.io.File;

/**
 * Configuration reader for BmiModelFactoryConfig for a BMI model.
 *
 * @author Arno Kockx
 */
public class BmiModelFactoryConfigReader {
	private final File pythonModelPythonPath;
	private final String pythonModelModuleName;
	private final String pythonModelClassName;
	private final File modelTemplateDirectory;
	/**
	 * The path and name of the model configuration file (relative to the model template directory).
	 */
	private final String relativeModelConfigFilePath;
	private final String[] hosts;

	public BmiModelFactoryConfigReader(File configFile) {
		BmiModelFactoryConfigXML castor = (BmiModelFactoryConfigXML) CastorUtils.parse(configFile,
				BmiModelFactoryConfigXML.class);

		if (castor.getBmiModelFactoryConfigXMLChoice().getPythonModel() != null) {
			BmiPythonModelXML pythonModel = castor.getBmiModelFactoryConfigXMLChoice().getPythonModel();

			String pythonPath = pythonModel.getPythonPath();
			this.pythonModelPythonPath = new File(configFile.getParentFile(), pythonPath);
			if (!this.pythonModelPythonPath.exists()) {
				throw new RuntimeException(getClass().getSimpleName() + ": Cannot find pythonModel pythonPath "
						+ this.pythonModelPythonPath.getAbsolutePath() + " configured in "
						+ configFile.getAbsolutePath());
			}

			this.pythonModelModuleName = pythonModel.getModuleName();
			if (this.pythonModelModuleName == null || this.pythonModelModuleName.isEmpty()) {
				throw new RuntimeException(getClass().getSimpleName()
						+ ": Configured pythonModel moduleName must not be an empty string in config file "
						+ configFile.getAbsolutePath());
			}

			this.pythonModelClassName = pythonModel.getClassName();
			if (this.pythonModelClassName == null || this.pythonModelClassName.isEmpty()) {
				throw new RuntimeException(getClass().getSimpleName()
						+ ": Configured pythonModel className must not be an empty string in config file "
						+ configFile.getAbsolutePath());
			}
		} else {
			throw new IllegalArgumentException("Unknown model type configured in bmiModelFactoryConfigFile "
					+ configFile.getAbsolutePath());
		}

		String modelTemplateDirectoryPath = castor.getModelTemplateDirectory();
		this.modelTemplateDirectory = new File(configFile.getParentFile(), modelTemplateDirectoryPath);
		if (!this.modelTemplateDirectory.exists()) {
			throw new RuntimeException(getClass().getSimpleName() + ": Cannot find model template directory "
					+ this.modelTemplateDirectory.getAbsolutePath() + " configured in " + configFile.getAbsolutePath());
		}

		this.relativeModelConfigFilePath = castor.getModelConfigFile();
		File modelConfigFile = new File(this.modelTemplateDirectory, this.relativeModelConfigFilePath);
		if (!modelConfigFile.exists()) {
			throw new RuntimeException(getClass().getSimpleName() + ": Cannot find model config file "
					+ modelConfigFile.getAbsolutePath() + " configured in " + configFile.getAbsolutePath());
		}

		String hosts = castor.getHosts();
		if (hosts == null || hosts.trim().isEmpty()) {//if hosts not configured.
			this.hosts = null;
		} else {
			this.hosts = hosts.split(",");
		}
	}

	public File getPythonModelPythonPath() {
		return this.pythonModelPythonPath;
	}

	public String getPythonModelModuleName() {
		return this.pythonModelModuleName;
	}

	public String getPythonModelClassName() {
		return this.pythonModelClassName;
	}

	public File getModelTemplateDirectory() {
		return this.modelTemplateDirectory;
	}

	public String getRelativeModelConfigFilePath() {
		return this.relativeModelConfigFilePath;
	}

	public String[] getHosts() {
		return hosts;
	}
}
