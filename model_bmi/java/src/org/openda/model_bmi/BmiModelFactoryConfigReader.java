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

import org.openda.model_bmi.io.castorgenerated.*;
import org.openda.utils.io.CastorUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * Configuration reader for BmiModelFactoryConfig for a BMI model.
 *
 * @author Arno Kockx
 */
public class BmiModelFactoryConfigReader {
	private final File pythonModelPythonPath;
	private final String pythonModelModuleName;
	private final String pythonModelClassName;
	private String pythonExecutablePath;
	private final File modelTemplateDirectory;
	private final List<BmiModelFactory.BmiModelStateExchangeItemsInfo> bmiModelStateExchangeItemsInfos;
	/**
	 * The path and name of the model configuration file (relative to the model template directory).
	 */
	private final String relativeModelConfigFilePath;
	private ArrayList<BmiModelForcingConfig> bmiModelForcingConfigs;
	private ArrayList<BmiModelForcingConfig> staticLimitDataConfigs;
	private final String[] hosts;
	private String inputStateDir;
	private String outputStateDir;
	private double modelMissingValue;

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

			this.pythonExecutablePath = pythonModel.getPythonExecutable();
			if (this.pythonExecutablePath == null || this.pythonExecutablePath.isEmpty()) {
				this.pythonExecutablePath = "python";
			} else {
				File pythonExe = new File(this.pythonExecutablePath);
				if (!pythonExe.isFile()) {
					throw new RuntimeException(getClass().getSimpleName() + ": Configured pythonExecutablePath is invalid " + configFile.getAbsolutePath());
				}
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

		bmiModelForcingConfigs = new ArrayList<BmiModelForcingConfig>();
		BmiModelForcingsConfigXML[] bmiModelForcingsConfigXMLs = castor.getBmiModelForcingsConfig();
		if (bmiModelForcingsConfigXMLs.length > 0){
			for (BmiModelForcingsConfigXML forcingsConfig : bmiModelForcingsConfigXMLs) {
				ForcingDataObjectXML dataObjectXML = forcingsConfig.getDataObject();

				String dataObjectClassName = dataObjectXML.getClassName();
				String fileName = dataObjectXML.getFile();
				String[] dataObjectArguments = dataObjectXML.getArg();

				BmiModelForcingConfig bmiModelForcingConfig = new BmiModelForcingConfig(dataObjectClassName, configFile.getParentFile(), fileName, dataObjectArguments);
				bmiModelForcingConfigs.add(bmiModelForcingConfig);
			}
		}

		staticLimitDataConfigs = new ArrayList<BmiModelForcingConfig>();
		int staticLimitDataObjectsCount = castor.getSpaceVaryingLimitsCount();
		for (int i = 0; i < staticLimitDataObjectsCount; i++) {
			BmiModelForcingsConfigXML staticLimitConfig = castor.getSpaceVaryingLimits(i);
			ForcingDataObjectXML dataObjectXML = staticLimitConfig.getDataObject();

			String dataObjectClassName = dataObjectXML.getClassName();
			String fileName = dataObjectXML.getFile();
			String[] dataObjectArguments = dataObjectXML.getArg();

			BmiModelForcingConfig bmiModelForcingConfig = new BmiModelForcingConfig(dataObjectClassName, configFile.getParentFile(), fileName, dataObjectArguments);
			staticLimitDataConfigs.add(bmiModelForcingConfig);
		}

		bmiModelStateExchangeItemsInfos = new ArrayList<>();
		BmiModelStateExchangeItemXML bmiModelStateExchangeItems = castor.getBmiModelStateExchangeItems();
		List<String> stateVectorIds = new ArrayList<>();
		List<String> lowerLimitExchangeItemIds = new ArrayList<>();
		List<String> upperLimitExchangeItemIds = new ArrayList<>();
		List<Double> lowerLimits = new ArrayList<>();
		List<Double> upperLimits = new ArrayList<>();
		String stateId = "state";
		for (BmiModelStateExchangeItemXMLItem item : bmiModelStateExchangeItems.getBmiModelStateExchangeItemXMLItem()) {
			LimitedExchangeItem limitedItem = item.getLimitedExchangeItem();
			stateVectorIds.add(limitedItem.getExchangeItemId());
			LimitedExchangeItemChoice lowerLimitChoice = limitedItem.getLimitedExchangeItemChoice();
			if (lowerLimitChoice == null || !lowerLimitChoice.hasLowerLimit()) {
				lowerLimits.add(Double.NaN);
			} else if (lowerLimitChoice.hasLowerLimit()) {
				lowerLimits.add(lowerLimitChoice.getLowerLimit());
			}
			if (lowerLimitChoice != null && lowerLimitChoice.getSpaceVaryingLowerLimitExchangeItemId() != null) {
				lowerLimitExchangeItemIds.add(lowerLimitChoice.getSpaceVaryingLowerLimitExchangeItemId());
			} else {
				lowerLimitExchangeItemIds.add(null);
			}
			LimitedExchangeItemChoice2 upperLimitChoice = limitedItem.getLimitedExchangeItemChoice2();
			if (upperLimitChoice == null || !upperLimitChoice.hasUpperLimit()) {
				upperLimits.add(Double.NaN);
			} else if (upperLimitChoice.hasUpperLimit()) {
				upperLimits.add(upperLimitChoice.getUpperLimit());
			}
			if (upperLimitChoice != null && upperLimitChoice.getSpaceVaryingUpperLimitExchangeItemId() != null) {
				upperLimitExchangeItemIds.add(upperLimitChoice.getSpaceVaryingUpperLimitExchangeItemId());
			} else {
				upperLimitExchangeItemIds.add(null);
			}
		}
		bmiModelStateExchangeItemsInfos.add(new BmiModelFactory.BmiModelStateExchangeItemsInfo(stateId, stateVectorIds.toArray(new String[0]), lowerLimits.toArray(new Double[0]), upperLimits.toArray(new Double[0]), lowerLimitExchangeItemIds.toArray(new String[0]), upperLimitExchangeItemIds.toArray(new String[0])));

		this.inputStateDir = castor.getInputStateDirectory();
		this.outputStateDir = castor.getOutputStateDirectory();

		//default is NaN.
		this.modelMissingValue = Double.NaN;
		if (castor.hasMissingValue()) {
			this.modelMissingValue = castor.getMissingValue();
		}

		String hosts = castor.getHosts();
		if (hosts == null || hosts.trim().isEmpty()) {//if hosts not configured.
			this.hosts = null;
		} else {
			this.hosts = hosts.split(",");
		}
	}

	public String getPythonExecutablePath() {return this.pythonExecutablePath;}

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

	public ArrayList<BmiModelForcingConfig> getBmiModelForcingConfigs() { return this.bmiModelForcingConfigs; }

	public ArrayList<BmiModelForcingConfig> getStaticLimitDataConfigs() {
		return this.staticLimitDataConfigs;
	}

	public List<BmiModelFactory.BmiModelStateExchangeItemsInfo> getModelStateExchangeItemInfos() {
		return bmiModelStateExchangeItemsInfos;
	}

	public String[] getHosts() {
		return hosts;
	}

	public String getInputStateDir() {
		return inputStateDir;
	}

	public String getOutputStateDir() {
		return outputStateDir;
	}

	public double getModelMissingValue() {
		return modelMissingValue;
	}
}
