package org.openda.model_zero_mq;

import org.openda.model_zero_mq.io.castorgenerated.*;
import org.openda.utils.io.CastorUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ZeroMqModelFactoryConfigReader {
	private final String executable;
	private final List<String> executableArguments;
	private final String host;
	private final Integer firstPortNumber;
	private final String modelConfigFile;
	private final File modelTemplateDirectory;
	private final String inputStateDirectory;
	private final String outputStateDirectory;
	private final double missingValue;
	private List<ZeroMqModelFactory.ZeroMqModelStateExchangeItemsInfo> zeroMqModelStateExchangeItemInfos;
	private ArrayList<ZeroMqModelForcingConfig> zeroMqModelForcingConfigs;
	private ArrayList<ZeroMqModelForcingConfig> staticLimitDataConfigs;

	public ZeroMqModelFactoryConfigReader(File configFile) {
		ZeroMqModelFactoryConfigXML castor = (ZeroMqModelFactoryConfigXML) CastorUtils.parse(configFile, ZeroMqModelFactoryConfigXML.class);

		executable = castor.getExecutable();
		executableArguments = Arrays.asList(castor.getExecutableArguments().getArgument());
		host = castor.getHost();
		firstPortNumber = castor.getFirstPortNumber();

		modelConfigFile = castor.getModelConfigFile();
		String modelTemplateDirectoryPath = castor.getModelTemplateDirectory();
		this.modelTemplateDirectory = new File(configFile.getParentFile(), modelTemplateDirectoryPath);
		if (!this.modelTemplateDirectory.exists()) {
			throw new RuntimeException(getClass().getSimpleName() + ": Cannot find model template directory "
				+ this.modelTemplateDirectory.getAbsolutePath() + " configured in " + configFile.getAbsolutePath());
		}
		inputStateDirectory = castor.getInputStateDirectory();
		outputStateDirectory = castor.getOutputStateDirectory();
		missingValue = castor.getMissingValue();

		initializeMqModelForcingConfigs(configFile, castor);
		initializeZeroMqModelForcingConfigs(configFile, castor);
		initializeZeroMqModelStateExchangeItemsInfos(castor);
	}

	private void initializeMqModelForcingConfigs(File configFile, ZeroMqModelFactoryConfigXML castor) {
		zeroMqModelForcingConfigs = new ArrayList<>();
		ZeroMqModelForcingsConfigXML[] zeroMqModelForcingsConfigXMLs = castor.getModelForcings();
		for (ZeroMqModelForcingsConfigXML forcingsConfig : zeroMqModelForcingsConfigXMLs) {
			addForcingConfig(forcingsConfig, configFile, zeroMqModelForcingConfigs);
		}
	}

	private void initializeZeroMqModelForcingConfigs(File configFile, ZeroMqModelFactoryConfigXML castor) {
		staticLimitDataConfigs = new ArrayList<>();
		int staticLimitDataObjectsCount = castor.getSpaceVaryingLimitsCount();
		for (int i = 0; i < staticLimitDataObjectsCount; i++) {
			ZeroMqModelForcingsConfigXML staticLimitConfig = castor.getSpaceVaryingLimits(i);
			addForcingConfig(staticLimitConfig, configFile, staticLimitDataConfigs);
		}
	}

	private void addForcingConfig(ZeroMqModelForcingsConfigXML staticLimitConfig, File configFile, ArrayList<ZeroMqModelForcingConfig> staticLimitDataConfigs) {
		ForcingDataObjectXML dataObjectXML = staticLimitConfig.getDataObject();

		String dataObjectClassName = dataObjectXML.getClassName();
		String fileName = dataObjectXML.getFile();
		String[] dataObjectArguments = dataObjectXML.getArg();

		ZeroMqModelForcingConfig zeroMqModelForcingConfig = new ZeroMqModelForcingConfig(dataObjectClassName, configFile.getParentFile(), fileName, dataObjectArguments);
		staticLimitDataConfigs.add(zeroMqModelForcingConfig);
	}

	private void initializeZeroMqModelStateExchangeItemsInfos(ZeroMqModelFactoryConfigXML castor) {
		zeroMqModelStateExchangeItemInfos = new ArrayList<>();
		ZeroMqModelStateExchangeItemXML zeroMqModelStateExchangeItems = castor.getZeroMqModelStateExchangeItems();
		List<String> stateVectorIds = new ArrayList<>();
		List<String> lowerLimitExchangeItemIds = new ArrayList<>();
		List<String> upperLimitExchangeItemIds = new ArrayList<>();
		List<Double> lowerLimits = new ArrayList<>();
		List<Double> upperLimits = new ArrayList<>();
		String stateId = "state";
		for (ZeroMqModelStateExchangeItemXMLItem item : zeroMqModelStateExchangeItems.getZeroMqModelStateExchangeItemXMLItem()) {
			LimitedExchangeItem limitedItem = item.getLimitedExchangeItem();
			stateVectorIds.add(limitedItem.getExchangeItemId());
			processLowerLimits(limitedItem, lowerLimits, lowerLimitExchangeItemIds);
			processUpperLimits(limitedItem, upperLimits, upperLimitExchangeItemIds);
		}
		zeroMqModelStateExchangeItemInfos.add(new ZeroMqModelFactory.ZeroMqModelStateExchangeItemsInfo(stateId, stateVectorIds.toArray(new String[0]), lowerLimits.toArray(new Double[0]), upperLimits.toArray(new Double[0]), lowerLimitExchangeItemIds.toArray(new String[0]), upperLimitExchangeItemIds.toArray(new String[0])));
	}

	private static void processLowerLimits(LimitedExchangeItem limitedItem, List<Double> lowerLimits, List<String> lowerLimitExchangeItemIds) {
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
	}

	private static void processUpperLimits(LimitedExchangeItem limitedItem, List<Double> upperLimits, List<String> upperLimitExchangeItemIds) {
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

	public String getExecutable() {
		return executable;
	}

	public List<String> getExecutableArguments() {
		return executableArguments;
	}

	public String getHost() {
		return host;
	}

	public Integer getFirstPortNumber() {
		return firstPortNumber;
	}

	public String getModelConfigFile() {
		return modelConfigFile;
	}

	public File getModelTemplateDirectory() {
		return modelTemplateDirectory;
	}

	public String getInputStateDirectory() {
		return inputStateDirectory;
	}

	public String getOutputStateDirectory() {
		return outputStateDirectory;
	}

	public double getMissingValue() {
		return missingValue;
	}

	public List<ZeroMqModelFactory.ZeroMqModelStateExchangeItemsInfo> getZeroMqModelStateExchangeItemInfos() {
		return zeroMqModelStateExchangeItemInfos;
	}

	public ArrayList<ZeroMqModelForcingConfig> getZeroMqModelForcingConfigs() {
		return zeroMqModelForcingConfigs;
	}

	public ArrayList<ZeroMqModelForcingConfig> getStaticLimitDataConfigs() {
		return staticLimitDataConfigs;
	}
}
