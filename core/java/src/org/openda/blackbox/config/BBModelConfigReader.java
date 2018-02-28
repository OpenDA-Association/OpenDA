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

package org.openda.blackbox.config;

import org.openda.core.io.castorgenerated.*;
import org.openda.core.io.castorgenerated.types.BlackBoxModelRoleTypesXML;
import org.openda.interfaces.IPrevExchangeItem;
import org.openda.interfaces.ITime;
import org.openda.utils.DimensionIndex;
import org.openda.utils.Time;
import org.openda.utils.io.CastorUtils;

import java.io.File;
import java.util.ArrayList;

/**
 * Reader for black box model configuration.
 */
public class BBModelConfigReader {

    private BBModelConfig bbModelConfig;

    public BBModelConfigReader(File modelConfigFile) {
        BlackBoxModelConfigXML bbModelConfigXML = (BlackBoxModelConfigXML) CastorUtils.parse(modelConfigFile, BlackBoxModelConfigXML.class);
        bbModelConfig = parseBBModelConfig(modelConfigFile, bbModelConfigXML);
    }

    public BBModelConfig getBBModelConfig() {
        return bbModelConfig;
    }

    public static BBModelConfig parseBBModelConfig(File modelConfigFile, BlackBoxModelConfigXML bbModelConfigXML) {
        // parse generic wrapper part

        File wrapperConfigFile = new File(modelConfigFile.getParentFile(),
                bbModelConfigXML.getWrapperConfig().getFile());
        if (!wrapperConfigFile.exists()) {
            throw new RuntimeException("Model configuration " + modelConfigFile.getAbsolutePath() +
                    " refers to non existing wrapper configuration file " + wrapperConfigFile.getAbsolutePath());
        }
        BBWrapperConfigReader wrapperConfigReader = new BBWrapperConfigReader(wrapperConfigFile);
        BBWrapperConfig bbWrapperConfig = wrapperConfigReader.getBBWrapperConfig();

        // parse and set alias values
        bbWrapperConfig.getAliasDefinitions().add("instanceNumber", "%", "%", null, null);
        AliasValueXML[] aliasValueXMLs = bbModelConfigXML.getAliasValues().getAlias();
        for (AliasValueXML aliasValueXML : aliasValueXMLs) {
			if (aliasValueXML.getValue() != null && aliasValueXML.getListValueCount() > 0) {
					throw new RuntimeException("Both alias's value and listValue specified, choose one (alias:" +
							aliasValueXML.getKey() + ", file: " + modelConfigFile.getAbsolutePath() + ")");
			}
			if (aliasValueXML.getValue() != null) {
				bbWrapperConfig.getAliasDefinitions().setAliasValue(aliasValueXML.getKey(), aliasValueXML.getValue());
			} else {
				bbWrapperConfig.getAliasDefinitions().setAliasListValues(aliasValueXML.getKey(), aliasValueXML.getListValue());
			}
        }

		String instanceNumberFormat = bbModelConfigXML.getInstanceNumberFormat();

        // parse io selections
        ArrayList<BBModelVectorConfig> vectorConfigs = new ArrayList<BBModelVectorConfig>();

        for (ExchangeItemXMLItem exchangeItemXMLItem : bbModelConfigXML.getExchangeItems().getExchangeItemXMLItem()) {

            String id;
            String objectId;
            String elementId;
			IPrevExchangeItem.Role role;
            DimensionIndex[] selectionIndices = null;
            BBConfigurable selectorConfig = null;
            String idSuffix = null;
            if (exchangeItemXMLItem.getSubVector() != null) {
                // subvector (with indices or selector)
                BlackBoxIoSubVectorXML subVector = exchangeItemXMLItem.getSubVector();
                id = subVector.getId();
                objectId = subVector.getIoObjectId();
                elementId = subVector.getElementId();
    			role = determineRoleType(subVector.getRole());
                selectionIndices = parseSelectionIndices(subVector.getSelection());
                selectorConfig = BBWrapperConfigReader.parseBbConfigurable(modelConfigFile, bbWrapperConfig.getAliasDefinitions(), subVector.getSelector());
            } else {
                // simple vector
                BlackBoxIoVectorXML vector = exchangeItemXMLItem.getVector();
                id = vector.getId();
                objectId = vector.getIoObjectId();
                elementId = vector.getElementId();
    			role = determineRoleType(vector.getRole());
                if (elementId == null) {
                    elementId = id;
                }
                idSuffix = vector.getIdSuffix();
            }

            IoObjectConfig ioObjectConfig = bbWrapperConfig.getIoObject(objectId);
            if (ioObjectConfig == null) {
                throw new RuntimeException("ObjectId \"" + objectId + "\" is not specified in the wrapper configuration " +
                        "(model file: " + modelConfigFile.getAbsolutePath() +
                        ", wrapper config. file: " + wrapperConfigFile.getAbsolutePath() + ")");
            }
            vectorConfigs.add(new BBModelVectorConfig(id, ioObjectConfig, elementId, selectionIndices, selectorConfig, role, idSuffix));
        }

        ITime startTime = null;
        ITime endTime = null;
        double timeStepMJD = Double.NaN;
        String[] startTimeExchangeItemIds = null;
        String[] endTimeExchangeItemIds = null;
        String[] timeStepExchangeItemIds = null;
        BlackBoxModelConfigXMLChoice timeInfoChoice = bbModelConfigXML.getBlackBoxModelConfigXMLChoice();
        if (timeInfoChoice != null) {
            TimeInfoXML timeInfoXML = timeInfoChoice.getTimeInfo();
            TimeInfoExchangeItemsXML[] timeInfoExchangeItemsXML = timeInfoChoice.getTimeInfoExchangeItems();
            if (timeInfoXML != null) {
                startTime = new Time(timeInfoXML.getStart());
                if (timeInfoXML.hasTimeStepInSeconds()) {
                    timeStepMJD = timeInfoXML.getTimeStepInSeconds() / 86400d;
                }
                endTime = new Time(timeInfoXML.getEnd());
            } else if (timeInfoExchangeItemsXML != null && timeInfoExchangeItemsXML.length > 0) {
                startTimeExchangeItemIds = new String[timeInfoExchangeItemsXML.length];
                endTimeExchangeItemIds = new String[timeInfoExchangeItemsXML.length];
                timeStepExchangeItemIds = new String[timeInfoExchangeItemsXML.length];
                for (int n = 0; n < timeInfoExchangeItemsXML.length; n++) {
                    startTimeExchangeItemIds[n] = timeInfoExchangeItemsXML[n].getStart();
                    endTimeExchangeItemIds[n] = timeInfoExchangeItemsXML[n].getEnd();
                    timeStepExchangeItemIds[n] = timeInfoExchangeItemsXML[n].getTimeStep();
                }
            }
        }
		boolean skipModelActionsIfInstanceDirExists = bbModelConfigXML.getSkipModelActionsIfInstanceDirExists();
		boolean doCleanUp = bbModelConfigXML.getDoCleanUp();
		if (doCleanUp && bbWrapperConfig.getCloneType() == BBWrapperConfig.CloneType.None) {
			throw new RuntimeException("\"doCleanUp\" can not be set \"true\" when the wrapper configuration " +
                    "does not specify file or model cloning (model file: " + modelConfigFile.getAbsolutePath() +
                    ", wrapper config. file: " + wrapperConfigFile.getAbsolutePath() + ")");
		}

		String[] restartFileNames = null;
		String savedStatesDirPrefix = null;
		if (bbModelConfigXML.getRestartInfo() != null) {
			savedStatesDirPrefix = bbModelConfigXML.getRestartInfo().getDirPrefix();
			restartFileNames = bbModelConfigXML.getRestartInfo().getModelStateFile();
		}
		return new BBModelConfig(modelConfigFile.getParentFile(), bbWrapperConfig,
				instanceNumberFormat,
				startTime, endTime,
                timeStepMJD, startTimeExchangeItemIds, endTimeExchangeItemIds,
                timeStepExchangeItemIds, vectorConfigs,
				skipModelActionsIfInstanceDirExists, doCleanUp, restartFileNames, savedStatesDirPrefix);
    }

    public static DimensionIndex[] parseSelectionIndices(IndicesXML indicesXML) {
        if (indicesXML != null) {
            return SelectionIndices(indicesXML.getIndex1(),
                    indicesXML.getIndex2(), indicesXML.getIndex3(), indicesXML.getBase());
        }
        return null;
    }

    private static DimensionIndex[] SelectionIndices(String index1, String index2, String index3, int dimensionBase) {
        DimensionIndex[] selectionIndices;
        int numIndices = 1;
        if (index2 != null) numIndices++;
        if (index3 != null) numIndices++;

        selectionIndices = new DimensionIndex[numIndices];

        selectionIndices[0] = new DimensionIndex(index1, dimensionBase);
        if (index2 != null) selectionIndices[1] = new DimensionIndex(index2, dimensionBase);
        if (index3 != null) selectionIndices[2] = new DimensionIndex(index3, dimensionBase);
        return selectionIndices;
    }

	private static IPrevExchangeItem.Role determineRoleType(BlackBoxModelRoleTypesXML roleXML) {
		IPrevExchangeItem.Role role = IPrevExchangeItem.Role.InOut;
		if (roleXML == null) return role;
		if (roleXML.getType() ==
				BlackBoxModelRoleTypesXML.INPUT_TYPE) {
			role = IPrevExchangeItem.Role.Input;
		} else if (roleXML.getType() ==
				BlackBoxModelRoleTypesXML.OUTPUT_TYPE) {
			role = IPrevExchangeItem.Role.Output;
		}
		return role;
	}

}
