/*
* Copyright (c) 2021 OpenDA Association 
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
import org.openda.utils.io.CastorUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;

/**
 * Reader for black box wrapper configuration.
 */
public class BBWrapperConfigReader {

    private final BBWrapperConfig bbWrapperConfig;

    public BBWrapperConfigReader(File wrapperConfigFile) {

        BlackBoxWrapperConfigXML bbWrapperConfigXML = (BlackBoxWrapperConfigXML) CastorUtils.parse(wrapperConfigFile, BlackBoxWrapperConfigXML.class);

        // parse alias definitions
        AliasDefinitions aliasDefinitions = parseAliasDefinitions(bbWrapperConfigXML.getAliasDefinitions(), wrapperConfigFile);
        String keyPrefix=bbWrapperConfigXML.getAliasDefinitions().getDefaultKeyPrefix();
        String keySuffix=bbWrapperConfigXML.getAliasDefinitions().getDefaultKeySuffix();
        aliasDefinitions.add("instanceNumber", keyPrefix, keySuffix, "-1", null);
        aliasDefinitions.add("currentTime", keyPrefix, keySuffix, "0.0", null);
		aliasDefinitions.add("targetTime", keyPrefix, keySuffix, "0.0", null);
		aliasDefinitions.add("startTime", keyPrefix, keySuffix, "0.0", null);
		aliasDefinitions.add("endTime", keyPrefix, keySuffix, "0.0", null);

        // parse initializeAndRun actions
        BBWrapperConfig.CloneType cloneType = BBWrapperConfig.CloneType.None;
        String templateName = null;
        String instanceName = null;
        Collection<BBAction> initializeActions;

        BlackBoxWrapperRunXMLChoice initialActionsChoice = bbWrapperConfigXML.getRun().getBlackBoxWrapperRunXMLChoice();
        if (initialActionsChoice.getInitializeActionsUsingFileClone() != null) {
            cloneType = BBWrapperConfig.CloneType.File;
            templateName = initialActionsChoice.getInitializeActionsUsingFileClone().getTemplateFile();
            instanceName = initialActionsChoice.getInitializeActionsUsingFileClone().getInstanceFile();
            initializeActions = parseActions(wrapperConfigFile, aliasDefinitions,
                    initialActionsChoice.getInitializeActionsUsingFileClone().getAction());
        } else if (initialActionsChoice.getInitializeActionsUsingDirClone() != null) {
            cloneType = BBWrapperConfig.CloneType.Directory;
            templateName = initialActionsChoice.getInitializeActionsUsingDirClone().getTemplateDir();
            instanceName = initialActionsChoice.getInitializeActionsUsingDirClone().getInstanceDir();
            initializeActions = parseActions(wrapperConfigFile, aliasDefinitions,
                    initialActionsChoice.getInitializeActionsUsingDirClone().getAction());
        } else {
            initializeActions = parseActions(wrapperConfigFile, aliasDefinitions,
                    initialActionsChoice.getInitializeActions().getAction());
        }

        // parse computate actions and cleanup actions
        Collection<BBAction> computeActions = parseActions(wrapperConfigFile, aliasDefinitions, bbWrapperConfigXML.getRun().getComputeActions().getAction());
        Collection<BBAction> addionalComputeActions = new ArrayList<>();
        if (bbWrapperConfigXML.getRun().getAdditionalComputeActions() != null) {
            addionalComputeActions = parseActions(wrapperConfigFile, aliasDefinitions, bbWrapperConfigXML.getRun().getAdditionalComputeActions().getAction());
        }
        Collection<BBAction> finalizeActions = parseActions(wrapperConfigFile, aliasDefinitions, bbWrapperConfigXML.getRun().getFinalizeActions().getAction());

        // parse IO objects
        HashMap<String, DataObjectConfig> ioObjects = parseIoObjects(aliasDefinitions, bbWrapperConfigXML.getInputOutput().getInputOutputXMLItem());

        // create the config for all
        bbWrapperConfig = new BBWrapperConfig(aliasDefinitions, cloneType, templateName, instanceName,
                initializeActions, computeActions, addionalComputeActions, finalizeActions, ioObjects);

    }

    public BBWrapperConfig getBBWrapperConfig() {
        return bbWrapperConfig;
    }

    private static HashMap<String, DataObjectConfig> parseIoObjects(AliasDefinitions aliasDefinitions, InputOutputXMLItem[] ioOrDataObjectXMLs) {
        HashMap<String, DataObjectConfig> ioObjects = new HashMap<>();
        for (InputOutputXMLItem inputOutputXMLItem : ioOrDataObjectXMLs) {
			DataObjectXML dataObjectXML = inputOutputXMLItem.getIoObject();
			if (dataObjectXML == null) {
			   dataObjectXML = inputOutputXMLItem.getDataObject();
			}
		   ioObjects.put(dataObjectXML.getId(), new DataObjectConfig(dataObjectXML.getId(), dataObjectXML.getClassName(),
			   dataObjectXML.getFile(), aliasDefinitions, dataObjectXML.getArg()));
		}
        return ioObjects;
    }

    public static BBConfigurable parseBbConfigurable(File wrapperConfigFile, AliasDefinitions aliasDefinitions, BlackBoxConfigurableXML bbConfigurableXML) {
        if (bbConfigurableXML != null) {
            String[] arguments = checkConfigurableArguments(wrapperConfigFile,
                    bbConfigurableXML.getConfigString(), bbConfigurableXML.getConfigFile(), bbConfigurableXML.getArg());
            return new BBConfigurable(bbConfigurableXML.getClassName(), arguments, aliasDefinitions);
        }
        return null;
    }

    public static String[] checkConfigurableArguments(File wrapperConfigFile, String configString, String configFileName, String[] arguments) {
        String[] configurableArguments = new String[0];
        if (configString != null) {
            configurableArguments = new String[]{configString};
        } else if (configFileName != null) {
            // config string represents a file, check if it exists
            File configFile = new File(wrapperConfigFile.getParentFile(), configFileName);
            if (!configFile.exists()) {
                throw new RuntimeException("Accessor config. file " + configFile.getAbsolutePath() +
                        " does not exist");
            }
            configurableArguments = new String[]{configFileName};
        } else if (arguments != null) {
            configurableArguments = arguments;
        }
        return configurableArguments;
    }

    public static AliasDefinitions parseAliasDefinitions(AliasDefinitionsXML aliasDefinitionsXML, File wrapperConfigFile) {
        AliasDefinitions aliasDefinitions;
        AliasDefinitionXML[] aliasesXML = aliasDefinitionsXML.getAlias();
        aliasDefinitions = new AliasDefinitions();
        for (AliasDefinitionXML anAliasDefinitionsXML : aliasesXML) {
            String keyPrefix = anAliasDefinitionsXML.getKeyPrefix() != null ?
                    anAliasDefinitionsXML.getKeyPrefix() :
                    aliasDefinitionsXML.getDefaultKeyPrefix();
            String keySuffix = anAliasDefinitionsXML.getKeySuffix() != null ?
                    anAliasDefinitionsXML.getKeySuffix() :
                    aliasDefinitionsXML.getDefaultKeySuffix();
			if (anAliasDefinitionsXML.getValue() != null && anAliasDefinitionsXML.getListValueCount() > 0) {
				throw new RuntimeException("Both alias's value and listValue specified, choose one (alias:" +
						anAliasDefinitionsXML.getKey() + ", file: " + wrapperConfigFile.getAbsolutePath() + ")");
			}
			aliasDefinitions.add(anAliasDefinitionsXML.getKey(),
                    keyPrefix, keySuffix,
                    anAliasDefinitionsXML.getValue(), anAliasDefinitionsXML.getListValue());
        }
        return aliasDefinitions;
    }

    public static Collection<BBAction> parseActions(File configFile, AliasDefinitions aliasDefinitions, ActionXML[] computeActionsXML) {
        ArrayList<BBAction> actions = new ArrayList<>();
        for (ActionXML actionXML : computeActionsXML) {
            BBAction bbAction = parseBBAction(configFile, actionXML, aliasDefinitions);
            if (bbAction != null) {
            actions.add(bbAction);
        }
        }
        return actions;
    }

    public static BBAction parseBBAction(File configFile, ActionXML actionXML, AliasDefinitions aliasDefinitions) {

        if (actionXML == null) {
            return null;
        }

        ArrayList<BBCheckOutput> checkOutputs = new ArrayList<>();
        for (CheckOutputXML checkOutputXML : actionXML.getCheckOutput()) {
            checkOutputs.add(new BBCheckOutput(checkOutputXML.getFile(),
                    checkOutputXML.getExpect(), aliasDefinitions));
        }

		CheckReturnStatusXML checkReturnXML = actionXML.getCheckReturnStatus();
        BBCheckReturnStatus checkReturnStatus = null;
        if (checkReturnXML != null) {
            checkReturnStatus = new BBCheckReturnStatus(checkReturnXML.getExpect(),aliasDefinitions);
        }

        String exeName = actionXML.getExe();
        if (exeName == null) {
            if (BBUtils.RUNNING_ON_LINUX) {
                exeName = actionXML.getLinuxExe();
            } else if (BBUtils.RUNNING_ON_WINDOWS) {
                exeName = actionXML.getWindowsExe();
            } else if (BBUtils.RUNNING_ON_MAC) {
                exeName = actionXML.getMacExe();
				if (exeName==null) {
					exeName = actionXML.getLinuxExe();
				}
            }
        }
        String className = actionXML.getClassName();
        if ((exeName == null && className == null) || (exeName != null && className != null)) {
            throw new RuntimeException("Either an executable name (exe= or linuxExe/macExe/windowsExe=) " +
                    "or a class name (className=) must be specified in the <action/>, file: " + configFile.getAbsolutePath());
        }
        String actualWorkingDirectory = actionXML.getActualWorkingDirectory();
        return new BBAction(
                configFile.getParentFile(),
                exeName,
                className,
                actionXML.getArg(),
                actualWorkingDirectory,
                checkOutputs,
				checkReturnStatus,
                actionXML.getIgnoreStatus(),
                aliasDefinitions);
    }
}
