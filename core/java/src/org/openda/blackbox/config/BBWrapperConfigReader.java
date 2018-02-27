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

    private BBWrapperConfig bbWrapperConfig;

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

        BlackBoxWrapperRunXMLChoice inititialActionsChoice = bbWrapperConfigXML.getRun().getBlackBoxWrapperRunXMLChoice();
        if (inititialActionsChoice.getInitializeActionsUsingFileClone() != null) {
            cloneType = BBWrapperConfig.CloneType.File;
            templateName = inititialActionsChoice.getInitializeActionsUsingFileClone().getTemplateFile();
            instanceName = inititialActionsChoice.getInitializeActionsUsingFileClone().getInstanceFile();
            initializeActions = parseActions(wrapperConfigFile, aliasDefinitions,
                    inititialActionsChoice.getInitializeActionsUsingFileClone().getAction());
        } else if (inititialActionsChoice.getInitializeActionsUsingDirClone() != null) {
            cloneType = BBWrapperConfig.CloneType.Directory;
            templateName = inititialActionsChoice.getInitializeActionsUsingDirClone().getTemplateDir();
            instanceName = inititialActionsChoice.getInitializeActionsUsingDirClone().getInstanceDir();
            initializeActions = parseActions(wrapperConfigFile, aliasDefinitions,
                    inititialActionsChoice.getInitializeActionsUsingDirClone().getAction());
        } else {
            initializeActions = parseActions(wrapperConfigFile, aliasDefinitions,
                    inititialActionsChoice.getInitializeActions().getAction());
        }

        // parse computate actions and cleanup actions
        Collection<BBAction> computeActions = parseActions(wrapperConfigFile, aliasDefinitions, bbWrapperConfigXML.getRun().getComputeActions().getAction());
        Collection<BBAction> addionalComputeActions = new ArrayList<BBAction>();
        if (bbWrapperConfigXML.getRun().getAdditionalComputeActions() != null) {
            addionalComputeActions = parseActions(wrapperConfigFile, aliasDefinitions, bbWrapperConfigXML.getRun().getAdditionalComputeActions().getAction());
        }
        Collection<BBAction> finalizeActions = parseActions(wrapperConfigFile, aliasDefinitions, bbWrapperConfigXML.getRun().getFinalizeActions().getAction());

        // parse IO objects
        HashMap<String, IoObjectConfig> ioObjects = parseIoObjects(aliasDefinitions, bbWrapperConfigXML.getInputOutput().getIoObject());

        // create the config for all
        bbWrapperConfig = new BBWrapperConfig(aliasDefinitions, cloneType, templateName, instanceName,
                initializeActions, computeActions, addionalComputeActions, finalizeActions, ioObjects);

    }

    public BBWrapperConfig getBBWrapperConfig() {
        return bbWrapperConfig;
    }

    private static HashMap<String, IoObjectConfig> parseIoObjects(AliasDefinitions aliasDefinitions, IoObjectXML[] ioObjectXMLs) {
        HashMap<String, IoObjectConfig> ioObjects = new HashMap<String, IoObjectConfig>();
        for (IoObjectXML ioObjectXML : ioObjectXMLs) {
            ioObjects.put(ioObjectXML.getId(), new IoObjectConfig(ioObjectXML.getId(), ioObjectXML.getClassName(),
                    ioObjectXML.getFile(), aliasDefinitions, ioObjectXML.getArg()));
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
        ArrayList<BBAction> actions = new ArrayList<BBAction>();
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

        ArrayList<BBCheckOutput> checkOutputs = new ArrayList<BBCheckOutput>();
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
		String os = System.getProperty("os.name");
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
