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

import java.util.Collection;
import java.util.HashMap;

/**
 * TODO: description
 */
public class BBWrapperConfig {
    public enum CloneType {
        None, Directory, File
    }

    private HashMap<String, IoObjectConfig> ioObjects = new HashMap<String, IoObjectConfig>();
    private AliasDefinitions aliasDefinitions;

    private CloneType cloneType;

    private String templateName;
    private String instanceName;
    Collection<String> aliasesUsedInTemplateName;
    Collection<String> aliasesUsedInInstanceName;

    private Collection<BBAction> initializeActions;
    private Collection<BBAction> computeActions;
    private Collection<BBAction> additionalComputeActions;
    private Collection<BBAction> finalizeActions;

    public BBWrapperConfig(AliasDefinitions aliasDefinitions,
                           CloneType cloneType,
                           String templateName,
                           String instanceName,
                           Collection<BBAction> initializeActions,
                           Collection<BBAction> computeActions,
                           Collection<BBAction> additionalComputeActions,
                           Collection<BBAction> finalizeActions,
                           HashMap<String, IoObjectConfig> ioObjects) {

        this.aliasDefinitions = aliasDefinitions;
        this.cloneType = cloneType;
        this.templateName = templateName;
        this.instanceName = instanceName;
        this.initializeActions = initializeActions;
        this.computeActions = computeActions;
        this.additionalComputeActions = additionalComputeActions;
        this.finalizeActions = finalizeActions;
        this.ioObjects = ioObjects;
        aliasesUsedInTemplateName = aliasDefinitions.getUsedAliasIds(this.templateName);
        aliasesUsedInInstanceName = aliasDefinitions.getUsedAliasIds(this.instanceName);
    }

    public AliasDefinitions getAliasDefinitions() {
        return aliasDefinitions;
    }

    public void validate() {
        aliasDefinitions.validate();
        for (BBAction action : initializeActions) {
            action.validate();
        }
        for (BBAction action : computeActions) {
            action.validate();
        }
        for (BBAction action : finalizeActions) {
            action.validate();
        }
    }

    public CloneType getCloneType() {
        return cloneType;
    }

    public String getTemplateName() {
        return aliasDefinitions.apply(templateName, aliasesUsedInTemplateName);
    }

	public String getTemplateName(AliasDefinitions externalAliasDefinitions) {
     return externalAliasDefinitions.apply(templateName, aliasesUsedInTemplateName);
 }


    public String getInstanceName() {
        return aliasDefinitions.apply(instanceName, aliasesUsedInInstanceName);
   }

    public String getInstanceName(AliasDefinitions externalAliasDefinitions) {
      return externalAliasDefinitions.apply(instanceName, aliasesUsedInInstanceName);
    }

    public Collection<BBAction> getInitializeActions() {
        return initializeActions;
    }

    public Collection<BBAction> getComputeActions() {
        return computeActions;
    }

    public Collection<BBAction> getAdditionalComputeActions() {
        return additionalComputeActions;
    }

    public Collection<BBAction> getFinalizeActions() {
        return finalizeActions;
    }

    public IoObjectConfig getIoObject(String id) {
        return ioObjects.get(id);
    }

}
