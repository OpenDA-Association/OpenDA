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
package org.openda.model_delft3d.dll;

import nl.deltares.openda.models.io.castorgenerated.*;
import org.openda.blackbox.config.AliasDefinitions;
import org.openda.utils.io.CastorUtils;

import java.io.File;
import java.util.ArrayList;

/**
 * Configuration reader for DLL Based Delft3d Flow wrapper
 */
public class D3dFlowModelConfigReader {

    D3dFlowModelConfig d3dFlowModelConfig;

    public D3dFlowModelConfigReader(File configFile) {
        File configDir = configFile.getParentFile();
        DD3dModelFactoryConfigXML modelFactoryConfigXML =
                (DD3dModelFactoryConfigXML)CastorUtils.parse(configFile, DD3dModelFactoryConfigXML.class);

        ArrayList<D3dFlowExchangeItemConfig> exchangeItemsList =
                new ArrayList<D3dFlowExchangeItemConfig>();

        if (modelFactoryConfigXML.getExchangeItems() != null) {
            for (DD3dExchangeItemXML exchangeItemXML : modelFactoryConfigXML.getExchangeItems().getExchangeItem()) {
                D3dFlowExchangeItemConfig exchangeItemConfig =
                        new D3dFlowExchangeItemConfig(exchangeItemXML.getId(),exchangeItemXML.getType());
                exchangeItemsList.add(exchangeItemConfig);
            }
        }

		AliasDefinitions aliasDefinitions = parseAliasDefinitions(
				modelFactoryConfigXML.getAliases(), configFile);

		String inputFileName = modelFactoryConfigXML.getModel().getDD3dModelConfigXMLChoice().getInputFile();
		if (inputFileName == null) {
			inputFileName = modelFactoryConfigXML.getModel().getDD3dModelConfigXMLChoice().getMdFile();
		}
		d3dFlowModelConfig = new D3dFlowModelConfig(
                configDir,
                modelFactoryConfigXML.getDll().getBinDir(),
                modelFactoryConfigXML.getDll().getType(),
                modelFactoryConfigXML.getDll().getFile(),
                modelFactoryConfigXML.getModel().getDirectory(),
				inputFileName,
				modelFactoryConfigXML.getModel().getArg(),
				modelFactoryConfigXML.getModel().getDirectoryForSavedStates(),
				modelFactoryConfigXML.getModel().getMaxNumInstancesInMemory(),
				exchangeItemsList,
				aliasDefinitions);
    }

	private AliasDefinitions parseAliasDefinitions(DD3dAliasDefinitionsXML aliasDefinitionsXML, File configFile) {
		AliasDefinitions aliasDefinitions = new AliasDefinitions();
		if (aliasDefinitionsXML != null) {
			for (DD3dAliasDefinitionXML anAliasDefinitionsXML : aliasDefinitionsXML.getAlias()) {
				String keyPrefix = anAliasDefinitionsXML.getKeyPrefix() != null ?
						anAliasDefinitionsXML.getKeyPrefix() :
						aliasDefinitionsXML.getDefaultKeyPrefix();
				String keySuffix = anAliasDefinitionsXML.getKeySuffix() != null ?
						anAliasDefinitionsXML.getKeySuffix() :
						aliasDefinitionsXML.getDefaultKeySuffix();
				if (anAliasDefinitionsXML.getValue() != null && anAliasDefinitionsXML.getListValueCount() > 0) {
					throw new RuntimeException("Both alias's value and listValue specified, choose one (alias:" +
							anAliasDefinitionsXML.getKey() + ", file: " + configFile.getAbsolutePath() + ")");
				}
				aliasDefinitions.add(anAliasDefinitionsXML.getKey(),
						keyPrefix, keySuffix,
						anAliasDefinitionsXML.getValue(), anAliasDefinitionsXML.getListValue());
			}
		}
		return aliasDefinitions;
	}

	public D3dFlowModelConfig getD3dFlowModelConfig() {
        return d3dFlowModelConfig;
    }
}
