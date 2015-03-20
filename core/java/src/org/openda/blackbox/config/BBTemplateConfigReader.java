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
import org.openda.core.io.castorgenerated.BlackBoxTemplateConfigXML;
import org.openda.core.io.castorgenerated.TemplateFileXML;
import org.openda.core.io.castorgenerated.TemplateKeyDefinitionXML;
import org.openda.core.io.castorgenerated.TemplateKeyDefinitionsXML;
import org.openda.utils.io.CastorUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;

/**
 * Reader for black box template config
 */
public class BBTemplateConfigReader {

    BBTemplateConfig bbTemplateConfig;

    public BBTemplateConfigReader(File templateConfigFile) {
        BlackBoxTemplateConfigXML bbTemplateConfigXML = (BlackBoxTemplateConfigXML) CastorUtils.parse(templateConfigFile, BlackBoxTemplateConfigXML.class);


        TemplateKeyDefinitions keyDefinitions = parseKeyDefinitions(bbTemplateConfigXML.getKeyDefinitions());
        String valuesFileName = bbTemplateConfigXML.getValuesFile();

        Collection<BBTemplateFile> templateFiles;
        templateFiles = parseTemplateFiles(keyDefinitions, bbTemplateConfigXML.getFiles().getFile(), valuesFileName);


        bbTemplateConfig = new BBTemplateConfig(keyDefinitions, templateFiles, valuesFileName);
    }

    private Collection<BBTemplateFile> parseTemplateFiles(TemplateKeyDefinitions keyDefinitions, TemplateFileXML[] filesXML, String valuesFileName) {
        ArrayList<BBTemplateFile> files = new ArrayList<BBTemplateFile>();
        for (TemplateFileXML fileXML : filesXML) {
            BBTemplateFile bbTemplateFile = parseBBTemplateFile(fileXML, keyDefinitions, valuesFileName);
            if (bbTemplateFile != null && !files.contains(bbTemplateFile) && !checkForDuplicateTemplateFiles(files, bbTemplateFile)) {
                files.add(bbTemplateFile);
            }
        }
        return files;
    }

    private boolean checkForDuplicateTemplateFiles(ArrayList<BBTemplateFile> files, BBTemplateFile bbTemplateFile) {
        for (BBTemplateFile templateFile: files) {
            if (templateFile.getTemplateFileName().equals(bbTemplateFile.getTemplateFileName())) {
                throw new RuntimeException("Duplicate Template file name found in BBTemplateConfig File " + bbTemplateFile.getTemplateFileName());
            }
        }
        return false;
    }

    private BBTemplateFile parseBBTemplateFile(TemplateFileXML fileXML, TemplateKeyDefinitions keyDefinitions, String valuesFileName) {
        if (fileXML == null) {
            return null;
        }

        String templateFile = fileXML.getTemplateFile();
        String targetFile = fileXML.getTargetFile();

        ArrayList<TemplateKeyDefinition> keys = new ArrayList<TemplateKeyDefinition>();
        for (String keyId : fileXML.getKeys().getKeyId()) {
            TemplateKeyDefinition keyDefinition = keyDefinitions.getKeyDefinitionById(keyId);
            if (keyDefinition != null && !keys.contains(keyDefinition)) {
                keys.add(keyDefinition);
            }
        }

        return new BBTemplateFile(templateFile, targetFile, keys, valuesFileName);


    }

    private static TemplateKeyDefinitions parseKeyDefinitions(TemplateKeyDefinitionsXML keyDefinitionsXML) {
        TemplateKeyDefinitions keyDefinitions;
        TemplateKeyDefinitionXML[] keyDefinitionXMLs = keyDefinitionsXML.getKey();
        keyDefinitions = new TemplateKeyDefinitions();
        for (TemplateKeyDefinitionXML aKeyDefinitionXML : keyDefinitionXMLs) {
            String keyPrefix = aKeyDefinitionXML.getKeyPrefix() != null ?
                    aKeyDefinitionXML.getKeyPrefix() :
                    keyDefinitionsXML.getDefaultKeyPrefix();
            String keySuffix = aKeyDefinitionXML.getKeySuffix() != null ?
                    aKeyDefinitionXML.getKeySuffix() :
                    keyDefinitionsXML.getDefaultKeySuffix();
            String targetType = aKeyDefinitionXML.getTargetType(); //parseTemplateKeyType(aKeyDefinitionXML);

			keyDefinitions.add(aKeyDefinitionXML.getId(), aKeyDefinitionXML.getName(),
                    keyPrefix, keySuffix,targetType);
        }
        return keyDefinitions;
    }

/*
    private static String parseTemplateKeyType(TemplateKeyDefinitionXML aKeyDefinitionXML) {
        String targetType = String.class.getName();
        BlackBoxTemplateKeyTypesXML keyTypesXML = aKeyDefinitionXML.getTargetType();
        if (BlackBoxTemplateKeyTypesXML.DOUBLE_TYPE == keyTypesXML.getType()) {
            targetType = Double.class.getName();
        } else if (BlackBoxTemplateKeyTypesXML.INTEGER_TYPE == keyTypesXML.getType()) {
            targetType = Integer.class.getName();
        }
        return targetType;
    }
*/

    public BBTemplateConfig getBBTemplateConfig() {
        return bbTemplateConfig;
    }
}
