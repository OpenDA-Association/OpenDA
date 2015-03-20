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
import org.openda.utils.io.FileSupport;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Collection;
import java.util.Properties;

/**
 * Object for storing Template file definitions from the Black box Template file config
 */
public class BBTemplateFile {

    private String templateFileName;
    private String targetFileName;
    private String valuesFileName;
    private Collection<TemplateKeyDefinition> keys;

    public BBTemplateFile(String templateFileName, String targetFileName, Collection<TemplateKeyDefinition> keys, String valuesFileName) {
        this.templateFileName = templateFileName;
        this.targetFileName = targetFileName;
        this.valuesFileName = valuesFileName;
        this.keys = keys;

    }

    public String getTargetFileName() {
        return targetFileName;
    }

    public String getTemplateFileName() {
        return templateFileName;
    }

    public Collection<TemplateKeyDefinition> getKeys() {
        return keys;
    }

    public void generateTargetFile(File fileOrDir) {
        File workingDir = fileOrDir.isDirectory() ? fileOrDir : fileOrDir.getParentFile();

        File templateFile = new File(workingDir,templateFileName);
        File targetFile = new File(workingDir, targetFileName);
        File valuesFile = new File(workingDir, valuesFileName);

        String[] contents = FileSupport.readContentOfFile(templateFile);

        Properties valueProperties = new Properties();
        try {
            valueProperties.load(new FileInputStream(valuesFile));
        } catch (IOException e) {
            throw new RuntimeException("Problem reading key value pairs from valuesFile " + valuesFile.getAbsolutePath());
        }
        for (String keyId: valueProperties.stringPropertyNames()) {
            String value = valueProperties.getProperty(keyId);
            TemplateKeyDefinition keyDefinition = getKeyDefinitionByKeyId(keyId);
            if (keyDefinition == null) continue;
            writeValueToContent(contents, keyDefinition, value);
        }

        FileSupport.writeContentOfFile(targetFile,contents);
    }

    private TemplateKeyDefinition getKeyDefinitionByKeyId(String keyId) {
        for (TemplateKeyDefinition keyDefinition: keys) {
            if (keyDefinition.getId().equals(keyId)) return keyDefinition;
        }
        return null;
    }

    private void writeValueToContent(String[] sContent, TemplateKeyDefinition keyDefinition, String value) {
        String keyString = keyDefinition.getKeyString();
        keyDefinition.setValue(value);
        if ("".equals(keyString)) return;

        for (int iLine=0; iLine < sContent.length; iLine++){
            if (sContent[iLine].contains(keyString)) {
                sContent[iLine] = keyDefinition.apply(sContent[iLine]);
            }
        }
    }

}
