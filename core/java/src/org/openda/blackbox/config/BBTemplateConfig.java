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
import java.util.Collection;

/**
 * Object for storing Black box Template config
 *
 */
public class BBTemplateConfig {
    private TemplateKeyDefinitions keyDefinitions;
    private Collection<BBTemplateFile> templateFiles;
    private String valuesFileName;


    public BBTemplateConfig(TemplateKeyDefinitions keyDefinitions, Collection<BBTemplateFile> templateFiles, String valuesFileName) {
        this.keyDefinitions = keyDefinitions;
        this.templateFiles = templateFiles;
        this.valuesFileName = valuesFileName;
    }

    public TemplateKeyDefinitions getKeyDefinitions() {
        return keyDefinitions;
    }

    public String getValuesFileName() {
        return valuesFileName;
    }

    public Collection<BBTemplateFile> getTemplateFiles() {
        return templateFiles;
    }
}
