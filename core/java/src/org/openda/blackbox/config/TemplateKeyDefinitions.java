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
import java.util.HashMap;

/**
 * Collection of key definitions for the black box template config.
 */
public class TemplateKeyDefinitions {
    HashMap<String, TemplateKeyDefinition> keyDefinitions;

    public HashMap<String, TemplateKeyDefinition> getKeyDefinitions() {
        return keyDefinitions;
    }

    public TemplateKeyDefinitions() {
        this.keyDefinitions = new HashMap<String, TemplateKeyDefinition>();
    }

    public void add(String id, String name, String keyPrefix, String keySuffix, String targetType) {
        keyDefinitions.put(id, new TemplateKeyDefinition(id, name, keyPrefix, keySuffix, targetType));
    }

    public TemplateKeyDefinition getKeyDefinitionById(String keyId) {
        return keyDefinitions.get(keyId);
    }
}
