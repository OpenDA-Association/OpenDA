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
import org.openda.blackbox.interfaces.IKeyType;
import org.openda.utils.ObjectSupport;

/**
 * Key definitions for the black box template config. The key is defined in the template config,
 * and the actual value is set in the defined template file by using the key=value pair from the defined valuesFile.
 */
public class TemplateKeyDefinition {

    private String id;
    private String value;
    private String name;
    private String keyString;
    private IKeyType keyType;

    public TemplateKeyDefinition(String id, String name, String keyPrefix, String keySuffix, String targetType) {
        this.id = id;
        this.name = name;
        this.keyString = keyPrefix + name + keySuffix;
        this.keyType = (IKeyType) ObjectSupport.createNewInstance(targetType);
    }

    public void setValue(String value) {
        this.value = value;
    }

    public String getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public String getKeyString() {
        return keyString;
    }

    public String apply(String string) {
        if (value==null) {
            throw new IllegalStateException("Value for keyDefinition \"" + id + "\" == null");
        }
        if (string != null) {
            int keyPos = string.indexOf(keyString);
            if (keyPos >= 0) {
                try {
                    return string.substring(0, keyPos) + keyType.getValueAsString(value) + string.substring(keyPos + keyString.length());
                } catch (NumberFormatException ne) {
                    throw new RuntimeException("Could not apply key for value = '" + value + "' with defined targetType '" +
							keyType.toString() + "' for keyDefinition with keyId = " + id);
                }
            }
        }
        return string;

    }


    public IKeyType getKeyType() {
        return keyType;
    }
}
