/* OpenDA v2.4 
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

/**
 * Alias definitions for the black box wrapper config / model config. The alias is defined in the model wrapper
 * config, and the actual value is set either directly in the wrapper config, or in the model config.
 * If the value of an alias is not specified, the environment is checked for an env.var. with the same name
 * as the alias's key.
 */
public class AliasDefinition {
    private String key;
    private String value;
    private String keyString;
	private boolean isUsed;
	private String[] listValues;

	public AliasDefinition(String key, String keyPrefix, String keySuffix, String value, String[] listValues) {
        this.key = key;
        this.keyString = keyPrefix + key + keySuffix;
        this.value = value;
		this.listValues = listValues != null ? listValues : new String[]{};
		if (this.value != null && this.listValues.length > 0) {
			throw new RuntimeException("Both alias's value and listValue specified for alias \"" + key + "\"");
		}
	}

	public AliasDefinition(String key, String keyString, String value, String[] listValues,boolean isUsed) {
        this.key = key;
        this.keyString = keyString;
        this.value = value;
        this.isUsed = isUsed;
		this.listValues = listValues != null ? listValues : new String[]{};
		if (this.value != null && this.listValues.length > 0) {
			throw new RuntimeException("Both alias's value and listValue specified for alias \"" + key + "\"");
		}
	}

    public String getKey() {
        return key;
    }

	public boolean isUsedIn(String string) {
        return string != null && (string.indexOf(keyString) >= 0);
    }

    public void setValue(String value) {
		this.listValues = new String[]{};
        this.value = value;
    }

	public void setListValues(String[] listValues) {
		this.value = null;
		this.listValues = listValues != null ? listValues : new String[]{};
	}

	public boolean isList() {
		return value==null;
	}

	public String[] getListValues() {
		return listValues;
	}

	public String apply(String string) {
        if (value==null) {
            throw new IllegalStateException("Value for alias \"" + key + "\" == null");
        }
        if (string != null) {
            int keyPos = string.indexOf(keyString);
            if (keyPos >= 0) {
                return string.substring(0, keyPos) + value + string.substring(keyPos + keyString.length());
            }
        }
        return string;
    }

    public void setIsUsed(boolean isUsed) {
        this.isUsed = isUsed;
    }

    public boolean isValid() {
        if (isUsed && value == null && listValues.length==0) {
            value = System.getenv(key);
            if (value == null) {
                return false;
            }
        }
        return true;
    }
    
    public String toString(){
    	String result="{ key:"+this.key;
    	if(!(this.value==null)){
    		result+=" value:"+this.value;
    	}else if(!(this.listValues==null)){
    		result+="{";
    		for( String val : this.listValues){
    			result+=" "+val;
    		}
    		result+="}";
    	}
    	result+="\n";
    	return result;
    }
    
    public AliasDefinition clone(){
    	AliasDefinition result= new AliasDefinition(this.key, this.keyString, value, this.listValues, this.isUsed);
    	return result;
    }
}
