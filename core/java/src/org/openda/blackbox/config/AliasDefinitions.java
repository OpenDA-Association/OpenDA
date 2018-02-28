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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;

/**
 * Collection of alias definitions for the black box wrapper config / model config.
 */
public class AliasDefinitions {

    HashMap<String, AliasDefinition> aliasDefinitions;

    public AliasDefinitions() {
        this.aliasDefinitions = new HashMap<String, AliasDefinition>();
    }

    public Collection<String> getUsedAliasIds(String[] strings) {

        Collection<String> usedAliases = new ArrayList<String>();

        for (String string : strings) {
            for (AliasDefinition aliasDefinition : aliasDefinitions.values()) {
                if (aliasDefinition.isUsedIn(string)) {
					aliasDefinition.setIsUsed(true);
                    usedAliases.add(aliasDefinition.getKey());
                }
            }
        }
        return usedAliases;
    }

    public Collection<String> getUsedAliasIds(String string) {
        ArrayList<String> usedAliases = new ArrayList<String>();
        for (AliasDefinition aliasDefinition : aliasDefinitions.values()) {
            if (aliasDefinition.isUsedIn(string)) {
                aliasDefinition.setIsUsed(true);
                usedAliases.add(aliasDefinition.getKey());
            }
        }
        return usedAliases;
    }

	public String[] applyToArguments(String[] arguments, Collection<String> usedAliasIds) {
		if (usedAliasIds == null) {
			throw new IllegalArgumentException("Invalid list of used aliases while expanding argument(s) \"" +
					arguments[0] + (arguments.length > 1 ? ", ...\"" : ""));
		}

		//replace all arguments with processed arguments.
		ArrayList<String> processedArguments = new ArrayList<String>();
		for (String argument : arguments) {
			//replace used aliases in current argument.
		    boolean aliasWithListValues = false;
			for (String usedAliasId : usedAliasIds) {
				if (argument.toLowerCase().contains(usedAliasId.toLowerCase())) {
					AliasDefinition aliasDefinition = aliasDefinitions.get(usedAliasId);
					if (aliasDefinition == null) {
						throw new IllegalArgumentException("Unknown alias identifier: \"" + usedAliasId + "\"");
					}

					//TODO if aliasDefinition not set, then isList returns true always. AK
					if (aliasDefinition.isList()) {
					    //replace argument with alias list values.
					    //TODO aliasListValues only works when argument contains only aliases, otherwise any extra text in argument is lost. AK
						processedArguments.addAll(Arrays.asList(aliasDefinition.getListValues()));
						aliasWithListValues = true;
					} else {
			            //replace current used alias in current argument.
						argument = aliasDefinition.apply(argument);
					}
				}
			}

			if (!aliasWithListValues) {
			    //add processed argument.
				processedArguments.add(argument);
			}
		}

		return processedArguments.toArray(new String[processedArguments.size()]);
	}

	public String apply(String string, Collection<String> usedAliasIds) {
        String resultString = string;
        if (string != null) {
            if (usedAliasIds == null) {
                throw new IllegalArgumentException("Invalid list of used aliases while expanding string \"" + string + "\"");
            }
            for (String usedAliasId : usedAliasIds) {
                AliasDefinition aliasDefinition = aliasDefinitions.get(usedAliasId);
                if (aliasDefinition == null) {
                    throw new IllegalArgumentException("Unknown alias identifier: \"" + usedAliasId + "\"");
                }
                resultString = aliasDefinition.apply(resultString);
            }
        }
        return resultString;
    }

    public void add(String key, String keyPrefix, String keySuffix, String value, String[] listValues) {
        aliasDefinitions.put(key, new AliasDefinition(key, keyPrefix, keySuffix, value, listValues));
    }

    public void add(AliasDefinition item){
    	this.aliasDefinitions.put(item.getKey(), item);
    }

    public void setAliasValue(String key, String value) {
        AliasDefinition aliasDefinition = aliasDefinitions.get(key);
        if (aliasDefinition == null) {
            throw new IllegalArgumentException("Unknown alias: \"" + key + "\"");
        }
        aliasDefinition.setValue(value);
    }

	public void setAliasListValues(String key, String[] listValues) {
		AliasDefinition aliasDefinition = aliasDefinitions.get(key);
		if (aliasDefinition == null) {
			throw new IllegalArgumentException("Unknown alias: \"" + key + "\"");
		}
		aliasDefinition.setListValues(listValues);
	}

	public void validate() {
        String errString = null;
        for (AliasDefinition aliasDefinition : aliasDefinitions.values()) {
            if (!aliasDefinition.isValid()) {
                if (errString == null) {
                    errString = "No valid value set for alias(es):";
                }
                errString += " " + aliasDefinition.getKey();
            }
        }
        if (errString != null) {
            throw new RuntimeException(errString);
        }
    }

	public boolean containsKey(String key) {
		for (String aliasKey: aliasDefinitions.keySet()) {
			if (aliasKey.equalsIgnoreCase(key)) {
				return true;
			}
		}
		return false;
	}
	
	public String toString(){
		String result="{";
		for(String key : this.aliasDefinitions.keySet()){
			result+= this.aliasDefinitions.get(key);
		}
		result+="}\n";
		return result;
	}
	
	public AliasDefinitions clone(){
		AliasDefinitions result= new AliasDefinitions();
		for( String key : this.aliasDefinitions.keySet()){
			AliasDefinition item = this.aliasDefinitions.get(key);
			result.add(item.clone());
		}
		return result;
	}
}
